#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny)
library(Seurat)
library(dplyr)
library(ggplot2)
library(patchwork)
library(clustree)
library(monocle3)
library(stringr)
library(SeuratWrappers)
library(mclust)
library(pheatmap)

# Change data load maximum
options(shiny.maxRequestSize = 115*1024^3)

#Source functions
source("./source.R")

# Define UI for application
ui <- shinyUI(fluidPage(
                        # Set style for vertical alignment
                        tags$head(tags$style(
                          HTML('
                               #vert { 
                               display: flex;
                               align-items: center;
                               margin-top: 50px;
                               }
                               .tooltip .tooltip-inner {
                               max-width: 100%;
                               }
                               '))),
                        shinyjs::useShinyjs(),
                        
                        # Application title
                        titlePanel(textOutput("title")),
                        fluidRow(tags$hr(style="border-color: black;")),
                        
                        # Create Input
                        fluidRow(column(2, wellPanel(
                        radioButtons(
                          "data_source",
                          "Seurat object source",
                          c(
                            "Upload from browser" = "upload",
                            "Use HPC scratch file" = "hpc"
                          ),
                          selected = "upload"
                        ),
                        conditionalPanel(
                          "input.data_source == 'upload'",
                          fileInput("file", 'Choose Rdata/Rds to upload',
                                    accept = c('.Rdata', ".rds", ".Rds")),
                          actionButton("upload", "Upload")
                        ),
                        conditionalPanel(
                          "input.data_source == 'hpc'",
                          textInput(
                            "hpc_input_path",
                            "Seurat .rds or .RData file inside HPC scratch",
                            value = ""
                          ),
                          actionButton("load_hpc", "Load HPC Object")
                        ),
                        textInput("hpc_project_name", "Persistent project name", value = "monocle3-project"),
                        #Set load button
                        textOutput("hpc_output_path"),
                        
                        fluidRow(tags$hr(style="border-color: black;")),
                        
                        # Set assay type
                        selectInput("assay",
                                    "Select assay",
                                    NULL),
                        
                        #Set data identity
                        selectInput("ident",
                                    "Select Identity for clustering",
                                    NULL),
                        
                        #Set progenitor gene
                        textInput("gene", "Progenitor gene:", value = ""),
                        
                        #Set load button
                        actionButton("run", "Run Monocle3"),
                        
                        fluidRow(tags$hr(style="border-color: black;")),
                        
                        #Select plot to download
                        selectInput("plot",
                                    "Select plot to download",
                                    c("Dimensions plot", "Monocle3 results", "Heatmap")),
                        #Download plot
                        downloadButton("download", "Download Plot"),
                        fluidRow(tags$hr(style="border-color: black;")),
                        
                        #Download dta
                        downloadButton("download2", "Download Results"))),
                        
                        #Plot dim and pseudotime
                        column(5, align="center", id="vert", plotOutput("dim", width="90%", height="800px")), 
                        column(5, id="vert", align="center", plotOutput("p2", width="90%", height="800px"))),
                        
                        # Heatmap
                        fluidRow(tags$hr(style="border-color: black;")),
                        fluidRow(column(10, align="center", offset=2, id="vert", h3("Heatmap of genes significantly associated with pseudotime"))),
                        fluidRow(column(10, align="center", offset=2, id="vert", plotOutput("p5", width="80%", height="700px"))),
                        fluidRow(tags$hr(style="border-color: black;"))
                        ))

# Define server logic
server <- shinyServer(function(input, output, session) {
  seurat_data <- reactiveVal(NULL)
  monocle_workers <- suppressWarnings(
    as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  )
  if (is.na(monocle_workers) || monocle_workers < 1L) monocle_workers <- 1L

  resolve_hpc_path <- function(path) {
    root <- normalizePath(
      Sys.getenv("NASQAR_DATA_ROOT", unset = "/scratch/nr83"),
      mustWork = TRUE
    )
    resolved <- normalizePath(path, mustWork = TRUE)
    allowed <- identical(resolved, root) ||
      startsWith(resolved, paste0(root, .Platform$file.sep))
    if (!allowed) stop("The file must be inside the configured HPC data root.")
    if (dir.exists(resolved)) stop("Select a Seurat .rds or .RData file, not a directory.")
    resolved
  }

  project_directory <- function(name) {
    root <- Sys.getenv("NASQAR_MONOCLE3_PROJECT_DIR", unset = "")
    if (!nzchar(root)) return(tempdir())
    safe_name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(name))
    if (!nzchar(safe_name)) safe_name <- "monocle3-project"
    path <- file.path(root, safe_name)
    dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!dir.exists(path) || file.access(path, 2) != 0) {
      stop("The Monocle3 HPC project directory is not writable.")
    }
    path
  }

  read_seurat_file <- function(path) {
    if (grepl("\\.rdata$", path, ignore.case = TRUE)) {
      load_env <- new.env(parent = emptyenv())
      object_names <- load(path, envir = load_env)
      candidates <- object_names[
        vapply(object_names, function(name) inherits(load_env[[name]], "Seurat"), logical(1))
      ]
      validate(need(length(candidates) == 1L, "The .RData file must contain exactly one Seurat object."))
      load_env[[candidates[[1L]]]]
    } else {
      readRDS(path)
    }
  }

  configure_seurat_input <- function(data) {
    validate(need(inherits(data, "Seurat"), "The transferred object is not a Seurat object."))
    seurat_data(data)
    updateSelectInput(session, "assay", "Select assay", choices = names(data@assays))
    updateSelectInput(
      session, "ident", "Select Identity for clustering",
      choices = colnames(data@meta.data)
    )
    shinyjs::enable("gene")
    shinyjs::enable("assay")
    shinyjs::enable("ident")
    shinyjs::enable("run")
  }

  read_exchange_object <- function(token) {
    validate(need(
      length(token) == 1L &&
        grepl("^[0-9a-fA-F-]{36}$", token),
      "Invalid NASQAR2 transfer token."
    ))
    exchange_dir <- file.path(tempdir(check = TRUE), "..", "nasqar_exchange")
    exchange_dir <- normalizePath(exchange_dir, mustWork = FALSE)
    path <- file.path(exchange_dir, paste0(token, ".rds"))
    validate(need(file.exists(path), "The transferred Seurat object has expired."))
    validate(need(
      as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs")) <= 3600,
      "The transferred Seurat object has expired."
    ))
    object <- readRDS(path)
    unlink(path)
    object
  }
  
  #Set text outputs
  output$title <- renderText("Pseudotime projection with Monocle3")
  
  # Load data
  load_Rdata <- function(){
    if(is.null(input$file)){return(NULL)} 
    rdata <- isolate({input$file})
    short <- input$file
    
    data <- read_seurat_file(rdata$datapath)
    
    return(data)
  }
  
  #Write function to find Seurat object
  is.Seurat <- function(x){
    any(class(x) == "Seurat")
  }
  
  #Disable buttons until Rdata is loaded
  shinyjs::disable("assay")
  shinyjs::disable("ident")
  shinyjs::disable("gene")
  shinyjs::disable("run")
  shinyjs::disable("plot")
  shinyjs::disable("download")
  
  observeEvent(input$upload,{
    data <- load_Rdata()
    configure_seurat_input(data)
  })

  observeEvent(input$load_hpc, {
    validate(need(nzchar(trimws(input$hpc_input_path)), "Enter an HPC Seurat object path."))
    path <- tryCatch(
      resolve_hpc_path(input$hpc_input_path),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )
    configure_seurat_input(read_seurat_file(path))
    showNotification("Seurat object loaded directly from HPC scratch.", type = "message")
  })

  observeEvent(TRUE, {
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["exchange"]])) {
      configure_seurat_input(read_exchange_object(query[["exchange"]]))
      showNotification(
        "Seurat object transferred from SeuratV5Shiny.",
        type = "message"
      )
    }
  }, once = TRUE)

  # Create event when report load button is activated
  observeEvent(input$run,{
    data <- seurat_data()
    req(data)
    
    #Set assay
    DefaultAssay(data) <- input$assay
    
    #Create progress bar
    withProgress(message = 'Running Monocle3...please wait', {
    out <- mon.run(data=data, gene=input$gene, id=input$ident, cores=monocle_workers)
    })

    output_dir <- project_directory(input$hpc_project_name)
    saveRDS(out$seurat, file.path(output_dir, "seurat-with-monocle3-pseudotime.rds"))
    saveRDS(out$cds, file.path(output_dir, "monocle3-cell-data-set.rds"))
    write.csv(out$t1, file.path(output_dir, "monocle3-pseudotime-results.csv"), row.names = FALSE)
    pdf(file.path(output_dir, "monocle3-plots.pdf"), width = 10, height = 8)
    print(out$dim)
    print(out$p1)
    print(out$p2)
    print(out$p3)
    print(out$p4)
    print(out$p5)
    dev.off()
    output$hpc_output_path <- renderText({
      if (nzchar(Sys.getenv("NASQAR_MONOCLE3_PROJECT_DIR", ""))) {
        paste("Saved persistently on HPC:", output_dir)
      }
    })
    
    output$dim <- renderPlot({out$dim})
    
    output$p2 <- renderPlot({out$p2})
    
    output$p5 <- renderPlot({out$p5})
    
    #Enable buttons
    shinyjs::enable("plot")
    shinyjs::enable("download")

    #Create multiplot file needed for saving

      savePlot <- function(){
        pnames <- list("Dimensions plot"=out$dim, "Monocle3 results"=NA, "Heatmap"=out$p5)
        pnames[input$plot][[1]]
      }
      
      #Create action for download button
      output$download <- downloadHandler(
        filename = function() {
          fnames <- c("Dimensions plot"="clustering_dimensions.pdf", "Monocle3 results"=paste("monocle3_pseudotime_", input$gene, ".pdf", sep=""), "Heatmap"="monocle3_heatmap.pdf")
          fnames[input$plot][[1]]
        },
        content = function(file) {
          
          if (input$plot == "Dimensions plot") {
            pdf(file)
            print(out$dim)
            dev.off()  
          } else if (input$plot == "Monocle3 results"){
            pdf(file)
            print(out$p1)
            print(out$p2)
            print(out$p3)
            print(out$p4)
            dev.off()
          } else if (input$plot == "Heatmap"){
            pdf(file, width=15, height=9)
            print(out$p5)
            dev.off()
          }
        })
      
      #Create action for download of data
      output$download2 <- downloadHandler(
        filename = function(){"monocle3_pseudotime_results.csv"},
        content = function(file){
          write.csv(out$t1, file, row.names = FALSE)
          })
  
      })
})

# Run the application 
shinyApp(ui = ui, server = server)
