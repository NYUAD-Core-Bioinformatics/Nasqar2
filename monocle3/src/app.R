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
source("nasqar-project.R")
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
                        tags$head(
                            # same icon as the NASQAR2 landing page
                            tags$link(rel = "icon", type = "image/png", href = "CGSB-Logo.png"),
                            tags$link(rel = "shortcut icon", type = "image/png", href = "CGSB-Logo.png"),
                            tags$style(
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
                            "Use a file in the private space" = "hpc"
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
                          div(
                              style = "display:flex; gap:8px; align-items:flex-end;",
                              div(style = "flex:1;",
                              textInput(
                                "hpc_input_path",
                                "Seurat .rds or .RData file in the private space",
                                value = ""
                              )
                              ),
                              div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_hpc_input", "Browse...", "Choose a Seurat .rds or .RData file in the private space", multiple = FALSE))
                          ),
                          actionButton("load_hpc", "Load from Data Root")
                        ),
                        nasqar_project_ui("monocle3-project",
                          help = "Trajectory outputs are kept under this name in the private space, so the project survives the session and can be restored later."),
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

  data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
  data_root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL

  # Server-side browser jailed to the private data root; it only fills the path box.
  if (!is.null(data_root)) {
      browse_roots <- nasqar_browse_roots()
      shinyFiles::shinyFileChoose(input, "browse_hpc_input", roots = browse_roots, session = session, filetypes = c("rds", "rdata"))
      observeEvent(input$browse_hpc_input, {
          selected <- tryCatch(shinyFiles::parseFilePaths(browse_roots, input$browse_hpc_input), error = function(e) NULL)
          if (!is.null(selected) && nrow(selected)) {
              updateTextInput(session, "hpc_input_path", value = as.character(selected$datapath[1]))
          }
      }, ignoreInit = TRUE)
  } else {
      removeUI(selector = "#browse_hpc_input")
      shinyjs::runjs("$('input[name=data_source][value=hpc]').closest('.radio').remove()")
  }


  # Private-space projects: projects/Monocle3/<name> (shared convention)
  source("nasqar-project.R", local = TRUE)
  project_app <- "Monocle3"
  project_legacy_env <- "NASQAR_MONOCLE3_PROJECT_DIR"
  private_space <- nasqar_project_available(project_app, project_legacy_env)
  project_directory <- function(name) {
    path <- nasqar_project_dir(project_app, name, project_legacy_env, fallback = "monocle3-project")
    if (!dir.exists(path) || file.access(path, 2) != 0) {
      stop("The Monocle3 private space directory is not writable.")
    }
    path
  }
  current_project_name <- function() {
    name <- input$nasqar_project_name
    if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
  }
  monocle_results <- reactiveVal(NULL)
  restore_pending <- reactiveVal(NULL)

  seurat_file_expected <- c(
    "An .rds file containing a Seurat object (saveRDS(seurat_obj, ...)), e.g. as exported by SeuratV5Shiny",
    "Or an .RData file holding exactly one Seurat object (save(seurat_obj, file = ...))"
  )
  # name: how to call the file in messages (the upload name, not the temporary path).
  read_seurat_file <- function(path, name = basename(path)) {
    received <- sprintf("%s (%s)", name, format(structure(file.size(path), class = "object_size"), units = "auto"))
    problem <- function(message) {
      nasqar_input_problem(message, seurat_file_expected, path = NULL, label = "Seurat object file")
    }
    if (grepl("\\.rdata$", path, ignore.case = TRUE)) {
      load_env <- new.env(parent = emptyenv())
      object_names <- tryCatch(load(path, envir = load_env), error = function(e) {
        problem(sprintf("%s could not be read as an .RData file: %s", received, conditionMessage(e)))
      })
      candidates <- object_names[
        vapply(object_names, function(name) inherits(load_env[[name]], "Seurat"), logical(1))
      ]
      if (length(candidates) != 1L) {
        problem(sprintf("%s holds %d Seurat object(s) among %s; exactly one is needed.", received,
                        length(candidates), paste(shQuote(object_names), collapse = ", ")))
      }
      load_env[[candidates[[1L]]]]
    } else {
      object <- tryCatch(readRDS(path), error = function(e) {
        problem(sprintf("%s could not be read as an .rds file: %s", received, conditionMessage(e)))
      })
      if (!inherits(object, "Seurat")) {
        problem(sprintf("%s contains an object of class %s, not a Seurat object.", received,
                        paste(class(object), collapse = "/")))
      }
      object
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
    
    data <- read_seurat_file(rdata$datapath, rdata$name)
    
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
    validate(need(nzchar(trimws(input$hpc_input_path)), "Enter a Seurat object path in the private space."))
    path <- tryCatch(
      nasqar_private_path(input$hpc_input_path, what = "Seurat .rds or .RData file"),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )
    configure_seurat_input(read_seurat_file(path))
    showNotification("Seurat object loaded directly from the private space.", type = "message")
    # Project state is recorded only once the load has succeeded (shared convention).
    if (private_space) {
      dir <- project_directory(current_project_name())
      nasqar_project_state_write(
        dir, project_app, basename(dir), "loaded",
        input_label = basename(input$hpc_input_path), source_type = "hpc",
        hpc_input_path = input$hpc_input_path
      )
      project_refresh(basename(dir))
    }
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

  # Render results (also used when a project is restored, without recomputing).
  show_monocle_results <- function(out, output_dir) {
    monocle_results(out)
    output$hpc_output_path <- renderText({
      if (private_space) paste("Saved persistently in the private space:", output_dir)
    })
    output$dim <- renderPlot({out$dim})
    output$p2 <- renderPlot({out$p2})
    output$p5 <- renderPlot({out$p5})
    shinyjs::enable("plot")
    shinyjs::enable("download")
  }

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

    output_dir <- project_directory(current_project_name())
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
    # the whole result set, so a restored project renders without rerunning
    saveRDS(out, file.path(output_dir, "monocle3-run.rds"))
    show_monocle_results(out, output_dir)
    if (private_space) {
      nasqar_project_state_write(
        output_dir, project_app, basename(output_dir), "monocle",
        assay = input$assay, ident = input$ident, gene = input$gene,
        run_file = file.path(output_dir, "monocle3-run.rds")
      )
      project_refresh(basename(output_dir))
    }
  })

  savePlot <- function(){
    out <- monocle_results()
    pnames <- list("Dimensions plot"=out$dim, "Monocle3 results"=NA, "Heatmap"=out$p5)
    pnames[input$plot][[1]]
  }

  #Create action for download button
  output$download <- nasqar_download_handler(
    filename = function() {
      fnames <- c("Dimensions plot"="clustering_dimensions.pdf", "Monocle3 results"=paste("monocle3_pseudotime_", input$gene, ".pdf", sep=""), "Heatmap"="monocle3_heatmap.pdf")
      fnames[input$plot][[1]]
    },
    content = function(file) {
      out <- monocle_results()
      req(out)
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
  output$download2 <- nasqar_download_handler(
    filename = function(){"monocle3_pseudotime_results.csv"},
    content = function(file){
      out <- monocle_results()
      req(out)
      write.csv(out$t1, file, row.names = FALSE)
    })

  # ---- private-space project state and restore (shared convention) ----
  observeEvent(seurat_data(), {
    pending <- restore_pending()
    if (is.null(pending)) return()
    restore_pending(NULL)
    if (!is.null(pending$assay)) updateSelectInput(session, "assay", selected = pending$assay)
    if (!is.null(pending$ident)) updateSelectInput(session, "ident", selected = pending$ident)
    if (!is.null(pending$gene)) updateTextInput(session, "gene", value = pending$gene)
    if (identical(pending$stage, "monocle") && file.exists(pending$run_file %||% "")) {
      out <- tryCatch(readRDS(pending$run_file), error = function(e) NULL)
      if (!is.null(out)) {
        show_monocle_results(out, pending$dir)
        showNotification("Monocle 3 results restored from the project.", type = "message")
      }
    }
  }, ignoreInit = TRUE)

  restore_project <- function(state, dir) {
    if (!identical(state$app, project_app)) return("this is not a Monocle 3 project")
    if (!identical(state$source_type, "hpc")) return("this project was made from an uploaded object; upload it again to rerun it")
    if (is.null(state$hpc_input_path) || !file.exists(state$hpc_input_path)) return("the project's Seurat object is no longer available")
    updateRadioButtons(session, "data_source", selected = "hpc")
    updateTextInput(session, "hpc_input_path", value = state$hpc_input_path)
    restore_pending(list(stage = state$stage, assay = state$assay, ident = state$ident, gene = state$gene,
                         run_file = state$run_file, dir = dir))
    shinyjs::delay(800, shinyjs::click("load_hpc"))
    NULL
  }

  project_refresh <- nasqar_project_server(
    input, output, session, project_app, project_legacy_env, on_restore = restore_project
  )
})

# Run the application 
shinyApp(ui = ui, server = server)
