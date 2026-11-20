library(shiny)
source("nasqar-project.R")
library(parallel)


# User Interface stuff
ui <- tagList(
    shinyjs::useShinyjs(),
    tags$head(
        # same icon as the NASQAR2 landing page
        tags$link(rel = "icon", type = "image/png", href = "CGSB-Logo.png"),
        tags$link(rel = "shortcut icon", type = "image/png", href = "CGSB-Logo.png"),
        tags$style(HTML(" .shiny-output-error-validation {color: darkred; } ")),
        tags$style(".mybuttonclass{background-color:#CD0000;} .mybuttonclass{color: #fff;} .mybuttonclass{border-color: #9E0000;}")
    ),
    navbarPage(
        theme = "bootstrap.spacelab.min.css",  # Bootswatch Spacelab 3.4.1 (MIT), vendored in www/
        inverse = TRUE,
        title = "Merge FPKM Files",
        tabPanel(
            "Home",
            icon = icon("home"),
            fluidRow(
                column(4, wellPanel(
                    h4(strong("1. Input data")),
                    # One question decides what the panel shows. The two
                    # private-space choices are removed by the server on a
                    # public instance.
                    radioButtons("counts_source", "Where is your data?",
                        c(
                            "Upload files" = "upload",
                            "Files in the private space" = "private_files",
                            "Reopen a saved project" = "savedProject"
                        ),
                        selected = "upload"),
                    conditionalPanel(
                        condition = "input.counts_source=='savedProject'",
                        div(class = "nasqar-project-private",
                            nasqar_project_restore_ui(button_label = "Open project", stacked = TRUE),
                            helpText(style = "margin-top:10px;", "The merged table comes back as you left it.")
                        )
                    ),
                    conditionalPanel(
                        condition = "input.counts_source=='private_files'",
                        div(
                            style = "display:flex; gap:8px; align-items:flex-end;",
                            div(style = "flex:1;",
                                textAreaInput("private_fpkm_files", "FPKM files in the private space (one per line)", value = "", rows = 4)
                            ),
                            div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_private_fpkm_files", "Browse...", "Choose the FPKM files in the private space", multiple = TRUE))
                        ),
                        helpText("Pick the files to merge with Browse (several at once) or paste their paths; files are read in place.")
                    ),
                    conditionalPanel(
                        condition = "input.counts_source=='upload'",
                        fileInput("datafile", "FPKM files, one per sample (at least two)",
                            accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                            ), multiple = TRUE
                        )
                    ),
                    conditionalPanel(
                    condition = "input.counts_source!='savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_name_ui(
                            "fpkm-project",
                            label = "Project name",
                            help = "The merged table is saved automatically in the private space under this name."
                        )
                    ),
                    hr(),
                    h4(strong("2. Options and merge")),
                    # checkboxInput("addOne", "Add +1 to counts (pseudo counts)", FALSE),
                    checkboxInput("fpkmToTpm", "Convert FPKM to TPM", FALSE),
                    checkboxInput("addGeneNames", "Look up gene names from Ensembl IDs", FALSE),
                    conditionalPanel(
                        "input.addGeneNames",
                        radioButtons("refGenome", "",
                            c(
                                "Homo_sapiens.GRCh38.81",
                                "Homo_sapiens.GRCh38.84",
                                "Mus_musculus.GRCm38.82",
                                "Danio_rerio.GRCz10.84",
                                "Drosophila_melanogaster.BDGP6.81"
                            ),
                            selected = "Homo_sapiens.GRCh38.81"
                        ),
                        radioButtons("geneNameColumn", "",
                            c(
                                "Add gene.names column after gene ids" = "add",
                                "Replace gene ids column by gene names" = "replace"
                            ),
                            selected = "add"
                        )
                    ),
                    conditionalPanel(
                        "output.filesUploaded",
                        actionButton("upload_data", "Merge files", icon = icon("play"),
                            class = "btn-primary", style = "color: #fff;"
                        )
                    )
                    ),
                    conditionalPanel(
                        "output.filesMerged",
                        downloadLink("downloadData", "Download merged table (.csv)", class = "btn btn-success", style = "color: #fff; margin-top: 10px;"),
                        uiOutput("projectSavedNote")
                    )
                )), # column
                column(
                    8,
                    conditionalPanel(
                        "output.filesMerged",
                        h2("Merged table"),
                        hr(),
                        dataTableOutput("contents")
                    ),
                    conditionalPanel(
                        "!output.filesMerged",
                        includeMarkdown("user_guide.md")
                    )
                ) # column
            ) # fluidrow
        ), # tabpanel
        # private instance only; removed by the server otherwise
        tabPanel(
            "Project files",
            icon = icon("folder-open"),
            div(class = "nasqar-project-private", style = "padding: 0 15px;",
                h3("Saved files of this project"),
                nasqar_project_files_ui()
            )
        ),



        ## ==================================================================================== ##
        ## FOOTER
        ## ==================================================================================== ##
        footer = p(
            hr(), p("mergeFPKMs developed by ", strong("Bioinformatics Core"), " ", align = "center", width = 4),
            p(("Center for Genomics and Systems Biology, NYU Abu Dhabi"), align = "center", width = 4),
            p(strong("Acknowledgements:"), " Nathalie Neriec", align = "center", width = 4),
            p(("Copyright (C) 2018, code licensed under GPLv3"), align = "center", width = 4)
        )
    ) # end navbarpage
) # end taglist



# Max upload size
options(shiny.maxRequestSize = 600 * 1024^2)
# Define server
server <- function(input, output, session) {
    output$contents <- renderDataTable({
        tmp <- dataReactive()
        if (!is.null(tmp)) tmp$data
    })

    output$filesMerged <- reactive({
        return(!is.null(dataReactive()))
    })
    outputOptions(output, "filesMerged", suspendWhenHidden = FALSE)

    output$downloadData <- nasqar_download_handler(
        filename = function() {
            paste("merged-", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            write.csv(dataReactive()$data, con, row.names = FALSE)
        }
    )

    inputDataReactive <- reactive({
        inFile <- if (identical(input$counts_source, "private_files")) data_root_files() else input$datafile
        if (is.null(inFile)) {
            return(NULL)
        }
        return(inFile)
    })


    # Private-space projects: projects/<name>/mergeFPKMs (shared convention)
    source("nasqar-project.R", local = TRUE)
    project_app <- "mergeFPKMs"
    private_space <- nasqar_project_available(project_app)
    # Reopening a project and its files page exist in a private instance only.
    if (!private_space) {
        shinyjs::runjs("$('input[name=counts_source][value=savedProject]').closest('.radio').remove()")
        shinyjs::runjs("$('.navbar-nav a[data-value=\\'Project files\\']').parent().remove()")
        # only "Upload files" is left: a one-choice question is not worth asking
        shinyjs::hide("counts_source")
    }
    current_project_name <- function() {
        name <- input$nasqar_project_name
        if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
    }
    restored_data <- reactiveVal(NULL)

    # Private data root: input files read in place (private instance only)
    data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
    data_root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL
    if (!is.null(data_root)) {
        browse_roots <- nasqar_browse_roots()
        shinyFiles::shinyFileChoose(input, "browse_private_fpkm_files", roots = browse_roots, session = session)
        observeEvent(input$browse_private_fpkm_files, {
            selected <- tryCatch(shinyFiles::parseFilePaths(browse_roots, input$browse_private_fpkm_files), error = function(e) NULL)
            if (!is.null(selected) && nrow(selected)) {
                updateTextAreaInput(session, "private_fpkm_files", value = paste(as.character(selected$datapath), collapse = "\n"))
            }
        }, ignoreInit = TRUE)
    } else {
        removeUI(selector = "#browse_private_fpkm_files")
        shinyjs::runjs("$('input[name=counts_source][value=private_files]').closest('.radio').remove()")
    }
    # Selected data-root files, in the same shape as a multi-file upload.
    data_root_files <- function() {
        lines <- trimws(unlist(strsplit(input$private_fpkm_files %||% "", "\n", fixed = TRUE)))
        lines <- lines[nzchar(lines)]
        shiny::validate(need(length(lines) > 0, "Select at least one file in the private space."))
        files <- vapply(lines, function(path) {
            tryCatch(nasqar_private_path(path, what = "FPKM file"), error = function(e) shiny::validate(need(FALSE, conditionMessage(e))))
        }, character(1), USE.NAMES = FALSE)
        data.frame(name = basename(files), size = file.info(files)$size, type = "", datapath = files, stringsAsFactors = FALSE)
    }
    dataReactive <- reactive({
        restored <- restored_data()
        if (!is.null(restored)) restored else mergeReactive()
    })
    observeEvent(mergeReactive(), {
        merged <- mergeReactive()
        if (!private_space || is.null(merged)) return()
        restored_data(NULL)
        dir <- nasqar_project_dir(project_app, current_project_name(), fallback = "fpkm-project")
        merged_file <- file.path(dir, "merged-fpkm.csv")
        utils::write.csv(merged$data, merged_file, row.names = FALSE)
        nasqar_project_state_write(dir, project_app, nasqar_project_name(dir), "merged",
                                   input_label = sprintf("%d files", nrow(inputDataReactive())), merged_file = merged_file)
        # also under downloads/, so it shows on the Project files page
        nasqar_project_save_output(merged_file, "merged-fpkm.csv", session)
        project_saved(list(name = nasqar_project_name(dir)))
        project_refresh(nasqar_project_name(dir))
    })
    project_saved <- reactiveVal(NULL)
    output$projectSavedNote <- renderUI({
        saved <- project_saved()
        if (!private_space || is.null(saved)) return(NULL)
        div(class = "alert alert-success", style = "margin-top:12px;",
            icon("check-circle"), " Saved in the private space as project ", strong(saved$name),
            ". The file is ",
            tags$code(file.path("projects", saved$name, project_app, "downloads", "merged-fpkm.csv")),
            "; see it on the ", strong("Project files"), " page.")
    })
    project_refresh <- nasqar_project_server(input, output, session, project_app, on_restore = function(state, dir) {
        if (!identical(state$app, project_app)) return("this project belongs to another application")
        if (is.null(state$merged_file) || !file.exists(state$merged_file)) return("the project's merged table is missing")
        restored_data(list(data = utils::read.csv(state$merged_file, check.names = FALSE, stringsAsFactors = FALSE)))
        NULL
    })

    mergeReactive <-
        eventReactive(input$upload_data,
            ignoreNULL = FALSE,
            {
                inFile <- inputDataReactive()
                if (is.null(inFile)) {
                    return(NULL)
                }

                progress <- Progress$new(session, min = 0, max = 1)
                on.exit(progress$close())

                progress$set(message = "Merging files ...")

                files <- list()

                # What a per-sample FPKM file must look like, for the input-problem modal
                expected <- c(
                    "A plain-text table, tab- or comma-separated, one file per sample (e.g. a cufflinks genes.fpkm_tracking file); the file name becomes the sample name.",
                    "A header row naming the columns; the columns 'gene_id' and 'FPKM' must be present (any other column is ignored).",
                    "'gene_id': one gene identifier per row; 'FPKM': the numeric FPKM value of that gene in this sample.",
                    "Every file lists the same set of gene_ids, so the FPKM columns can be placed side by side.",
                    "Files picked in the private space are read in place and must have the same layout as uploaded files."
                )
                check_fpkm_file <- function(df) {
                    missing <- setdiff(c("gene_id", "FPKM"), colnames(df))
                    if (length(missing)) {
                        return(sprintf("Column(s) %s not found in the header; the columns found are: %s.",
                                       paste(shQuote(missing), collapse = ", "),
                                       paste(shQuote(head(colnames(df), 8)), collapse = ", ")))
                    }
                    if (!nrow(df)) return("The table has a header but no rows.")
                    if (!is.numeric(df$FPKM)) return("The FPKM column is not numeric.")
                    NULL
                }
                read_fpkm_file <- function(i) {
                    nasqar_read_table(inFile$datapath[i], expected, check = check_fpkm_file,
                                      label = paste("FPKM file", basename(inFile$name[i])))
                }

                if (length(inFile$datapath) == 0) {
                    return(NULL)
                }

                fileContent <- read_fpkm_file(1)
                # fileContent = fileContent[!grepl("__", fileContent$V1),] #remove rows containing underscores


                ######################


                # Remove all columns except gene_id and FPKM
                fileContent <- fileContent[, colnames(fileContent) %in% c("gene_id", "FPKM")]

                # Sort by gene_id incase they are not sorted
                fileContent <- fileContent[order(fileContent$gene_id), ]

                # Create data frame table
                # one column for the gene ids plus one per file (inFile is a data
                # frame with one row per file, so its length is not the file count)
                total <- data.frame(matrix(ncol = nrow(inFile) + 1, nrow = nrow(fileContent)))

                # Extract sample name from file name
                samplename <- tools::file_path_sans_ext(inFile$name[1])


                # Create first column "gene.ids", and second column for first sample FPKMS
                total[, 1] <- as.character(fileContent$gene_id)
                total[, 2] <- fileContent$FPKM

                # convert to TPMs if selected
                if (input$fpkmToTpm) {
                    total[, 2] <- FPKM_to_TPM(fileContent$FPKM)
                } else {
                    total[, 2] <- fileContent$FPKM
                }

                # Set Column names
                colnames(total)[1] <- "gene.ids"
                colnames(total)[2] <- samplename


                # Add the rest of the sample files FPKMs to data frame table
                for (i in 2:length(inFile$datapath))
                {
                    if (length(inFile$datapath) == 1) {
                        break
                    }

                    fileContent <- read_fpkm_file(i)

                    # Remove all columns except gene_id and FPKM
                    fileContent <- fileContent[, colnames(fileContent) %in% c("gene_id", "FPKM")]

                    # Sort by gene_id incase they are not sorted
                    fileContent <- fileContent[order(fileContent$gene_id), ]

                    # The FPKM columns are placed side by side: the gene set must match the first file
                    if (!identical(as.character(fileContent$gene_id), total[, 1])) {
                        nasqar_input_problem(
                            sprintf("The gene_ids differ from those of the first file, %s (%d genes here, %d there); every file must list the same genes.",
                                    basename(inFile$name[1]), nrow(fileContent), nrow(total)),
                            expected, inFile$datapath[i], paste("FPKM file", basename(inFile$name[i]))
                        )
                    }

                    # extract file name
                    samplename <- tools::file_path_sans_ext(inFile$name[i])

                    # samplename = gsub("_gene_fpkms","",samplename)

                    # if checkbox is checked, convert to TPMs
                    if (input$fpkmToTpm) {
                        total[, i + 1] <- FPKM_to_TPM(fileContent$FPKM)
                    } else {
                        total[, i + 1] <- fileContent$FPKM
                    }

                    # Set the column names to the file names they came from
                    colnames(total)[i + 1] <- samplename

                    # increase progress bar
                    progress$set(value = i / length(inFile$datapath))
                }


                ######################


                if (input$addGeneNames) {
                    geneNames <- getNamesFromEnsembl(total$gene.ids, progress)

                    if (input$geneNameColumn == "add") {
                        total <- as.data.frame(append(total, list(gene.names = geneNames), after = 1))
                    } else {
                        total[, 1] <- make.names(geneNames, unique = TRUE)
                        colnames(total)[1] <- "gene.names"
                    }
                }

                # if(input$addOne)
                #   total[,!(names(total) %in% c("gene.ids","gene.names"))] = total[,!(names(total) %in% c("gene.ids","gene.names"))] + 1
                #


                return(list("data" = total))
            }
        )

    # function to get gene names from ids based on gtf files for reference genome
    getNamesFromEnsembl <- function(ensNames, progress) {
        progress$set(value = 0.3)
        progress$set(message = "Adding gene names ...")


        if (input$refGenome == "Homo_sapiens.GRCh38.81") {
            load("www/exampleData/Homo_sapiens.GRCh38.81.Rda")
        } else if (input$refGenome == "Homo_sapiens.GRCh38.84") {
            load("www/exampleData/Homo_sapiens.GRCh38.84.Rda")
        } else if (input$refGenome == "Mus_musculus.GRCm38.82") {
            load("www/exampleData/Mus_musculus.GRCm38.82.Rda")
        } else if (input$refGenome == "Danio_rerio.GRCz10.84") {
            load("www/exampleData/Danio_rerio.GRCz10.84.Rda")
        } else if (input$refGenome == "Drosophila_melanogaster.BDGP6.81") {
            load("www/Drosophila_melanogaster.BDGP6.81.Rda")
        }


        # using clusters to make it faster

        # Calculate the number of cores
        no_cores <- detectCores() - 1

        # Initiate cluster
        cl <- makeCluster(no_cores)

        print(paste(format(Sys.time(), "%H:%M:%OS3"), ": Started Renaming ", length(ensNames), " genes"))

        levelsList <- parallel::parLapply(cl, ensNames, function(x) {
            return(geneid2name[geneid2name$gene_id == as.character(x), ]$gene_name)
        })


        print(paste(format(Sys.time(), "%H:%M:%OS3"), ": Finished renaming"))
        stopCluster(cl)


        progress$set(value = 0.8)

        flatList <- unlist(levelsList)

        progress$set(value = 1)
        return(flatList)
    }

    # Function to convert FPKMs to TPMs
    FPKM_to_TPM <- function(FPKMCol) {
        # CALCULATE SUM(FPKM)
        Total_FPKMS <- sum(FPKMCol)
        # Divide Total FPKM by 10^6 (so dont have to multiple everythin by 10^6)
        Total_FPKMS_divided <- Total_FPKMS / 1000000
        # process for each Row from FPKM to TPM
        TPMCol <- FPKMCol / Total_FPKMS_divided

        return(TPMCol)
    }


    output$filesUploaded <- reactive({
        return(!is.null(inputDataReactive()))
    })
    outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)

    output$filesMerged <- reactive({
        return(!is.null(dataReactive()))
    })
    outputOptions(output, "filesMerged", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
