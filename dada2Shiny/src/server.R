options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output, session) {
    workers <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")))
    if (is.na(workers) || workers < 1L) workers <- 1L

    data_root <- Sys.getenv("NASQAR_CONTAINER_DATA_ROOT", unset = "")
    data_root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL

    # Server-side browser jailed to the private data root; it only fills the path box.
    if (!is.null(data_root)) {
        browse_roots <- nasqar_browse_roots   # a function: the open project comes first
        shinyFiles::shinyDirChoose(input, "browse_fastq_directory", roots = browse_roots, session = session)
        observeEvent(input$browse_fastq_directory, {
            selected <- tryCatch(shinyFiles::parseDirPath(browse_roots, input$browse_fastq_directory), error = function(e) character(0))
            if (length(selected) == 1L && nzchar(selected)) {
                updateTextInput(session, "private_fastq_directory", value = as.character(selected))
            }
        }, ignoreInit = TRUE)
    } else {
        removeUI(selector = "#browse_fastq_directory")
        shinyjs::runjs("$('input[name=data_file_type][value=private_directory]').closest('.radio').remove()")
    }


    # Private-space projects: projects/<name>/DADA2 (shared convention)
    source("nasqar-project.R", local = TRUE)
    project_app <- "DADA2"
    private_space <- nasqar_project_available(project_app)
    # Reopening a project and its files page exist in a private instance only.
    if (!private_space) {
        shinyjs::runjs("$('input[name=data_file_type][value=savedProject]').closest('.radio').remove()")
        shinyjs::runjs("$('a[data-value=projectFilesTab]').closest('li').remove()")
    }
    project_directory <- function(name) {
        path <- nasqar_project_dir(project_app, name, fallback = "dada2-project")
        if (!dir.exists(path) || file.access(path, 2) != 0) {
            stop("The DADA2 private space directory is not writable.")
        }
        path
    }
    current_project_name <- function() {
        name <- input$nasqar_project_name
        if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
    }

    # Filtered FASTQs of a run: <project>/work (the project lives in tempdir()
    # without a private space). Removed again when the run finishes.
    local_working_directory <- function(project_dir, name) {
        path <- file.path(project_dir, "work")
        dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
        if (!dir.exists(path) || file.access(path, 2) != 0) {
            stop("The DADA2 work directory is not writable.")
        }
        path
    }

    observe({
        # Ping the server every 10 seconds to keep the connection alive
        invalidateLater(10000, session)
        input$keepAlive
    })
    source("analysis-helpers.R", local = TRUE)
    source("server-inputdata.R", local = TRUE)
    source("server-qualityprofile.R", local = TRUE)
    source("server-filter_and_trim.R", local = TRUE)
    source("server-errorRates.R", local = TRUE)
    source("server-mergePairedReads.R", local = TRUE)
    source("server-trackReads.R", local = TRUE)
    source("server-taxonomy.R", local = TRUE)
    source("server-alphaDiversity.R", local = TRUE)
    source("server-project.R", local = TRUE)
}
