options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output, session) {
    hpc_workers <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")))
    if (is.na(hpc_workers) || hpc_workers < 1L) hpc_workers <- 1L

    data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
    data_root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL

    # Server-side browser jailed to the private data root; it only fills the path box.
    if (!is.null(data_root)) {
        browse_roots <- nasqar_browse_roots()
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


    # Private-space projects: projects/DADA2/<name> (shared convention)
    source("nasqar-project.R", local = TRUE)
    project_app <- "DADA2"
    project_legacy_env <- "NASQAR_DADA2_PROJECT_DIR"
    private_space <- nasqar_project_available(project_app, project_legacy_env)
    project_directory <- function(name) {
        path <- nasqar_project_dir(project_app, name, project_legacy_env, fallback = "dada2-project")
        if (!dir.exists(path) || file.access(path, 2) != 0) {
            stop("The DADA2 private space directory is not writable.")
        }
        path
    }
    current_project_name <- function() {
        name <- input$nasqar_project_name
        if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
    }

    local_working_directory <- function(name) {
        root <- Sys.getenv("NASQAR_DADA2_WORK_DIR", unset = tempdir())
        safe_name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(name))
        if (!nzchar(safe_name)) safe_name <- paste0("session-", session$token)
        path <- file.path(root, safe_name)
        dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
        if (!dir.exists(path) || file.access(path, 2) != 0) {
            stop("The DADA2 node-local work directory is not writable.")
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
