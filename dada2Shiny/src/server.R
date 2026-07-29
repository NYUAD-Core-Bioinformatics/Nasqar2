options(shiny.maxRequestSize = 30 * 1024^4)

server <- function(input, output, session) {
    hpc_workers <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")))
    if (is.na(hpc_workers) || hpc_workers < 1L) hpc_workers <- 1L

    resolve_hpc_path <- function(path) {
        root <- normalizePath(
            Sys.getenv("NASQAR_DATA_ROOT", unset = "/scratch/nr83"),
            mustWork = TRUE
        )
        resolved <- normalizePath(path, mustWork = TRUE)
        allowed <- identical(resolved, root) ||
            startsWith(resolved, paste0(root, .Platform$file.sep))
        if (!allowed) stop("The directory must be inside the configured HPC data root.")
        if (!dir.exists(resolved)) stop("The HPC FASTQ path must be a directory.")
        resolved
    }

    project_directory <- function(name) {
        root <- Sys.getenv("NASQAR_DADA2_PROJECT_DIR", unset = "")
        if (!nzchar(root)) return(file.path(tempdir(), "dada2-project"))
        safe_name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(name))
        if (!nzchar(safe_name)) safe_name <- "dada2-project"
        path <- file.path(root, safe_name)
        dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
        if (!dir.exists(path) || file.access(path, 2) != 0) {
            stop("The DADA2 HPC project directory is not writable.")
        }
        path
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
    source("server-inputdata.R", local = TRUE)
    source("server-qualityprofile.R", local = TRUE)
    source("server-filter_and_trim.R", local = TRUE)
    source("server-errorRates.R", local = TRUE)
    source("server-mergePairedReads.R", local = TRUE)
    source("server-trackReads.R", local = TRUE)
    source("server-taxonomy.R", local = TRUE)
    source("server-alphaDiversity.R", local = TRUE)
}
