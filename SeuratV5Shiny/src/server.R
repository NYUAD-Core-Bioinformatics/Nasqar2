
#max upload 300mb
options(shiny.maxRequestSize = 400*1024^2)
options(future.globals.maxSize = 3000 * 1024 ^ 2)
library(future)
nasqar_seurat_workers <- suppressWarnings(
  as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
)
if (is.na(nasqar_seurat_workers) || nasqar_seurat_workers < 1L) {
  nasqar_seurat_workers <- 1L
}
plan("multisession", workers = nasqar_seurat_workers)

server <- function(input, output, session) {
  resolve_hpc_path <- function(path) {
    root <- normalizePath(
      Sys.getenv("NASQAR_DATA_ROOT", unset = "/scratch/nr83"),
      mustWork = TRUE
    )
    resolved <- normalizePath(path, mustWork = TRUE)
    allowed <- identical(resolved, root) ||
      startsWith(resolved, paste0(root, .Platform$file.sep))
    if (!allowed) stop("The path must be inside the configured HPC data root.")
    resolved
  }

  seurat_project_directory <- function(name) {
    root <- Sys.getenv("NASQAR_SEURAT_PROJECT_DIR", unset = "")
    if (!nzchar(root)) return(tempdir())
    safe_name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(name))
    if (!nzchar(safe_name)) safe_name <- "seurat-project"
    path <- file.path(root, safe_name)
    dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!dir.exists(path) || file.access(path, 2) != 0) {
      stop("The Seurat HPC project directory is not writable.")
    }
    path
  }

  source("server-initInputData.R",local = TRUE)

  source("server-qcfilter.R",local = TRUE)

  source("server-vln.R",local = TRUE)

  source("server-normSelect.R",local = TRUE)

  source("server-dispersion.R",local = TRUE)

  source("server-runPca.R",local = TRUE)

  source("server-pcaPlots.R",local = TRUE)

  source("server-jackStraw.R",local = TRUE)

  source("server-clusterCells.R",local = TRUE)

  source("server-nonLinReduction.R", local = TRUE)

  source("server-download.R",local = TRUE)

  source("server-findMarkers.R",local = TRUE)

  GotoTab <- function(name){
    
    shinyjs::show(selector = paste0("a[data-value=\"",name,"\"]"))
    
    shinyjs::runjs("window.scrollTo(0, 0)")
  }
}
