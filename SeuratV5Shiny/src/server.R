
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
  data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
  data_root <- if (nzchar(data_root)) normalizePath(data_root, winslash = "/", mustWork = FALSE) else NULL

  # Server-side browser jailed to the private data root; it only fills the path box.
  if (!is.null(data_root)) {
      browse_roots <- nasqar_browse_roots()
      shinyFiles::shinyDirChoose(input, "browse_private_dir", roots = browse_roots, session = session)
      observeEvent(input$browse_private_dir, {
          selected <- tryCatch(shinyFiles::parseDirPath(browse_roots, input$browse_private_dir), error = function(e) character(0))
          if (length(selected) == 1L && nzchar(selected)) {
              updateTextInput(session, "private_data_path", value = as.character(selected))
          }
      }, ignoreInit = TRUE)
      shinyFiles::shinyFileChoose(input, "browse_private_file", roots = browse_roots, session = session, filetypes = c("csv", "tsv", "txt", "gz", "rds"))
      observeEvent(input$browse_private_file, {
          selected <- tryCatch(shinyFiles::parseFilePaths(browse_roots, input$browse_private_file), error = function(e) NULL)
          if (!is.null(selected) && nrow(selected)) {
              updateTextInput(session, "private_data_path", value = as.character(selected$datapath[1]))
          }
      }, ignoreInit = TRUE)
  } else {
      removeUI(selector = "#browse_private_dir")
      removeUI(selector = "#browse_private_file")
      shinyjs::runjs("$('input[name=data_file_type][value=privateSpace]').closest('.radio').remove()")
  }


  # Private-space projects: projects/SeuratV5/<name> (shared convention); the
  # name comes from the app's own "projectname" input.
  source("nasqar-project.R", local = TRUE)
  project_app <- "SeuratV5"
  project_legacy_env <- "NASQAR_SEURAT_PROJECT_DIR"
  private_space <- nasqar_project_available(project_app, project_legacy_env)
  current_project_name <- function() {
    name <- input$projectname
    if (private_space && !is.null(name) && nzchar(trimws(name))) trimws(name) else paste0("session-", session$token)
  }
  seurat_project_directory <- function(name) {
    path <- nasqar_project_dir(project_app, name, project_legacy_env, fallback = "seurat-project")
    if (!dir.exists(path) || file.access(path, 2) != 0) {
      stop("The Seurat private space directory is not writable.")
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
  source("server-project.R", local = TRUE)

  GotoTab <- function(name){
    
    shinyjs::show(selector = paste0("a[data-value=\"",name,"\"]"))
    
    shinyjs::runjs("window.scrollTo(0, 0)")
  }
}
