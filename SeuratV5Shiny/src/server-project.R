# Private-space project state and restore (shared convention).
# Restoring replays the input (data-root path) and, when the project holds a
# saved Seurat object, makes it available on the finish tab for download and
# the Monocle 3 handoff.

restore_pending <- reactiveVal(NULL)

observeEvent(inputDataReactive(), {
  if (!private_space || is.null(inputDataReactive())) return()
  # Every session starts with the same default project name: take a free one
  # rather than overwrite another session's project (restored projects are kept).
  project_name <- nasqar_project_unique_name(
    project_app, current_project_name(), project_legacy_env,
    owned = session$userData$nasqar_project_last_dir, fallback = "seurat-project"
  )
  if (!identical(project_name, nasqar_project_sanitize(current_project_name(), "seurat-project"))) {
    updateTextInput(session, "projectname", value = project_name)
    showNotification(
      sprintf("Project renamed to '%s': the name was already taken by another project.", project_name),
      type = "warning", duration = 10
    )
  }
  dir <- seurat_project_directory(project_name)
  label <- if (identical(input$data_file_type, "privateSpace")) basename(input$private_data_path) else input$data_file_type
  nasqar_project_state_write(
    dir, project_app, basename(dir), "loaded",
    input_label = label, source_type = input$data_file_type,
    private_data_path = input$private_data_path, projectname = project_name,
    mincells = input$mincells, mingenes = input$mingenes
  )
  project_refresh(basename(dir))
  pending <- restore_pending()
  if (!is.null(pending)) {
    restore_pending(NULL)
    if (identical(pending$stage, "seurat") && file.exists(pending$object_file %||% "")) {
      pbmc <- withProgress(message = "Loading the project's saved Seurat object, please wait", {
        tryCatch(readRDS(pending$object_file), error = function(e) NULL)
      })
      if (is.null(pbmc)) {
        showNotification("The project's saved Seurat object could not be read.", type = "error", duration = 10)
        return()
      }
      myValues$finalData = list('pbmc'=pbmc)
      myValues$filepath <- pending$object_file
      myValues$privateOutputPath <- pending$object_file
      shinyjs::show(selector = "a[data-value=\"finishTab\"]")
      showNotification("The project's saved Seurat object is available on the Download R Object tab.", type = "message", duration = 8)
    }
  }
})

restore_project <- function(state, dir) {
  if (!identical(state$app, project_app)) return("this is not a Seurat v5 project")
  if (!identical(state$source_type, "privateSpace")) return("this project was made from uploaded files; upload them again to rerun it")
  data_path <- state$private_data_path
  if (is.null(data_path) || !file.exists(data_path)) return("the project's input data is no longer available")
  updateRadioButtons(session, "data_file_type", selected = "privateSpace")
  updateTextInput(session, "private_data_path", value = data_path)
  updateTextInput(session, "projectname", value = state$projectname %||% basename(dir))
  if (!is.null(state$mincells)) updateNumericInput(session, "mincells", value = state$mincells)
  if (!is.null(state$mingenes)) updateNumericInput(session, "mingenes", value = state$mingenes)
  restore_pending(list(stage = state$stage, object_file = state$object_file))
  shinyjs::delay(800, shinyjs::click("load_private_data"))
  NULL
}

project_refresh <- nasqar_project_server(
  input, output, session, project_app, project_legacy_env, on_restore = restore_project
)
