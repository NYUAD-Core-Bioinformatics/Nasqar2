# Private-space project state and restore (shared convention). Restoring
# supplies the saved input table, replays the settings step by step (the
# option lists are filled by the app as the previous step completes) and
# re-runs the enrichment, which the project cache serves instantly.

restore_pending <- reactiveVal(NULL)

schedule_restore_step <- function(step, delay_ms = 2500) {
    shinyjs::runjs(sprintf(
        "setTimeout(function() { Shiny.setInputValue('nasqar_restore_step', %d, {priority: 'event'}); }, %d);",
        step, delay_ms))
}

set_restored_input <- function(params, ids) {
    for (id in ids) {
        value <- params[[id]]
        if (!is.null(value)) session$sendInputMessage(id, list(value = value))
    }
}

observeEvent(inputDataReactive(), {
    data <- inputDataReactive()
    if (!private_space || is.null(data)) return()
    dir <- nasqar_project_dir(project_app, current_project_name(), fallback = "ora-project")
    input_file <- file.path(dir, "input-table.csv")
    restored <- restored_input()
    if (is.null(restored) || !identical(normalizePath(restored, mustWork = FALSE), normalizePath(input_file, mustWork = FALSE))) {
        utils::write.csv(data$data, input_file, row.names = FALSE)
    }
    label <- if (!is.null(input$datafile)) input$datafile$name[1] else if (identical(input$data_file_type, "examplecounts")) "example data" else "gene table"
    nasqar_project_state_write(dir, project_app, basename(dir), "loaded", input_label = label, input_file = input_file)
    project_refresh(basename(dir))
    if (!is.null(restore_pending())) schedule_restore_step(1)
})

observeEvent(enrichGoReactive(), {
    if (!private_space || is.null(enrichGoReactive())) return()
    dir <- nasqar_project_dir(project_app, current_project_name(), fallback = "ora-project")
    nasqar_project_state_write(dir, project_app, basename(dir), "enriched", params = enrichment_params())
    project_refresh(basename(dir))
})

observeEvent(input$nasqar_restore_step, {
    pending <- restore_pending()
    if (is.null(pending)) return()
    params <- pending$params %||% list()
    step <- input$nasqar_restore_step
    if (step == 1) {
        set_restored_input(params, c("geneColumn", "log2fcColumn", "padjColumn"))
        schedule_restore_step(2, 1500)
    } else if (step == 2) {
        shinyjs::click("nextInitParams")          # the app repopulates the organism list here
        schedule_restore_step(3, 2500)
    } else if (step == 3) {
        set_restored_input(params, "organismDb")  # which in turn repopulates the keytype list
        schedule_restore_step(4, 3000)
    } else if (step == 4) {
        set_restored_input(params, setdiff(names(params), c("geneColumn", "log2fcColumn", "padjColumn", "organismDb")))
        schedule_restore_step(5, 2000)
    } else {
        restore_pending(NULL)
        if (identical(pending$stage, "enriched")) shinyjs::click("initGo")
    }
})

restore_project <- function(state, dir) {
    if (!identical(state$app, project_app)) return("this project belongs to another application")
    if (is.null(state$input_file) || !file.exists(state$input_file)) return("the project's input table is missing")
    restore_pending(list(stage = state$stage, params = state$params))
    project_name_override(basename(dir))
    restored_file_type(input$data_file_type)
    restored_input(state$input_file)
    NULL
}

observeEvent(input$nasqar_project_name, project_name_override(NULL), ignoreInit = TRUE)

project_refresh <- nasqar_project_server(input, output, session, project_app, on_restore = restore_project)
