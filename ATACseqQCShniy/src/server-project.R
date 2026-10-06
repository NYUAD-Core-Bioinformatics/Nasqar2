# Private-space project state and restore (shared convention). ATACseqQC
# results are cached per project, so restoring replays the directory input
# and later steps are served from the cache.

observeEvent(my_values$scratch_dir, {
    if (!private_space || is.null(my_values$scratch_dir)) return()
    dir <- nasqar_project_dir(project_app, current_project_name(), fallback = "atac-project")
    nasqar_project_state_write(
        dir, project_app, basename(dir), "loaded",
        input_label = basename(my_values$scratch_dir), source_type = "hpc_scratch_directory",
        scratch_directory = my_values$scratch_dir, bs_genome = input$bs_genome_input
    )
    project_refresh(basename(dir))
}, ignoreInit = TRUE)

observeEvent(input$bs_genome_input, {
    if (!private_space || is.null(my_values$scratch_dir) || is.null(input$bs_genome_input) || !nzchar(input$bs_genome_input)) return()
    dir <- nasqar_project_dir(project_app, current_project_name(), fallback = "atac-project")
    if (!is.null(nasqar_project_state_read(dir))) {
        nasqar_project_state_write(dir, project_app, basename(dir), "loaded", bs_genome = input$bs_genome_input)
    }
}, ignoreInit = TRUE)

restore_project <- function(state, dir) {
    if (!identical(state$app, project_app)) return("this is not an ATACseqQC project")
    if (is.null(state$scratch_directory) || !dir.exists(state$scratch_directory)) {
        return("the project's BAM directory is no longer available")
    }
    updateRadioButtons(session, "data_file_type", selected = "hpc_scratch_directory")
    updateTextInput(session, "scratch_directory", value = state$scratch_directory)
    shinyjs::delay(800, shinyjs::click("load_scratch_directory"))
    if (!is.null(state$bs_genome) && nzchar(state$bs_genome)) {
        shinyjs::delay(4000, updateSelectInput(session, "bs_genome_input", selected = state$bs_genome))
    }
    NULL
}

project_refresh <- nasqar_project_server(
    input, output, session, project_app, on_restore = restore_project
)
