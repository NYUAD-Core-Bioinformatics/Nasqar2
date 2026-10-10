# Private-space project state and restore (shared convention).
# DADA2 results and taxonomy are cached in the project, so restoring replays
# the inputs and re-triggers the stages reached; the caches make that fast.

restore_pending <- reactiveVal(NULL)

observeEvent(input_files_reactive(), {
    samples <- input_files_reactive()
    if (!private_space || is.null(my_values$work_dir) || is.null(samples) || !nrow(samples)) return()
    label <- switch(input$data_file_type,
        private_directory = basename(my_values$base_dir %||% ""),
        example_fastq_file = "MiSeq_SOP example",
        "uploaded FASTQ files")
    nasqar_project_state_write(
        my_values$work_dir, project_app, basename(my_values$work_dir), "loaded",
        input_label = label, source_type = input$data_file_type,
        base_dir = my_values$base_dir, seq_type = input$seq_type,
        forward_pattern = input$forward_pattern, reverse_pattern = input$reverse_pattern,
        samples = rownames(samples)
    )
    project_refresh(basename(my_values$work_dir))
    pending <- restore_pending()
    if (!is.null(pending) && pending$stage %in% c("dada2", "taxonomy")) {
        shinyjs::delay(800, shinyjs::click("runDADA2"))
    } else {
        restore_pending(NULL)
    }
})

observeEvent(reactiveInputData(), {
    if (!private_space || is.null(my_values$work_dir)) return()
    nasqar_project_state_write(
        my_values$work_dir, project_app, basename(my_values$work_dir), "dada2",
        parameters = list(
            trunc_fwd = input$truncLen_fwd, trunc_rev = input$truncLen_rev,
            max_ee_fwd = input$maxEE_fwd, max_ee_rev = input$maxEE_rev
        ),
        results_file = file.path(my_values$work_dir, "dada2-results.rds")
    )
    project_refresh(basename(my_values$work_dir))
    pending <- restore_pending()
    if (!is.null(pending) && identical(pending$stage, "taxonomy")) {
        shinyjs::delay(800, shinyjs::click("assignTaxonomy"))
    } else {
        restore_pending(NULL)
    }
})

observeEvent(reactiveTaxonomyData$taxa, {
    if (!private_space || is.null(my_values$work_dir)) return()
    nasqar_project_state_write(
        my_values$work_dir, project_app, basename(my_values$work_dir), "taxonomy",
        database_choice = input$database_choice,
        species_assignment_choice = input$species_assignment_choice,
        taxonomy_file = file.path(my_values$work_dir, "taxonomy-table.csv")
    )
    project_refresh(basename(my_values$work_dir))
    restore_pending(NULL)
})

restore_project <- function(state, dir) {
    if (!identical(state$app, project_app)) return("this is not a DADA2 project")
    if (!isTRUE(state$source_type %in% c("private_directory", "hpc_scratch_directory"))) {
        return("this project was made from uploaded or example files; load them again to rerun it (its caches are kept)")
    }
    if (is.null(state$base_dir) || !dir.exists(state$base_dir)) {
        return("the project's FASTQ directory is no longer available")
    }
    updateRadioButtons(session, "data_file_type", selected = "private_directory")
    updateTextInput(session, "private_fastq_directory", value = state$base_dir)
    updateRadioButtons(session, "seq_type", selected = state$seq_type %||% "paired")
    updateTextInput(session, "forward_pattern", value = state$forward_pattern %||% "_R1_001.fastq")
    updateTextInput(session, "reverse_pattern", value = state$reverse_pattern %||% "_R2_001.fastq")
    parameters <- state$parameters
    if (!is.null(parameters)) {
        updateNumericInput(session, "truncLen_fwd", value = parameters$trunc_fwd)
        updateNumericInput(session, "truncLen_rev", value = parameters$trunc_rev)
        updateNumericInput(session, "maxEE_fwd", value = parameters$max_ee_fwd)
        updateNumericInput(session, "maxEE_rev", value = parameters$max_ee_rev)
    }
    if (!is.null(state$database_choice)) {
        updateRadioButtons(session, "database_choice", selected = state$database_choice)
    }
    if (!is.null(state$species_assignment_choice)) {
        updateRadioButtons(session, "species_assignment_choice", selected = state$species_assignment_choice)
    }
    restore_pending(list(stage = state$stage %||% "loaded", name = basename(dir)))
    shinyjs::delay(800, shinyjs::click("initFastq"))
    NULL
}

project_refresh <- nasqar_project_server(
    input, output, session, project_app, project_legacy_env, on_restore = restore_project
)
