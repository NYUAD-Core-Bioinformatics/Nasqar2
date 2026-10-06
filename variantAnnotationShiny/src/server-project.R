# Restore a private-space project: repopulate the session from state.rds and
# the files it points at, without recomputing anything.

restore_project <- function(state, dir) {
    if (!identical(state$app, project_app)) return("this is not a Variant Annotation project")
    resource <- available_genomes()[[state$genome %||% ""]]
    if (is.null(resource)) {
        return(sprintf("reference genome '%s' is not installed on this instance", state$genome %||% "?"))
    }
    if (is.null(state$vcf_path) || !file.exists(state$vcf_path) ||
        is.null(state$vcf_index) || !file.exists(state$vcf_index)) {
        return("the project's VCF is no longer available; load it again")
    }
    if (!is.null(state$source_signature) && file.exists(state$source_path %||% "") &&
        !nasqar_signature_matches(state$source_signature, state$source_path)) {
        return("the source VCF changed since this project was made; load it again instead of restoring")
    }

    js$addStatusIcon("input_tab", "loading")
    vcf_ready(FALSE)
    summary_ready(FALSE)
    my_values$summary <- NULL
    my_values$input_status <- NULL
    reset_downstream("filter")

    overview <- vcf_overview(state$vcf_path, state$vcf_index)
    genome <- load_genome_resource(resource, with_snplocs = TRUE)
    my_values$vcf_path <- state$vcf_path
    my_values$vcf_index <- state$vcf_index
    my_values$vcf_name <- state$vcf_name %||% basename(state$vcf_path)
    my_values$overview <- overview
    my_values$genome <- genome
    my_values$project_dir <- dir
    updateSelectInput(session, "genome_input", selected = resource$key)
    if (identical(state$source_type, "hpc_vcf_file")) {
        updateRadioButtons(session, "data_file_type", selected = "hpc_vcf_file")
        updateTextInput(session, "hpc_vcf_path", value = state$source_path %||% "")
    }
    vcf_ready(TRUE)
    updateSelectizeInput(session, "chromosomes",
        choices = names(overview$contig_counts), selected = state$chromosomes %||% character(0), server = TRUE)
    js$addStatusIcon("input_tab", "done")
    shinyjs::show(selector = "a[data-value=\"summary_tab\"]")
    shinyjs::show(selector = "a[data-value=\"filter_tab\"]")
    js$addStatusIcon("summary_tab", "done")
    js$addStatusIcon("filter_tab", "next")
    if (overview$total_records <= yield_size_limit()) run_record_summary()

    stage <- state$stage %||% "loaded"
    if (!stage %in% c("filtered", "annotated")) return(NULL)
    if (is.null(state$filtered_path) || !file.exists(state$filtered_path) ||
        is.null(state$filtered_index) || !file.exists(state$filtered_index)) {
        showNotification("The project's filtered VCF is missing; restored up to the loaded VCF.", type = "warning", duration = 10)
        return(NULL)
    }
    spec <- state$filter_spec %||% default_filter_spec()
    updateCheckboxInput(session, "pass_only", value = isTRUE(spec$pass_only))
    updateNumericInput(session, "min_qual", value = if (is.na(spec$min_qual %||% NA)) NA else spec$min_qual)
    updateNumericInput(session, "min_dp", value = if (is.na(spec$min_dp %||% NA)) NA else spec$min_dp)
    updateCheckboxGroupInput(session, "variant_types", selected = spec$types %||% variant_type_levels())
    updateTextAreaInput(session, "regions", value = state$regions %||% "")
    updateTextAreaInput(session, "gene_list", value = state$gene_list %||% "")
    updateNumericInput(session, "gene_flank", value = state$gene_flank %||% 2000)
    my_values$filtered_path <- state$filtered_path
    my_values$filtered_index <- state$filtered_index
    my_values$filtered_records <- state$filtered_records
    my_values$filtered_sample_counts <- state$filtered_sample_counts
    my_values$filter_spec <- spec
    my_values$filter_notes <- state$filter_notes
    my_values$filter_stats <- state$filter_stats
    filter_ready(TRUE)
    js$addStatusIcon("filter_tab", "done")
    shinyjs::show(selector = "a[data-value=\"annotate_tab\"]")
    js$addStatusIcon("annotate_tab", "next")

    if (!identical(stage, "annotated")) return(NULL)
    summary_file <- state$annotation_summary %||% ""
    summary <- if (file.exists(summary_file)) tryCatch(readRDS(summary_file), error = function(error) NULL) else NULL
    if (is.null(summary) || !all(file.exists(c(summary$files$per_variant, summary$files$per_transcript)))) {
        showNotification("The project's annotation results are missing; restored up to the filtered VCF.", type = "warning", duration = 10)
        return(NULL)
    }
    options <- state$annotation_options
    if (!is.null(options)) {
        updateNumericInput(session, "promoter_upstream", value = options$promoter_upstream %||% 2000)
        updateNumericInput(session, "promoter_downstream", value = options$promoter_downstream %||% 200)
    }
    my_values$annotation <- summary
    my_values$cache_status <- paste("Restored annotation from", basename(dirname(summary_file)))
    annotation_ready(TRUE)
    js$addStatusIcon("annotate_tab", "done")
    shinyjs::show(selector = "a[data-value=\"plots_tab\"]")
    shinyjs::show(selector = "a[data-value=\"downloads_tab\"]")
    shinyjs::show(selector = "a[data-value=\"handoff_tab\"]")
    js$addStatusIcon("plots_tab", "next")
    js$addStatusIcon("downloads_tab", "download")
    NULL
}

project_refresh <- nasqar_project_server(
    input, output, session, project_app, project_legacy_env, on_restore = restore_project
)
