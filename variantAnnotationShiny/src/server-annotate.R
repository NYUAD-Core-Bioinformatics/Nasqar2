annotation_options <- function() {
    list(
        promoter_upstream = as.integer(input$promoter_upstream %||% 2000L),
        promoter_downstream = as.integer(input$promoter_downstream %||% 200L),
        add_rsid = !is.null(my_values$genome$snplocs)
    )
}

observeEvent(input$runAnnotation, {
    req(my_values$filtered_path)
    js$addStatusIcon("annotate_tab", "loading")
    reset_downstream("annotate")
    options <- annotation_options()
    genome <- my_values$genome

    signature <- file_signature(my_values$vcf_path)
    key <- cache_key_for(
        "annotate", signature, my_values$filter_spec, input$chromosomes, input$regions,
        input$gene_list, input$gene_flank, options,
        packages = package_versions(genome$packages),
        variantannotation = as.character(utils::packageVersion("VariantAnnotation"))
    )
    cache_dir <- file.path(my_values$project_dir, "cache", key)
    summary_file <- file.path(cache_dir, "summary.rds")

    outcome <- tryCatch({
        cached <- if (file.exists(summary_file)) {
            tryCatch(readRDS(summary_file), error = function(error) NULL)
        } else {
            NULL
        }
        cached_ok <- !is.null(cached) &&
            all(file.exists(c(cached$files$per_variant, cached$files$per_transcript)))
        if (cached_ok) {
            my_values$cache_status <- paste("Loaded cached annotation:", key)
            cached
        } else {
            my_values$cache_status <- paste("Computing annotation:", key)
            result <- withProgress(message = "Annotating variants", value = 0, {
                stream_annotation(
                    my_values$filtered_path, my_values$filtered_index, genome, options, hpc_workers,
                    output_dir = cache_dir,
                    total_records = my_values$filtered_records,
                    has_dp = isTRUE(my_values$overview$has_dp),
                    progress = function(fraction, acc) {
                        setProgress(fraction, detail = sprintf(
                            "chunk %d: %s of %s records, %s alleles annotated",
                            acc$chunks, format_count(acc$input_records),
                            format_count(my_values$filtered_records), format_count(acc$alleles)))
                    }
                )
            })
            write_cache(result, summary_file)
            my_values$cache_status <- paste("Saved annotation cache:", key)
            result
        }
    }, error = function(error) error)

    if (inherits(outcome, "error")) {
        js$addStatusIcon("annotate_tab", "fail")
        my_values$cache_status <- NULL
        showNotification(conditionMessage(outcome), type = "error", duration = 15)
        return()
    }

    my_values$annotation <- outcome
    base <- vcf_base_name(my_values$vcf_name)
    file.copy(outcome$files$per_variant,
        file.path(my_values$project_dir, paste0(base, "_annotated_per_variant.csv")), overwrite = TRUE)
    file.copy(outcome$files$per_transcript,
        file.path(my_values$project_dir, paste0(base, "_annotated_per_transcript.csv")), overwrite = TRUE)

    annotation_ready(TRUE)
    if (private_space) {
        nasqar_project_state_write(
            my_values$project_dir, project_app, basename(my_values$project_dir), "annotated",
            annotation_summary = summary_file, annotation_options = options
        )
        project_refresh(basename(my_values$project_dir))
    }
    js$addStatusIcon("annotate_tab", "done")
    shinyjs::show(selector = "a[data-value=\"plots_tab\"]")
    shinyjs::show(selector = "a[data-value=\"downloads_tab\"]")
    shinyjs::show(selector = "a[data-value=\"handoff_tab\"]")
    js$addStatusIcon("plots_tab", "next")
    js$addStatusIcon("downloads_tab", "download")
})

output$cache_status <- renderText(my_values$cache_status)

output$annotation_report <- renderText({
    req(my_values$annotation)
    report <- my_values$annotation$report
    rows <- my_values$annotation$rows
    lines <- c(
        sprintf("Reference: %s", report$genome),
        sprintf("Filtered records: %s", format_count(report$input_records)),
        sprintf("Alleles annotated: %s", format_count(report$alleles_annotated)),
        sprintf("Coding alleles: %s", format_count(report$coding_variants)),
        sprintf("Genes with variants: %s", format_count(report$genes)),
        sprintf("Rows: %s per variant, %s per variant and transcript",
                format_count(rows$per_variant), format_count(rows$per_transcript)),
        sprintf("Processed in %d chunk(s) of up to %s records", report$chunks, format_count(report$chunk_size)),
        sprintf("Symbolic or non-ACGT alleles removed: %s", report$dropped_alleles),
        sprintf("Records on contigs missing from the gene model: %s", report$dropped_records),
        if (length(report$dropped_contigs)) {
            sprintf("Contigs dropped: %s", paste(report$dropped_contigs, collapse = ", "))
        },
        sprintf("dbSNP rsIDs: %s", ifelse(isTRUE(report$rsid), "added", "not added")),
        sprintf("VariantAnnotation %s; %s", report$variantannotation,
                paste(names(report$packages), report$packages, collapse = "; "))
    )
    paste(lines, collapse = "\n")
})

output$annotation_table_note <- renderText({
    req(my_values$annotation)
    view <- if (identical(input$table_view, "per_transcript")) "per_transcript" else "per_variant"
    total <- my_values$annotation$rows[[view]]
    shown <- nrow(my_values$annotation$previews[[view]] %||% data.frame())
    if (shown < total) {
        sprintf("Showing the first %s of %s rows. The complete table is available under Downloads and in the project directory.",
                format_count(shown), format_count(total))
    } else {
        sprintf("%s rows.", format_count(total))
    }
})

output$annotation_table <- DT::renderDataTable({
    req(my_values$annotation)
    frame <- if (identical(input$table_view, "per_transcript")) {
        my_values$annotation$previews$per_transcript
    } else {
        my_values$annotation$previews$per_variant
    }
    DT::datatable(frame, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
})
