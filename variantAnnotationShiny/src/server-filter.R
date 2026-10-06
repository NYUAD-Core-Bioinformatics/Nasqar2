current_filter_spec <- function() {
    list(
        pass_only = isTRUE(input$pass_only),
        min_qual = if (is.null(input$min_qual) || is.na(input$min_qual)) NA_real_ else as.numeric(input$min_qual),
        min_dp = if (is.null(input$min_dp) || is.na(input$min_dp)) NA_real_ else as.numeric(input$min_dp),
        types = if (length(input$variant_types)) input$variant_types else variant_type_levels()
    )
}

output$scan_estimate <- renderText({
    req(my_values$overview)
    overview <- my_values$overview
    regions <- tryCatch(parse_regions(input$regions), error = function(error) NULL)
    if (is.null(regions)) return("Check the region syntax: chr:start-end, one per line.")
    genes <- parse_gene_symbols(input$gene_list)
    if (length(genes)) {
        return(sprintf("%d gene symbol(s) will be mapped to gene regions when the filters run.", length(genes)))
    }
    plan <- scan_plan(overview, input$chromosomes, regions)
    if (identical(plan$mode, "regions")) {
        sprintf("At most %s records will be read from the selected regions.", format_count(plan$estimate))
    } else if (identical(plan$mode, "single")) {
        sprintf("%s records will be read in one pass.", format_count(plan$estimate))
    } else {
        sprintf("%s records will be streamed in chunks of %s; records passing the filters are written to the project directory, so any number can be kept.",
                format_count(plan$estimate), format_count(yield_size_limit()))
    }
})

observeEvent(input$applyFilter, {
    req(my_values$overview)
    js$addStatusIcon("filter_tab", "loading")
    reset_downstream("filter")
    spec <- current_filter_spec()
    overview <- my_values$overview
    genome <- my_values$genome
    base <- vcf_base_name(my_values$vcf_name)
    writer <- new_filtered_vcf_writer(file.path(my_values$project_dir, paste0(base, "_filtered")))

    outcome <- tryCatch({
        withProgress(message = "Filtering variants", value = 0, {
            regions <- parse_regions(input$regions)
            notes <- character(0)
            genes <- parse_gene_symbols(input$gene_list)
            if (length(genes)) {
                setProgress(0.02, detail = "Mapping gene symbols")
                mapped <- gene_regions(genes, genome$txdb, genome$orgdb, flank = input$gene_flank %||% 0,
                                       gene_keytype = genome$gene_keytype, symbol_column = genome$symbol_column)
                if (length(mapped$missing)) {
                    notes <- c(notes, paste("Gene symbols not found in the gene model:",
                                            paste(mapped$missing, collapse = ", ")))
                }
                if (!length(mapped$regions)) {
                    stop("None of the gene symbols could be mapped to the selected gene model.", call. = FALSE)
                }
                gene_gr <- GenomicRanges::GRanges(
                    as.character(GenomeInfoDb::seqnames(mapped$regions)),
                    IRanges::ranges(mapped$regions)
                )
                regions <- if (length(regions)) c(regions, gene_gr) else gene_gr
            }
            plan <- scan_plan(overview, input$chromosomes, regions)
            predicate <- build_filter_predicate(spec)
            counts <- stream_vcf_filtered(
                my_values$vcf_path, my_values$vcf_index, genome$key, overview, plan, predicate, writer,
                progress = function(fraction, scanned, kept) {
                    setProgress(fraction, detail = sprintf("%s scanned, %s kept",
                        format_count(scanned), format_count(kept)))
                }
            )
            if (!counts$kept) {
                writer$discard()
                stop("No variants passed the filters.", call. = FALSE)
            }
            setProgress(0.98, detail = "Compressing and indexing the filtered VCF")
            written <- writer$finish()
            list(counts = counts, written = written, plan = plan, notes = notes)
        })
    }, error = function(error) {
        writer$discard()
        error
    })

    if (inherits(outcome, "error")) {
        js$addStatusIcon("filter_tab", "fail")
        showNotification(conditionMessage(outcome), type = "error", duration = 15)
        return()
    }

    my_values$filtered_path <- outcome$written$path
    my_values$filtered_index <- outcome$written$index
    my_values$filtered_records <- outcome$written$records
    my_values$filtered_sample_counts <- outcome$written$sample_counts
    my_values$filter_spec <- spec
    my_values$filter_notes <- outcome$notes
    my_values$filter_stats <- data.frame(
        Metric = c("Read mode", "Records scanned", "Records kept", "PASS only",
                   "Minimum QUAL", "Minimum DP", "Variant types",
                   "Chromosomes", "Regions or genes", "Filtered VCF"),
        Value = c(
            outcome$plan$mode,
            format_count(outcome$counts$scanned),
            format_count(outcome$counts$kept),
            ifelse(spec$pass_only, "yes", "no"),
            ifelse(is.na(spec$min_qual), "none", spec$min_qual),
            ifelse(is.na(spec$min_dp), "none", spec$min_dp),
            paste(spec$types, collapse = ", "),
            ifelse(length(input$chromosomes), paste(input$chromosomes, collapse = ", "), "all"),
            ifelse(identical(outcome$plan$mode, "regions"),
                   paste(length(outcome$plan$regions), "tile(s)"), "none"),
            outcome$written$path
        ),
        stringsAsFactors = FALSE
    )
    filter_ready(TRUE)
    if (private_space) {
        nasqar_project_state_write(
            my_values$project_dir, project_app, basename(my_values$project_dir), "filtered",
            filter_spec = spec, chromosomes = input$chromosomes, regions = input$regions,
            gene_list = input$gene_list, gene_flank = input$gene_flank,
            filtered_path = outcome$written$path, filtered_index = outcome$written$index,
            filtered_records = outcome$written$records,
            filtered_sample_counts = outcome$written$sample_counts,
            filter_stats = my_values$filter_stats, filter_notes = outcome$notes
        )
        project_refresh(basename(my_values$project_dir))
    }
    js$addStatusIcon("filter_tab", "done")
    shinyjs::show(selector = "a[data-value=\"annotate_tab\"]")
    js$addStatusIcon("annotate_tab", "next")
})

output$filter_stats_table <- DT::renderDataTable({
    req(my_values$filter_stats)
    DT::datatable(my_values$filter_stats, rownames = FALSE, options = list(dom = "t", pageLength = 20))
})

output$filter_notes <- renderText({
    notes <- my_values$filter_notes
    if (is.null(notes) || !length(notes)) return("")
    paste(notes, collapse = " ")
})
