header_table <- function(frame) {
    if (is.null(frame) || !nrow(frame)) {
        return(data.frame(Field = character(0), stringsAsFactors = FALSE))
    }
    out <- as.data.frame(frame, stringsAsFactors = FALSE)
    data.frame(Field = rownames(frame), out, row.names = NULL, check.names = FALSE,
               stringsAsFactors = FALSE)
}

output$info_table <- DT::renderDataTable({
    req(my_values$overview)
    DT::datatable(header_table(VariantAnnotation::info(my_values$overview$header)),
                  rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
})

output$format_table <- DT::renderDataTable({
    req(my_values$overview)
    DT::datatable(header_table(VariantAnnotation::geno(my_values$overview$header)),
                  rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
})

output$samples_table <- DT::renderDataTable({
    req(my_values$overview)
    samples <- my_values$overview$samples
    DT::datatable(data.frame(Sample = samples, stringsAsFactors = FALSE),
                  rownames = FALSE, options = list(pageLength = 10))
})

output$contig_table <- DT::renderDataTable({
    req(my_values$overview)
    overview <- my_values$overview
    contigs <- names(overview$contig_counts)
    lengths <- overview$contig_lengths[contigs]
    DT::datatable(data.frame(
        Contig = contigs,
        Records = as.integer(overview$contig_counts),
        Length = ifelse(is.na(lengths), NA, format(lengths, big.mark = ",")),
        stringsAsFactors = FALSE
    ), rownames = FALSE, options = list(pageLength = 10))
})

run_record_summary <- function() {
    req(my_values$overview)
    outcome <- tryCatch({
        withProgress(message = "Streaming VCF records", value = 0, {
            summarise_vcf_file(
                my_values$vcf_path, my_values$vcf_index, my_values$genome$key,
                my_values$overview,
                progress = function(fraction) setProgress(fraction)
            )
        })
    }, error = function(error) error)
    if (inherits(outcome, "error")) {
        showNotification(conditionMessage(outcome), type = "error", duration = 12)
        return(invisible(NULL))
    }
    my_values$summary <- outcome
    summary_ready(TRUE)
    invisible(outcome)
}

observeEvent(input$compute_summary, {
    run_record_summary()
})

output$summary_status <- renderText({
    if (!vcf_ready()) return("Load a VCF first.")
    if (is.null(my_values$summary)) {
        return(paste("Not computed yet.", format(my_values$overview$total_records, big.mark = ","),
                     "records would be streamed."))
    }
    paste("Records summarised:", format(my_values$summary$records, big.mark = ","))
})

count_table <- function(counts, label) {
    if (!length(counts)) return(data.frame(Value = character(0), Records = integer(0)))
    frame <- data.frame(names(counts), as.integer(counts), stringsAsFactors = FALSE)
    names(frame) <- c(label, "Records")
    frame
}

output$type_counts_table <- DT::renderDataTable({
    req(my_values$summary)
    DT::datatable(count_table(my_values$summary$type, "Variant type"), rownames = FALSE,
                  options = list(dom = "t"))
})

output$filter_counts_table <- DT::renderDataTable({
    req(my_values$summary)
    DT::datatable(count_table(my_values$summary$filter, "FILTER"), rownames = FALSE,
                  options = list(dom = "t", pageLength = 25))
})

output$qual_bins_table <- DT::renderDataTable({
    req(my_values$summary)
    frame <- count_table(my_values$summary$qual, "QUAL range")
    frame <- rbind(frame, data.frame(`QUAL range` = "missing", Records = my_values$summary$qual_missing,
                                     check.names = FALSE))
    DT::datatable(frame, rownames = FALSE, options = list(dom = "t", pageLength = 25))
})

output$dp_bins_table <- DT::renderDataTable({
    req(my_values$summary)
    if (!my_values$overview$has_dp) {
        return(DT::datatable(data.frame(Note = "The VCF has no INFO/DP field."), rownames = FALSE,
                             options = list(dom = "t")))
    }
    frame <- count_table(my_values$summary$dp, "DP range")
    frame <- rbind(frame, data.frame(`DP range` = "missing", Records = my_values$summary$dp_missing,
                                     check.names = FALSE))
    DT::datatable(frame, rownames = FALSE, options = list(dom = "t", pageLength = 25))
})
