download_base <- function() vcf_base_name(my_values$vcf_name %||% "variants")

output$download_annot_per_variant <- nasqar_download_handler(
    filename = function() paste0(download_base(), "_annotated_per_variant.csv"),
    content = function(file) {
        req(my_values$annotation)
        file.copy(my_values$annotation$files$per_variant, file, overwrite = TRUE)
    }
)

output$download_annot_per_transcript <- nasqar_download_handler(
    filename = function() paste0(download_base(), "_annotated_per_transcript.csv"),
    content = function(file) {
        req(my_values$annotation)
        file.copy(my_values$annotation$files$per_transcript, file, overwrite = TRUE)
    }
)

output$download_gene_list <- nasqar_download_handler(
    filename = function() paste0(download_base(), "_genes.csv"),
    content = function(file) {
        req(my_values$annotation)
        genes <- my_values$annotation$genes[, c("gene_id", "gene_symbol"), drop = FALSE]
        genes <- genes[order(genes$gene_symbol), , drop = FALSE]
        utils::write.csv(genes, file, row.names = FALSE)
    }
)

output$download_filter_stats <- nasqar_download_handler(
    filename = function() paste0(download_base(), "_filter_summary.csv"),
    content = function(file) {
        req(my_values$filter_stats)
        utils::write.csv(my_values$filter_stats, file, row.names = FALSE)
    }
)

output$download_filtered_vcf <- nasqar_download_handler(
    filename = function() paste0(download_base(), "_filtered.vcf.bgz"),
    contentType = "application/gzip",
    content = function(file) {
        req(my_values$filtered_path)
        if (!file.copy(my_values$filtered_path, file, overwrite = TRUE)) {
            stop("Could not prepare the filtered VCF download.", call. = FALSE)
        }
    }
)

output$downloads_project_path <- renderText({
    req(my_values$project_dir)
    paste("Annotation tables and caches are also written to:", my_values$project_dir)
})
