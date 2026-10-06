plot_theme <- function() {
    theme_bw(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "bottom")
}

empty_plot <- function(message) {
    ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = message, size = 5) +
        theme_void()
}

annotation_stats <- reactive({
    req(my_values$annotation)
    my_values$annotation$stats
})

named_counts_frame <- function(counts, name, levels = names(counts)) {
    frame <- data.frame(key = factor(names(counts), levels = levels), Freq = as.numeric(counts))
    names(frame)[1] <- name
    frame
}

variant_type_plot <- reactive({
    counts <- named_counts_frame(annotation_stats()$type_counts, "type", c("SNV", "Indel", "MNV"))
    ggplot(counts, aes(x = type, y = Freq)) +
        geom_col(fill = "#4877d2") +
        geom_text(aes(label = format(Freq, big.mark = ",")), vjust = -0.3) +
        labs(x = NULL, y = "Alleles") +
        plot_theme()
})

location_plot <- reactive({
    counts <- named_counts_frame(annotation_stats()$location_counts, "location")
    ggplot(counts, aes(x = location, y = Freq)) +
        geom_col(fill = "#00a4a6") +
        labs(x = NULL, y = "Variant-transcript pairs") +
        plot_theme() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
})

consequence_plot <- reactive({
    counts <- named_counts_frame(annotation_stats()$effect_counts, "effect", consequence_severity_levels())
    counts <- counts[counts$Freq > 0, , drop = FALSE]
    counts$class <- effect_class(as.character(counts$effect))
    ggplot(counts, aes(x = effect, y = Freq, fill = class)) +
        geom_col() +
        scale_fill_brewer(palette = "Set2", name = NULL) +
        labs(x = NULL, y = "Variants") +
        plot_theme() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
})

titv_plot <- reactive({
    stats <- annotation_stats()
    counts <- named_counts_frame(stats$substitution_counts, "substitution")
    counts$class <- ifelse(counts$substitution %in% c("A>G", "G>A", "C>T", "T>C"), "Transition", "Transversion")
    ratio <- stats$titv
    subtitle <- sprintf("Ti = %s, Tv = %s, Ti/Tv = %s",
        format(ratio$transitions, big.mark = ","), format(ratio$transversions, big.mark = ","),
        ifelse(is.na(ratio$ratio), "NA", sprintf("%.2f", ratio$ratio)))
    ggplot(counts, aes(x = substitution, y = Freq, fill = class)) +
        geom_col() +
        scale_fill_manual(values = c(Transition = "#d95f02", Transversion = "#7570b3"), name = NULL) +
        labs(x = NULL, y = "SNVs", subtitle = subtitle) +
        plot_theme()
})

per_chromosome_plot <- reactive({
    levels <- my_values$annotation$seqlevels
    counts <- named_counts_frame(annotation_stats()$chromosome_counts, "chromosome", levels)
    counts <- counts[counts$Freq > 0, , drop = FALSE]
    ggplot(counts, aes(x = chromosome, y = Freq)) +
        geom_col(fill = "#4877d2") +
        labs(x = NULL, y = "Variants") +
        plot_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

qual_hist_plot <- reactive({
    values <- annotation_stats()$qual
    if (!length(values)) return(empty_plot("No QUAL values in the filtered variants."))
    ggplot(data.frame(QUAL = values), aes(x = QUAL)) +
        geom_histogram(bins = 50, fill = "#4877d2", colour = "white") +
        scale_x_log10() +
        labs(x = "QUAL (log scale)", y = "Variants") +
        plot_theme()
})

dp_hist_plot <- reactive({
    values <- annotation_stats()$dp
    if (!length(values)) return(empty_plot("The VCF has no INFO/DP values."))
    ggplot(data.frame(DP = values), aes(x = DP)) +
        geom_histogram(bins = 50, fill = "#00a4a6", colour = "white") +
        scale_x_log10() +
        labs(x = "INFO/DP (log scale)", y = "Variants") +
        plot_theme()
})

per_sample_counts <- reactive({
    req(my_values$filtered_sample_counts)
    my_values$filtered_sample_counts
})

output$has_samples <- reactive({
    annotation_ready() && !is.null(my_values$filtered_sample_counts) &&
        length(my_values$overview$samples) > 1L
})
outputOptions(output, "has_samples", suspendWhenHidden = FALSE)

per_sample_plot <- reactive({
    counts <- per_sample_counts()
    req(counts)
    counts$sample <- factor(counts$sample, levels = counts$sample[order(-counts$variants)])
    ggplot(counts, aes(x = sample, y = variants)) +
        geom_col(fill = "#d95f02") +
        labs(x = NULL, y = "Non-reference genotypes") +
        plot_theme() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
})

output$variant_type_plot <- renderPlot(variant_type_plot())
output$location_plot <- renderPlot(location_plot())
output$consequence_plot <- renderPlot(consequence_plot())
output$titv_plot <- renderPlot(titv_plot())
output$per_chromosome_plot <- renderPlot(per_chromosome_plot())
output$qual_hist_plot <- renderPlot(qual_hist_plot())
output$dp_hist_plot <- renderPlot(dp_hist_plot())
output$per_sample_plot <- renderPlot(per_sample_plot())

figure_name <- function(suffix) {
    function() paste0(vcf_base_name(my_values$vcf_name %||% "variants"), "_", suffix)
}

register_publication_downloads(output, "download_variant_type", figure_name("variant_types"), variant_type_plot, 7, 5)
register_publication_downloads(output, "download_location", figure_name("locations"), location_plot, 8, 5)
register_publication_downloads(output, "download_consequence", figure_name("consequences"), consequence_plot, 9, 6)
register_publication_downloads(output, "download_titv", figure_name("titv"), titv_plot, 9, 6)
register_publication_downloads(output, "download_per_chromosome", figure_name("per_chromosome"), per_chromosome_plot, 10, 5)
register_publication_downloads(output, "download_qual_hist", figure_name("qual"), qual_hist_plot, 7, 5)
register_publication_downloads(output, "download_dp_hist", figure_name("depth"), dp_hist_plot, 7, 5)
register_publication_downloads(output, "download_per_sample", figure_name("per_sample"), per_sample_plot, 10, 6)
