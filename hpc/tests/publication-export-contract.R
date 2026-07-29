read_sources <- function(directory, pattern) {
    files <- list.files(directory, pattern = pattern, full.names = TRUE)
    paste(
        vapply(
            files,
            function(file) paste(readLines(file, warn = FALSE), collapse = "\n"),
            FUN.VALUE = character(1)
        ),
        collapse = "\n"
    )
}

assert_ids <- function(label, ui_text, server_text, ids) {
    missing_ui <- ids[!vapply(
        ids,
        function(id) grepl(id, ui_text, fixed = TRUE),
        logical(1)
    )]
    missing_server <- ids[!vapply(
        ids,
        function(id) grepl(id, server_text, fixed = TRUE),
        logical(1)
    )]
    if (length(missing_ui) || length(missing_server)) {
        stop(
            label,
            " publication export mismatch. UI: ",
            paste(missing_ui, collapse = ", "),
            "; server: ",
            paste(missing_server, collapse = ", "),
            call. = FALSE
        )
    }
}

dada_ui <- read_sources("dada2Shiny/src", "^ui-tab-.*\\.R$")
dada_server <- read_sources("dada2Shiny/src", "^server-.*\\.R$")
assert_ids(
    "DADA2",
    dada_ui,
    dada_server,
    c(
        "download_quality_forward", "download_quality_reverse",
        "download_error_forward", "download_error_reverse",
        "download_fragments_summary",
        "download_sequence_distribution", "download_alpha_diversity",
        "download_ordination", "download_composition"
    )
)

atac_ui <- read_sources("ATACseqQCShniy/src", "^ui-tab-.*\\.R$")
atac_server <- read_sources("ATACseqQCShniy/src", "^server-.*\\.R$")
assert_ids(
    "ATACseqQC",
    atac_ui,
    atac_server,
    c(
        "download_replicate_heatmap", "download_library_complexity",
        "download_fragment_size", "download_pt_score", "download_nfr_score",
        "download_tss_enrichment", "download_cumulative_tags",
        "download_tss_heatmap", "download_nucleosome_signals",
        "download_footprint", "download_binding_heatmap", "download_vplot",
        "download_dyad_distance"
    )
)

deseq_ui <- read_sources("deseq2shiny/src", "^ui-tab-.*\\.R$")
deseq_server <- read_sources("deseq2shiny/src", "^server-.*\\.R$")
assert_ids(
    "DESeq2Shiny",
    deseq_ui,
    deseq_server,
    c(
        "download_ma_plot", "download_volcano_plot", "download_venn_plot",
        "download_boxplot", "downloadHighResHeatmap",
        "download_vst_distance", "download_vst_pca",
        "download_rlog_distance", "download_rlog_pca",
        "download_sva_scatter", "download_sva_pca"
    )
)

for (application in c("ClusterProfShinyORA", "ClusterProfShinyGSEA")) {
    helper <- paste(readLines(
        file.path(application, "src", "plot-helpers.R"),
        warn = FALSE
    ), collapse = "\n")
    if (!grepl("res    = 300", helper, fixed = TRUE) ||
        !grepl("toImageButtonOptions", helper, fixed = TRUE) ||
        grepl("error = function(e) NULL", helper, fixed = TRUE)) {
        stop(application, paste(
            "must export static PNG at 300 DPI, configure high-resolution",
            "interactive downloads, and propagate errors."
        ),
             call. = FALSE)
    }

    server <- read_sources(file.path(application, "src"), "^server-.*\\.R$")
    ui <- read_sources(file.path(application, "src"), "^ui-tab-.*\\.R$")
    handler_ids <- unique(unlist(regmatches(
        server,
        gregexpr(
            '(?<=make_downloader\\(output, ")[^"]+',
            server,
            perl = TRUE
        )
    )))
    for (id in handler_ids) {
        if (!grepl(paste0('plot_dl_buttons("', id, '")'), ui, fixed = TRUE)) {
            stop(application, " is missing download controls for ", id,
                 call. = FALSE)
        }
    }
}

gene_merger <- paste(readLines("GeneCountMerger/src/app.R", warn = FALSE),
                     collapse = "\n")
stopifnot(grepl("downloadHandler", gene_merger, fixed = TRUE))

cat("Publication export contracts passed.\n")
