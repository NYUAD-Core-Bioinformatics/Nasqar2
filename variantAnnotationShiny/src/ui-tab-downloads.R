tabItem(
    tabName = "downloads_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Annotation tables", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE,
                p("Comma-separated tables of the annotated, filtered variants."),
                downloadButton("download_annot_per_variant", "Per-variant table (CSV)", class = "btn btn-primary btn-block"),
                br(),
                downloadButton("download_annot_per_transcript", "Per-variant, per-transcript table (CSV)", class = "btn btn-primary btn-block"),
                br(),
                downloadButton("download_gene_list", "Genes carrying variants (CSV)", class = "btn btn-primary btn-block"),
                br(),
                downloadButton("download_filter_stats", "Filter summary (CSV)", class = "btn btn-default btn-block")
            )
        ),
        column(
            6,
            box(
                title = "Filtered VCF", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE,
                p(
                    "The filtered records as a bgzip-compressed VCF. It carries the columns that were read",
                    "for annotation: the fixed fields, INFO/DP and, for files with at most 50 samples, GT."
                ),
                downloadButton("download_filtered_vcf", "Filtered VCF (.vcf.bgz)", class = "btn btn-primary btn-block"),
                hr(),
                textOutput("downloads_project_path")
            )
        )
    )
)
