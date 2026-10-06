tabItem(
    tabName = "plots_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Variant types", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE,
                withSpinner(plotOutput("variant_type_plot", height = "320px")),
                publication_downloads("download_variant_type")
            ),
            box(
                title = "Predicted effect (most severe per variant)", solidHeader = TRUE, status = "primary",
                width = 12, collapsible = TRUE,
                withSpinner(plotOutput("consequence_plot", height = "360px")),
                publication_downloads("download_consequence")
            ),
            box(
                title = "Variants per chromosome", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE,
                withSpinner(plotOutput("per_chromosome_plot", height = "320px")),
                publication_downloads("download_per_chromosome")
            ),
            box(
                title = "QUAL distribution", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE,
                withSpinner(plotOutput("qual_hist_plot", height = "300px")),
                publication_downloads("download_qual_hist")
            )
        ),
        column(
            6,
            box(
                title = "Genomic location (all transcripts)", solidHeader = TRUE, status = "primary",
                width = 12, collapsible = TRUE,
                withSpinner(plotOutput("location_plot", height = "320px")),
                publication_downloads("download_location")
            ),
            box(
                title = "Substitution spectrum and Ti/Tv", solidHeader = TRUE, status = "primary",
                width = 12, collapsible = TRUE,
                withSpinner(plotOutput("titv_plot", height = "360px")),
                publication_downloads("download_titv")
            ),
            box(
                title = "INFO/DP distribution", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE,
                withSpinner(plotOutput("dp_hist_plot", height = "300px")),
                publication_downloads("download_dp_hist")
            ),
            conditionalPanel(
                condition = "output.has_samples === true",
                box(
                    title = "Non-reference genotypes per sample", solidHeader = TRUE, status = "primary",
                    width = 12, collapsible = TRUE,
                    withSpinner(plotOutput("per_sample_plot", height = "320px")),
                    publication_downloads("download_per_sample")
                )
            )
        )
    )
)
