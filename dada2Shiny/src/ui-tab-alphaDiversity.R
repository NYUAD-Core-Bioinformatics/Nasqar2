tabItem(
    tabName = "alphaDiversityTab",
    fluidRow(
        column(
            6,
            box(
                title = "Upload QIIME Metadata File", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,

                # File input for uploading sample meta data
                fileInput("sampleData", "Choose QIIME Metadata File",
                    accept = c(".csv", ".tsv", ".txt")
                ),
                uiOutput("alphaDiversityMappings"),
                actionButton(
                    "runAlphaDiversity",
                    "Run diversity analysis",
                    class = "btn-info btn-success",
                    style = "width: 100%"
                )
            ),
        ),
        column(
            12,
            conditionalPanel(
                condition = "output.divergen_available",
                box(
                    title = "Alpha diversity",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    withSpinner(plotOutput("plotAlphaDiversity", height = "420px")),
                    tags$p(
                        class = "text-muted",
                        "Within-sample Shannon and Simpson diversity grouped by the selected metadata variables."
                    ),
                    publication_downloads("download_alpha_diversity")
                )
            )
        ),
        column(
            12,
            conditionalPanel(
                condition = "output.divergen_available",
                box(
                    title = "Bray-Curtis ordination",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    withSpinner(plotOutput("plotOrdination", height = "520px")),
                    tags$p(
                        class = "text-muted",
                        "NMDS representation of between-sample Bray-Curtis dissimilarity."
                    ),
                    publication_downloads("download_ordination")
                )
            )
            # withSpinner(plotOutput("plotOrdination"))
        ),
        column(
            12,
            conditionalPanel(
                condition = "output.divergen_available",
                box(
                    title = "Taxonomic composition",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    withSpinner(plotOutput("plotBar", height = "520px")),
                    tags$p(
                        class = "text-muted",
                        "Relative abundance of the 20 most abundant ASVs at the selected taxonomy rank."
                    ),
                    publication_downloads("download_composition")
                )
            )
            #     withSpinner(plotOutput("plotBar"))
        )
    )
)
