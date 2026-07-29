tabItem(
    tabName = "nucleosomepositioning_tab",
    fluidRow(
        column(
            12,
            box(
                title = "Core ATAC-seq QC",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                collapsible = TRUE,
                id = "selectSample1",
                fluidRow(
                    column(
                        6,
                        selectInput(
                            "sel_sample_for_npositioning",
                            "Sample",
                            choices = NULL,
                            selected = NULL
                        )
                    ),
                    column(
                        2,
                        selectInput(
                            "sel_chromosome",
                            "Chromosome",
                            choices = NULL,
                            selected = NULL
                        )
                    ),
                    column(
                        12,
                        actionButton(
                            "run_qc",
                            "Run core QC",
                            class = "btn btn-success",
                            style = "width:100%;height:54px;"
                        )
                    )
                )
            ),
            textOutput("empty_txt_output"),
            conditionalPanel(
                condition = "output.task_done",
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        4,
                        analysis_panel(
                            "Promoter/transcript body score",
                            "Promoter enrichment relative to transcript-body coverage.",
                            withSpinner(plotOutput("plot_pt_score", height = "400px")),
                            publication_downloads("download_pt_score")
                        )
                    ),
                    column(
                        4,
                        analysis_panel(
                            "Nucleosome-free region score",
                            "Accessibility around transcription start sites.",
                            withSpinner(plotOutput("plot_nfr_score", height = "400px")),
                            publication_downloads("download_nfr_score")
                        )
                    ),
                    column(
                        4,
                        analysis_panel(
                            "TSS enrichment score",
                            "Aggregate cut-site enrichment centered on transcription start sites.",
                            withSpinner(plotOutput("plot_tssre_score", height = "400px")),
                            publication_downloads("download_tss_enrichment")
                        )
                    )
                )
            )
        )
    )
)
