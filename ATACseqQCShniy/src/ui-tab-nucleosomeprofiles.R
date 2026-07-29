tabItem(
    tabName = "nucleosomeprofiles_tab",
    fluidRow(
        column(
            12,
            box(
                title = "Nucleosome profiles",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                tags$div(
                    class = "analysis-context",
                    tags$strong("Core result: "),
                    textOutput("nucleosome_context", inline = TRUE)
                ),
                actionButton(
                    "run_nucleosome_plots",
                    "Run nucleosome analysis",
                    class = "btn btn-primary",
                    style = "width:100%;height:50px;"
                )
            ),
            conditionalPanel(
                condition = "output.nucleosome_task_done",
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        5,
                        analysis_panel(
                            "Cumulative tag allocation",
                            "Cumulative fraction of nucleosome-free and mononucleosome fragments.",
                            withSpinner(
                                plotOutput(
                                    "plotCumulativePercentage",
                                    height = "420px"
                                )
                            ),
                            publication_downloads("download_cumulative_tags")
                        )
                    ),
                    column(
                        7,
                        analysis_panel(
                            "TSS-aligned heatmap",
                            "Fragment-class signal aligned across transcription start sites.",
                            withSpinner(
                                plotOutput(
                                    "plot_tss_featureAlignedHeatmap",
                                    height = "420px"
                                )
                            ),
                            publication_downloads("download_tss_heatmap")
                        )
                    )
                ),
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        12,
                        analysis_panel(
                            "Nucleosome-free and nucleosome-bound distributions",
                            "Normalized fragment-class coverage across the TSS window.",
                            withSpinner(plotOutput("plot_signals", height = "420px")),
                            publication_downloads("download_nucleosome_signals")
                        )
                    )
                ),
                fluidRow(
                    column(
                        12,
                        analysis_panel(
                            "Generated fragment-class BAM files",
                            "Derived alignment files are available for downstream analysis.",
                            withSpinner(dataTableOutput("bamfilesTable"))
                        ),
                        downloadButton(
                            outputId = "download_bamfiles_btn",
                            label = "Download selected BAM files",
                            icon = icon("file-download")
                        )
                    )
                )
            )
        )
    )
)
