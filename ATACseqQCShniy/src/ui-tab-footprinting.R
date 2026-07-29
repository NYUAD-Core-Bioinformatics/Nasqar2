tabItem(
    tabName = "footprinting_tab",
    fluidRow(
        column(
            12,
            box(
                title = "Transcription-factor footprinting",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        8,
                        textInput(
                            "motif_value",
                            "Motif",
                            value = "CTCF"
                        )
                    ),
                    column(
                        4,
                        tags$div(
                            class = "analysis-context",
                            tags$strong("Core result: "),
                            textOutput("footprint_context", inline = TRUE)
                        )
                    ),
                    column(
                        12,
                        actionButton(
                            "run_footprint_plots",
                            "Run footprint analysis",
                            class = "btn btn-primary",
                            style = "width:100%;height:50px;"
                        )
                    )
                )
            ),
            conditionalPanel(
                condition = "output.footprint_task_done",
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        12,
                        analysis_panel(
                            "DNA-binding factor footprint",
                            paste(
                                "Forward- and reverse-strand cut-site probability",
                                "around the selected motif."
                            ),
                            withSpinner(
                                tags$div(
                                    class = paste(
                                        "analysis-image-frame",
                                        "analysis-image-frame--wide"
                                    ),
                                    imageOutput(
                                        "plot_Footprints",
                                        width = "100%",
                                        height = "100%"
                                    )
                                )
                            ),
                            publication_downloads("download_footprint", vector = FALSE)
                        )
                    )
                ),
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        6,
                        analysis_panel(
                            "Binding-site heatmap",
                            paste(
                                "Tn5 cut signal aligned to motif binding sites",
                                "and ordered by score."
                            ),
                            withSpinner(
                                plotOutput(
                                    "plot_binding_sites_featureAlignedHeatmap",
                                    height = "520px"
                                )
                            ),
                            publication_downloads("download_binding_heatmap")
                        )
                    ),
                    column(
                        6,
                        analysis_panel(
                            "Fragment midpoint versus length",
                            paste(
                                "Fragment length plotted against midpoint",
                                "distance from the motif center."
                            ),
                            withSpinner(
                                tags$div(
                                    class = paste(
                                        "analysis-image-frame",
                                        "analysis-image-frame--standard"
                                    ),
                                    imageOutput(
                                        "plot_vp",
                                        width = "100%",
                                        height = "100%"
                                    )
                                )
                            ),
                            publication_downloads("download_vplot", vector = FALSE)
                        )
                    )
                ),
                fluidRow(
                    class = "analysis-plot-row",
                    column(
                        12,
                        analysis_panel(
                            "Distance of potential nucleosome dyad",
                            "Estimated dyad position relative to the selected motif.",
                            conditionalPanel(
                                condition = "output.dyad_available",
                                withSpinner(
                                    plotOutput(
                                        "plot_distanceDyad",
                                        height = "420px"
                                    )
                                ),
                                publication_downloads("download_dyad_distance")
                            ),
                            conditionalPanel(
                                condition = "!output.dyad_available",
                                tags$div(
                                    class = "analysis-no-result",
                                    icon("info-circle"),
                                    tags$p(
                                        paste(
                                            "No nucleosome dyad position could be",
                                            "estimated for this sample, chromosome,",
                                            "and motif."
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
