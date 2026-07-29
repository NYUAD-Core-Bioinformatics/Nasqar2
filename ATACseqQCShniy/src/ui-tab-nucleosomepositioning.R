tabItem(
    tabName = "nucleosomepositioning_tab",
    fluidRow(
        column(
            12,
            box(
                title = "Nucleosome positioning",
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
                        4,
                        textInput(
                            "motif_value",
                            "Motif for optional footprinting",
                            value = "CTCF"
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
                    column(
                        6,
                        wellPanel(
                            h3("Promoter/transcript body score"),
                            withSpinner(plotOutput("plot_pt_score"))
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("Nucleosome-free region score"),
                            withSpinner(plotOutput("plot_nfr_score"))
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("TSS enrichment score"),
                            withSpinner(plotOutput("plot_tssre_score"))
                        )
                    )
                ),
                fluidRow(
                    column(
                        6,
                        actionButton(
                            "run_nucleosome_plots",
                            "Run nucleosome plots",
                            class = "btn btn-primary",
                            style = "width:100%;height:50px;"
                        )
                    ),
                    column(
                        6,
                        actionButton(
                            "run_footprint_plots",
                            "Run footprint plots",
                            class = "btn btn-primary",
                            style = "width:100%;height:50px;"
                        )
                    )
                )
            ),
            conditionalPanel(
                condition = "output.nucleosome_task_done",
                fluidRow(
                    column(
                        6,
                        wellPanel(
                            h3("Cumulative tag allocation"),
                            withSpinner(plotOutput("plotCumulativePercentage"))
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("TSS-aligned heatmap"),
                            withSpinner(plotOutput("plot_tss_featureAlignedHeatmap"))
                        )
                    ),
                    column(
                        12,
                        wellPanel(
                            h3("Nucleosome-free and nucleosome-bound distributions"),
                            withSpinner(plotOutput("plot_signals"))
                        )
                    ),
                    column(
                        10,
                        tags$div(
                            class = "BoxArea2",
                            withSpinner(dataTableOutput("bamfilesTable"))
                        ),
                        downloadButton(
                            outputId = "download_bamfiles_btn",
                            label = "Download selected BAM files",
                            icon = icon("file-download")
                        )
                    )
                )
            ),
            conditionalPanel(
                condition = "output.footprint_task_done",
                fluidRow(
                    column(
                        6,
                        wellPanel(
                            h3("DNA-binding factor footprint"),
                            withSpinner(
                                imageOutput(
                                    "plot_Footprints",
                                    width = "100%",
                                    height = "520px"
                                )
                            )
                        )
                    ),
                    column(
                        6,
                        wellPanel(
                            h3("Binding-site heatmap"),
                            withSpinner(
                                plotOutput("plot_binding_sites_featureAlignedHeatmap")
                            )
                        )
                    ),
                    column(
                        7,
                        wellPanel(
                            h3("Fragment midpoint versus length"),
                            withSpinner(
                                imageOutput(
                                    "plot_vp",
                                    width = "100%",
                                    height = "520px"
                                )
                            )
                        )
                    ),
                    column(
                        7,
                        wellPanel(
                            h3("Distance of potential nucleosome dyad"),
                            withSpinner(plotOutput("plot_distanceDyad"))
                        )
                    )
                )
            )
        )
    )
)
