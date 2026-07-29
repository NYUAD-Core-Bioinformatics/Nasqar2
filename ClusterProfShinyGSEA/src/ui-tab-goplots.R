tabItem(
    tabName = "goplotsTab",
    h2(strong("GO GSEA Plots")),
    uiOutput("goPlots_selectionBanner"),
    fluidRow(
        column(
            4,
            wellPanel(
                numericInput(
                    "showCategory_go_global",
                    "Number of gene sets to show",
                    value = 5,
                    min = 1
                ),
                tags$small(
                    class = "text-muted",
                    "Disabled when terms are selected in the gseGO table."
                )
            )
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Interactive gene membership",
            "Intersection sizes and member genes for the displayed GO gene sets.",
            plotly::plotlyOutput(
                "genesInGoTerm",
                width = "100%",
                height = "520px"
            ),
            uiOutput("go_clicked_genes_inline"),
            tags$hr(),
            h4(strong("Gene membership")),
            dataTableOutput("genesGoMembershipTable")
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Dot plot",
            "Normalized enrichment scores and significance for GO gene sets.",
            withSpinner(
                plotOutput("dotPlot", height = "440px"),
                type = 8
            ),
            plot_dl_buttons("dotPlot"),
            width = 6
        ),
        analysis_plot_panel(
            "Ridge plot",
            "Distribution of ranked gene statistics across enriched GO gene sets.",
            withSpinner(
                plotOutput("ridgePlot", height = "440px"),
                type = 8
            ),
            plot_dl_buttons("ridgePlot"),
            width = 6
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Enrichment map",
            "Similarity network connecting GO gene sets with shared members.",
            plotOutput("gsePlotMap", height = "620px"),
            plot_dl_buttons("gsePlotMap")
        ),
        analysis_plot_panel(
            "Category-gene network",
            "Network linking enriched GO gene sets to their leading-edge genes.",
            plotOutput("cnetplot", height = "620px"),
            plot_dl_buttons("cnetplot")
        ),
        analysis_plot_panel(
            "GO term tree",
            "Hierarchical clustering of enriched GO gene sets.",
            numericInput(
                "nCluster_tree",
                "Number of clusters",
                value = 5,
                min = 2,
                max = 20
            ),
            withSpinner(
                plotOutput("treePlot", height = "620px"),
                type = 8
            ),
            plot_dl_buttons("treePlot")
        ),
        analysis_plot_panel(
            "Gene-term heat plot",
            "Ranked gene statistics across selected enriched GO gene sets.",
            withSpinner(
                plotOutput("heatPlot", height = "520px"),
                type = 8
            ),
            plot_dl_buttons("heatPlot")
        ),
        analysis_plot_panel(
            "Running enrichment score",
            "Running-score curve and ranked-list position for the selected GO gene set.",
            numericInput(
                "geneSetId_gsea",
                "Gene-set position",
                value = 1,
                min = 1
            ),
            plotOutput("gseaplot", width = "100%", height = "520px"),
            plot_dl_buttons("gseaplot")
        )
    )
)
