tabItem(
    tabName = "goplotsTab",
    h2(strong("GO Plots")),
    uiOutput("goPlots_selectionBanner"),
    fluidRow(
        column(
            4,
            wellPanel(
                numericInput(
                    "showCategory_go_global",
                    "Number of categories to show",
                    value = 5,
                    min = 1
                ),
                tags$small(
                    class = "text-muted",
                    "Disabled when terms are selected in the enrichGO table."
                )
            )
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Interactive gene membership",
            "Intersection sizes and member genes for the displayed GO terms.",
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
            "Bar plot",
            "Enriched GO terms ranked by the selected significance measure.",
            withSpinner(
                plotOutput("barPlot", height = "420px"),
                type = 8
            ),
            plot_dl_buttons("barPlot"),
            width = 6
        ),
        analysis_plot_panel(
            "Dot plot",
            "GO term enrichment, gene ratio, and significance in a compact comparison.",
            withSpinner(
                plotOutput("dotPlot", height = "420px"),
                type = 8
            ),
            plot_dl_buttons("dotPlot"),
            width = 6
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Enrichment map",
            "Similarity network connecting GO terms with overlapping gene sets.",
            plotOutput("enrichPlotMap", height = "620px"),
            plot_dl_buttons("enrichPlotMap")
        ),
        analysis_plot_panel(
            "GO induced graph",
            "Ontology graph showing relationships among selected enriched terms.",
            plotOutput("goInducedGraph", height = "620px"),
            plot_dl_buttons("goInducedGraph")
        ),
        analysis_plot_panel(
            "Category-gene network",
            "Network linking enriched GO terms to the genes contributing to each term.",
            plotOutput("cnetplot", width = "100%", height = "620px"),
            plot_dl_buttons("cnetplot")
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "GO term tree",
            "Hierarchical clustering of enriched GO terms by semantic similarity.",
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
            "Gene-level fold changes across the selected enriched GO terms.",
            withSpinner(
                plotOutput("heatPlot", height = "520px"),
                type = 8
            ),
            plot_dl_buttons("heatPlot")
        )
    )
)
