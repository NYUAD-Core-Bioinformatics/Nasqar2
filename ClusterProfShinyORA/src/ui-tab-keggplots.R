tabItem(
    tabName = "keggPlotsTab",
    h2(strong("KEGG Plots")),
    uiOutput("keggPlots_selectionBanner"),
    fluidRow(
        column(
            4,
            wellPanel(
                numericInput(
                    "showCategory_kegg_global",
                    "Number of pathways to show",
                    value = 5,
                    min = 1
                ),
                tags$small(
                    class = "text-muted",
                    "Disabled when pathways are selected in the enrichKEGG table."
                )
            )
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        conditionalPanel(
            "output.enrichKEGGAvailable",
            analysis_plot_panel(
                "Interactive gene membership",
                "Intersection sizes and member genes for the displayed KEGG pathways.",
                plotly::plotlyOutput(
                    "genesInKeggPathway",
                    width = "100%",
                    height = "520px"
                ),
                uiOutput("kegg_clicked_genes_inline"),
                tags$hr(),
                h4(strong("Gene membership")),
                dataTableOutput("genesKeggMembershipTable")
            )
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Bar plot",
            "Enriched KEGG pathways ranked by the selected significance measure.",
            withSpinner(
                plotOutput("barPlot_kegg", height = "420px"),
                type = 8
            ),
            plot_dl_buttons("barPlot_kegg"),
            width = 6
        ),
        analysis_plot_panel(
            "Dot plot",
            "KEGG pathway enrichment, gene ratio, and significance.",
            withSpinner(
                plotOutput("dotPlot_kegg", height = "420px"),
                type = 8
            ),
            plot_dl_buttons("dotPlot_kegg"),
            width = 6
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Enrichment map",
            "Similarity network connecting KEGG pathways with overlapping genes.",
            plotOutput("enrichPlotMap_kegg", height = "620px"),
            plot_dl_buttons("enrichPlotMap_kegg")
        ),
        analysis_plot_panel(
            "Category-gene network",
            "Network linking enriched KEGG pathways to contributing genes.",
            plotOutput("cnetplot_kegg", width = "100%", height = "620px"),
            plot_dl_buttons("cnetplot_kegg")
        ),
        analysis_plot_panel(
            "KEGG pathway tree",
            "Hierarchical clustering of enriched pathways by gene-set similarity.",
            numericInput(
                "nCluster_tree_kegg",
                "Number of clusters",
                value = 5,
                min = 2,
                max = 20
            ),
            withSpinner(
                plotOutput("treePlot_kegg", height = "620px"),
                type = 8
            ),
            plot_dl_buttons("treePlot_kegg")
        ),
        analysis_plot_panel(
            "Gene-pathway heat plot",
            "Gene-level fold changes across selected KEGG pathways.",
            withSpinner(
                plotOutput("heatPlot_kegg", height = "520px"),
                type = 8
            ),
            plot_dl_buttons("heatPlot_kegg")
        )
    )
)
