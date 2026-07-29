tabItem(
    tabName = "keggPlotsTab",
    h2(strong("KEGG GSEA Plots")),
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
                    "Disabled when pathways are selected in the gseKEGG table."
                )
            )
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        conditionalPanel(
            "output.gseKEGGAvailable",
            analysis_plot_panel(
                "Interactive gene membership",
                "Intersection sizes and genes for the displayed KEGG pathways.",
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
            "Dot plot",
            "Normalized enrichment scores and significance for KEGG pathways.",
            withSpinner(
                plotOutput("dotPlot_kegg", height = "440px"),
                type = 8
            ),
            plot_dl_buttons("dotPlot_kegg"),
            width = 6
        ),
        analysis_plot_panel(
            "Ridge plot",
            "Distribution of ranked gene statistics across enriched pathways.",
            withSpinner(
                plotOutput("ridgePlot_kegg", height = "440px"),
                type = 8
            ),
            plot_dl_buttons("ridgePlot_kegg"),
            width = 6
        )
    ),
    fluidRow(
        class = "analysis-plot-row",
        analysis_plot_panel(
            "Enrichment map",
            "Similarity network connecting KEGG pathways with shared genes.",
            plotOutput("gsePlotMap_kegg", height = "620px"),
            plot_dl_buttons("gsePlotMap_kegg")
        ),
        analysis_plot_panel(
            "Category-gene network",
            "Network linking enriched pathways to their leading-edge genes.",
            plotOutput("cnetplot_kegg", width = "100%", height = "620px"),
            plot_dl_buttons("cnetplot_kegg")
        ),
        analysis_plot_panel(
            "KEGG pathway tree",
            "Hierarchical clustering of enriched KEGG pathways.",
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
            "Ranked gene statistics across selected KEGG pathways.",
            withSpinner(
                plotOutput("heatPlot_kegg", height = "520px"),
                type = 8
            ),
            plot_dl_buttons("heatPlot_kegg")
        ),
        analysis_plot_panel(
            "Running enrichment score",
            "Running-score curve and ranked-list position for the selected KEGG pathway.",
            numericInput(
                "geneSetId_gsea_kegg",
                "Pathway position",
                value = 1,
                min = 1
            ),
            plotOutput("gseaplot_kegg", width = "100%", height = "520px"),
            plot_dl_buttons("gseaplot_kegg")
        )
    )
)
