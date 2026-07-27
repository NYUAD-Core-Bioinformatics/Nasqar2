tabItem(
    tabName = "keggPlotsTab",
    h2(strong("KEGG Plots")),
    uiOutput("keggPlots_selectionBanner"),
    fluidRow(
        column(
            3,
            wellPanel(
                numericInput("showCategory_kegg_global", "Number of categories to show", value = 5, min = 1),
                tags$small(class = "text-muted", "Disabled when pathways are selected in the gseKEGG tab.")
            )
        )
    ),
    tags$div(
        conditionalPanel(
            "output.gseKEGGAvailable",
            box(
                title = "UpSet Plot (Interactive)", solidHeader = T, status = "danger", width = 12, collapsible = T,
                fluidRow(
                    column(
                        12,
                        wellPanel(
                            plotly::plotlyOutput("genesInKeggPathway", width = "100%", height = "500px"),
                            uiOutput("kegg_clicked_genes_inline"),
                            tags$hr(),
                            h4(strong("Gene Membership")),
                            dataTableOutput("genesKeggMembershipTable")
                        )
                    )
                )
            )
        ),
        box(
            title = "Dot Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "dotplot_kegg",
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "dotPlot_kegg"), type = 8),
                    plot_dl_buttons("dotPlot_kegg")
                ))
            )
        ),
        box(
            title = "Enrichment Plot Map", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "gsePlotMap_kegg",
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "gsePlotMap_kegg"),
                    plot_dl_buttons("gsePlotMap_kegg")
                ))
            )
        ),
        box(
            title = "Category Netplot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "cnetplot_kegg",
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "cnetplot_kegg", width = "100%", height = "400px"),
                    plot_dl_buttons("cnetplot_kegg")
                ))
            )
        ),
        box(
            title = "Tree Plot (hierarchical clustering of KEGG pathways)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(3, wellPanel(
                    numericInput("nCluster_tree_kegg", "Number of clusters", value = 5, min = 2, max = 20)
                )),
                column(9, wellPanel(
                    withSpinner(plotOutput(outputId = "treePlot_kegg", height = "500px"), type = 8),
                    plot_dl_buttons("treePlot_kegg")
                ))
            )
        ),
        box(
            title = "Heat Plot (gene-pathway associations)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "heatPlot_kegg", height = "400px"), type = 8),
                    plot_dl_buttons("heatPlot_kegg")
                ))
            )
        ),
        box(
            title = "Ridge Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "ridgeplot_kegg",
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "ridgePlot_kegg"), type = 8),
                    plot_dl_buttons("ridgePlot_kegg")
                ))
            )
        ),
        box(
            title = "GSEA Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "gseaplot_kegg",
            fluidRow(
                column(
                    3,
                    wellPanel(
                        numericInput("geneSetId_gsea_kegg", "Gene Set ID (position in selected pathways)", value = 1, min = 1)
                    )
                ),
                column(
                    9,
                    wellPanel(
                        plotOutput(outputId = "gseaplot_kegg", width = "100%", height = "400px"),
                        plot_dl_buttons("gseaplot_kegg")
                    )
                )
            )
        ),
        style = "display:table;"
    )
)
