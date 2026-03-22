tabItem(
    tabName = "keggPlotsTab",
    h2(strong("KEGG Plots")),
    uiOutput("keggPlots_selectionBanner"),
    fluidRow(
        column(
            3,
            wellPanel(
                numericInput("showCategory_kegg_global", tags$span(
                    "Number of categories to show ",
                    tags$span(class = "help-tip",
                        icon("question-circle"),
                        tags$span(class = "help-tip-content",
                            "Used when no rows are selected in the enrichKEGG tab. Shows top N pathways by p.adjust; q-value cutoff is still applied by the plot functions. When rows are manually selected this is disabled — q-value cutoff is bypassed so all selected pathways are plotted (p-value is already enforced by the results table)."
                        )
                    )
                ), value = 5, min = 1),
                tags$small(class = "text-muted", "Disabled when pathways are selected in the enrichKEGG tab.")
            )
        )
    ),
    tags$div(
        conditionalPanel(
            "output.enrichKEGGAvailable",
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
            title = "Bar Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "barplot_kegg",
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "barPlot_kegg"), type = 8),
                    plot_dl_buttons("barPlot_kegg")
                ))
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
            title = "Enrichment Plot Map", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "enrichPlotMap_kegg_box",
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "enrichPlotMap_kegg"),
                    plot_dl_buttons("enrichPlotMap_kegg")
                ))
            )
        ),
        box(
            title = "Category Netplot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "cnetplot_kegg_box",
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
                column(
                    3,
                    wellPanel(
                        numericInput("nCluster_tree_kegg", "Number of clusters", value = 5, min = 2, max = 20)
                    )
                ),
                column(
                    9,
                    wellPanel(
                        withSpinner(plotOutput(outputId = "treePlot_kegg", height = "500px"), type = 8),
                        plot_dl_buttons("treePlot_kegg")
                    )
                )
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
        style = "display:table;"
    )
)
