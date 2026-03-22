tabItem(
    tabName = "goplotsTab",
    h2(strong("GO Plots")),
    uiOutput("goPlots_selectionBanner"),
    fluidRow(
        column(
            3,
            wellPanel(
                numericInput("showCategory_go_global", "Number of categories to show", value = 5, min = 1),
                tags$small(class = "text-muted", "Disabled when terms are selected in the enrichGO tab.")
            )
        )
    ),
    tags$div(
        box(
            title = "UpSet Plot (Interactive)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(
                    12,
                    wellPanel(
                        plotly::plotlyOutput("genesInGoTerm", width = "100%", height = "500px"),
                        uiOutput("go_clicked_genes_inline"),
                        tags$hr(),
                        h4(strong("Gene Membership")),
                        dataTableOutput("genesGoMembershipTable")
                    )
                )
            )
        ),
        box(
            title = "Bar Plot", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "barPlot"), type = 8),
                    plot_dl_buttons("barPlot")
                ))
            )
        ),
        box(
            title = "Dot Plot", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "dotPlot"), type = 8),
                    plot_dl_buttons("dotPlot")
                ))
            )
        ),
        box(
            title = "Enrichment Plot Map", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "enrichPlotMap"),
                    plot_dl_buttons("enrichPlotMap")
                ))
            )
        ),
        box(
            title = "Category Netplot", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "goInducedGraph"),
                    plot_dl_buttons("goInducedGraph")
                ))
            )
        ),
        box(
            title = "Enriched GO induced graph (cnetplot)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "cnetplot", width = "100%", height = "400px"),
                    plot_dl_buttons("cnetplot")
                ))
            )
        ),
        box(
            title = "Tree Plot (hierarchical clustering of GO terms)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(
                    3,
                    wellPanel(
                        numericInput("nCluster_tree", "Number of clusters", value = 5, min = 2, max = 20)
                    )
                ),
                column(
                    9,
                    wellPanel(
                        withSpinner(plotOutput(outputId = "treePlot", height = "500px"), type = 8),
                        plot_dl_buttons("treePlot")
                    )
                )
            )
        ),
        box(
            title = "Heat Plot (gene-term associations)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "heatPlot", height = "400px"), type = 8),
                    plot_dl_buttons("heatPlot")
                ))
            )
        ),
        style = "display:table;"
    )
)
