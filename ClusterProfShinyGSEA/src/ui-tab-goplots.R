tabItem(
    tabName = "goplotsTab",
    h2(strong("GO Plots")),
    uiOutput("goPlots_selectionBanner"),
    fluidRow(
        column(
            3,
            wellPanel(
                numericInput("showCategory_go_global", "Number of categories to show", value = 5, min = 1),
                tags$small(class = "text-muted", "Disabled when terms are selected in the gseGO tab.")
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
            title = "Dot Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "dotplot",
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "dotPlot"), type = 8),
                    plot_dl_buttons("dotPlot")
                ))
            )
        ),
        box(
            title = "Enrichment Plot Map", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "gsePlotMap",
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "gsePlotMap"),
                    plot_dl_buttons("gsePlotMap")
                ))
            )
        ),
        box(
            title = "Category Netplot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "cnetplot",
            fluidRow(
                column(12, wellPanel(
                    plotOutput(outputId = "cnetplot"),
                    plot_dl_buttons("cnetplot")
                ))
            )
        ),
        box(
            title = "Tree Plot (hierarchical clustering of GO terms)", solidHeader = T, status = "danger", width = 12, collapsible = T,
            fluidRow(
                column(3, wellPanel(
                    numericInput("nCluster_tree", "Number of clusters", value = 5, min = 2, max = 20)
                )),
                column(9, wellPanel(
                    withSpinner(plotOutput(outputId = "treePlot", height = "500px"), type = 8),
                    plot_dl_buttons("treePlot")
                ))
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
        box(
            title = "Ridge Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "ridgeplot",
            fluidRow(
                column(12, wellPanel(
                    withSpinner(plotOutput(outputId = "ridgePlot"), type = 8),
                    plot_dl_buttons("ridgePlot")
                ))
            )
        ),
        box(
            title = "GSEA Plot", solidHeader = T, status = "danger", width = 12, collapsible = T, id = "gseaplot",
            fluidRow(
                column(
                    3,
                    wellPanel(
                        numericInput("geneSetId_gsea", "Gene Set ID (position in selected terms)", value = 1, min = 1)
                    )
                ),
                column(
                    9,
                    wellPanel(
                        plotOutput(outputId = "gseaplot", width = "100%", height = "400px"),
                        plot_dl_buttons("gseaplot")
                    )
                )
            )
        ),
        style = "display:table;"
    )
)
