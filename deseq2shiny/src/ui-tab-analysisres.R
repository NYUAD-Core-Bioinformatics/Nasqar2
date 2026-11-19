tabItem(
    tabName = "resultsTab",
    fluidRow(
        column(
            12,
            box(
                title = "1. Choose a comparison", solidHeader = T, status = "primary", width = 12,
                conditionalPanel(
                    "output.noReplicates",
                    p('Experiments without replicates do not allow for estimation of the dispersion of counts around the expected value for each group, which is critical for differential expression analysis. If an experimental design is supplied which does not contain the necessary degrees of freedom for differential analysis, DESeq will provide a message to the user and follow the strategy outlined in Anders and Huber (2010) under the section "Working without replicates", wherein all the samples are considered as replicates of a single group for the estimation of dispersion. As noted in the reference above: "Some overestimation of the variance may be expected, which will make that approach conservative." Furthermore, "while one may not want to draw strong conclusions from such an analysis, it may still be useful for exploration and hypothesis generation."')
                ),
                conditionalPanel(
                    "!output.noReplicates",
                    radioButtons("resultNameOrFactor", "Compare by", choices = c("Factors", "Result Names"), inline = TRUE),
                    conditionalPanel(
                        "input.resultNameOrFactor == 'Result Names'",
                        selectizeInput("resultNamesInput", "Result names (one or two)", choices = c("Intercept"), multiple = T, width = "100%")
                    ),
                    conditionalPanel(
                        "input.resultNameOrFactor == 'Factors'",
                        fluidRow(
                            column(4, selectizeInput("factorNameInput", "Factor", choices = c("Intercept"), width = "100%")),
                            column(
                                8,
                                conditionalPanel(
                                    "output.ddsComputed",
                                    fluidRow(
                                        column(5, selectInput("condition1", "Condition 1", choices = NULL, width = "100%")),
                                        column(2, div(style = "text-align:center; margin-top:30px; color:#777;", strong("vs"))),
                                        column(5, selectInput("condition2", "Condition 2 (baseline)", choices = NULL, width = "100%"))
                                    )
                                ),
                                conditionalPanel(
                                    "!output.ddsComputed",
                                    div(class = "alert alert-info", style = "margin-top: 22px;",
                                        "The conditions can be chosen once DESeq2 has run.")
                                )
                            )
                        ),
                        helpText("Fold changes are reported as condition 1 relative to condition 2.")
                    )
                ),
                actionButton("getDiffResVs", "Get results", icon = icon("play"), class = "btn btn-primary",
                             style = "color:#fff; min-width:180px;")
            ),
            conditionalPanel(
                "output.comparisonComputed",
                fluidRow(
                    column(
                        8,
                        box(
                            title = "2. Results", solidHeader = T, status = "primary", width = 12,
                            p(strong("Comparison: "), textOutput("factorsStr", inline = TRUE)),
                            div(
                                style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:6px;",
                                actionButton("do", "Save comparison", icon = icon("bookmark"), class = "btn btn-primary", style = "color:#fff;"),
                                downloadButton("downloadVsCsv", "Download table (.csv)", class = "btn btn-default")
                            ),
                            helpText("Saving keeps this comparison for the Volcano Plot and UpSet Plot tabs."),
                            withSpinner(dataTableOutput("comparisonData"))
                        )
                    ),
                    column(
                        4,
                        box(
                            title = "Saved comparisons", solidHeader = T, status = "primary", width = 12,
                            uiOutput("saved_comparisons_ui")
                        ),
                        htmlOutput("analysisRes_enrichGo")
                    )
                ),
                fluidRow(
                    column(
                        8,
                        box(
                            title = "MA plot", solidHeader = T, status = "primary", width = 12,
                            withSpinner(plotOutput(outputId = "maPlot"))
                        )
                    ),
                    column(
                        4,
                        box(
                            title = "MA plot settings", solidHeader = T, status = "primary", width = 12,
                            sliderInput("alpha", "Adjusted p-value threshold", min = 0, max = 1, value = 0.1, step = 0.001),
                            numericInput("ylim", label = "Y axis range (absolute value)", min = 1, max = 10, value = 2),
                            hr(),
                            fluidRow(
                                column(6, selectInput("ma_plot_format", "Download as",
                                    choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"), selected = "pdf")),
                                column(6, numericInput("ma_plot_dpi", "DPI", value = 300, min = 72, max = 600, step = 50))
                            ),
                            fluidRow(
                                column(6, numericInput("ma_plot_width", "Width (in)", value = 10, min = 3, max = 20, step = 1)),
                                column(6, numericInput("ma_plot_height", "Height (in)", value = 8, min = 3, max = 20, step = 1))
                            ),
                            downloadButton("download_ma_plot", "Download plot", class = "btn-success", style = "width:100%; color:#fff;")
                        )
                    )
                )
            )
        )
    )
)
