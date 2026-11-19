tabItem(
    tabName = "venndiagramTab",
    fluidRow(
        conditionalPanel(
            condition = "output.saved_comparison_count < 2",
            div(
                style = "margin:15px; padding:16px 20px; border:1px solid #cfe2f3; border-left:4px solid #3c8dbc; border-radius:4px; background:#f4f8fb;",
                h4(style = "margin-top:0;", icon("circle-info"), " Save at least two comparisons first"),
                p(style = "margin-bottom:6px;", "The UpSet plot compares the significant genes of saved comparisons. On the DE Results tab, get the results for a comparison and press 'mark it for further analysis' under the table, then repeat for each comparison you want to include."),
                p(style = "margin:0; color:#666;", "Saved so far: ", textOutput("saved_comparison_note_upset", inline = TRUE))
            )
        ),
        conditionalPanel(
        condition = "output.saved_comparison_count >= 2",
        box(
            title = "UpSet plot and set operations", solidHeader = T, status = "primary", width = 12,
            fluidRow(
                column(
                    6,
                    h4(strong("1. Comparisons")),
                    uiOutput("select_venn_ui"),
                    fluidRow(
                        column(7, radioButtons("venn_sig_genes_selection",
                            label = "Genes to include",
                            choices = list("All significant genes" = 1, "Up-regulated genes" = 2, "Down-regulated genes" = 3),
                            selected = 1
                        )),
                        column(5, conditionalPanel(
                            condition = "input.gene_alias=='included'",
                            radioButtons("venn_sel_gene_type",
                                label = "List genes by",
                                c("gene.id" = "gene.id", "gene.name" = "gene.name"),
                                selected = "gene.id"
                            )
                        ))
                    )
                ),
                column(
                    6,
                    h4(strong("2. Thresholds")),
                    radioButtons("venn_threshold_type",
                        label = "Significance given as",
                        choices = list("-log10(padj) slider" = "slider", "padj value" = "direct"),
                        selected = "slider", inline = TRUE
                    ),
                    conditionalPanel(
                        condition = "input.venn_threshold_type == 'slider'",
                        sliderInput("venn_significance_threshold",
                            label = "Significance, -log10(padj)", min = 0,
                            max = 15, value = 3, step = 0.1
                        )
                    ),
                    conditionalPanel(
                        condition = "input.venn_threshold_type == 'direct'",
                        numericInput("venn_direct_padj",
                            label = "padj below",
                            min = 0, max = 1, value = 0.05, step = 0.001
                        )
                    ),
                    sliderInput("venn_log_fold_change_threshold",
                        label = "Absolute log2 fold change above", min = 0,
                        max = 5, value = 3, step = 0.1
                    )
                )
            ),
            actionButton("plotVenn", "Plot UpSet", icon = icon("chart-column"), class = "btn-primary", style = "color:#fff; min-width:180px;")
        )
        )
    ),
    fluidRow(
        hr(),
        conditionalPanel(
            condition = "output.panelStatus && (input.select_avo_de_venn_files || []).length > 1",
            tags$div(
                class = "BoxArea2",
                column(
                    12,
                    helpText(
                        "Each bar is the set of genes significant in exactly that combination of comparisons;",
                        "the dots below show which comparisons make up the bar. Click a bar to list its genes."
                    ),
                    plotly::plotlyOutput("vennDiagram", height = "auto"),
                    fluidRow(
                        style = "margin-top:10px;",
                        column(3, selectInput("venn_plot_format", "Download as",
                            choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"), selected = "pdf")),
                        column(2, numericInput("venn_plot_width", "Width (in)", value = 10, min = 3, max = 20, step = 1)),
                        column(2, numericInput("venn_plot_height", "Height (in)", value = 8, min = 3, max = 20, step = 1)),
                        column(2, numericInput("venn_plot_dpi", "DPI", value = 300, min = 72, max = 600, step = 50)),
                        column(3, div(style = "margin-top:25px;",
                            downloadButton("download_venn_plot", "Download plot", class = "btn-success", style = "width:100%; color:#fff;")))
                    )
                ),
                column(
                    12,
                    hr(),
                    h4(strong("Set operations")),
                    helpText(
                        "Combine comparisons by their letter:", tags$code("*"), "intersection,",
                        tags$code("+"), "union,", tags$code("-"), "difference, with parentheses to group,",
                        "e.g.", tags$code("(A*B)-C"), ". Clicking a bar fills in the expression for that bar."
                    )
                ),
                column(
                    6,
                    DTOutput("gene_data_sets")
                ),
                column(
                    6,
                    selectInput("select_expression", "Ready-made expressions (one per bar)", choices = c()),
                    textInput("venn_set_expression_input", "Set expression", value = ""),
                    actionButton("evaluateExpression", "Evaluate expression", class = "btn-primary", style = "color:#fff;")
                ),
                column(
                    12,
                    uiOutput("upset_selection_title")
                ),
                hr(),
                column(
                    12,
                    DTOutput("venn_expression_result")
                ),
                hr(),
                column(
                    12,
                    conditionalPanel(
                        condition = "output.venn_expression_result",
                        InteractiveComplexHeatmapOutput("ht1",output_ui = htmlOutput("info"))
                    )
                ),
                div(style = "clear:both;")
            )
        )
    )
)
