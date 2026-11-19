tabItem(
    tabName = "volcanoplotTab",
    fluidRow(
        conditionalPanel(
            condition = "output.saved_comparison_count < 1",
            div(
                style = "margin:15px; padding:16px 20px; border:1px solid #cfe2f3; border-left:4px solid #3c8dbc; border-radius:4px; background:#f4f8fb;",
                h4(style = "margin-top:0;", icon("circle-info"), " Save a comparison first"),
                p(style = "margin-bottom:6px;", "The volcano plot is drawn from a saved comparison. On the DE Results tab, get the results for a comparison and press 'mark it for further analysis' under the table. One saved comparison is enough."),
                p(style = "margin:0; color:#666;", "Saved so far: ", textOutput("saved_comparison_note_volcano", inline = TRUE))
            )
        ),
        conditionalPanel(
        condition = "output.saved_comparison_count >= 1",
        column(
            12,
            box(
                title = "Volcano plot", solidHeader = T, status = "primary", width = 12,
                fluidRow(
                    column(
                        4,
                        h4(strong("1. Comparison")),
                        uiOutput("select_ui"),
                        radioButtons("sig_genes_selection",
                            label = "Genes to list",
                            choices = list("All significant genes" = 1, "Up-regulated genes" = 2, "Down-regulated genes" = 3),
                            selected = 1
                        )
                    ),
                    column(
                        4,
                        h4(strong("2. Thresholds")),
                        radioButtons("volcano_threshold_type",
                            label = "Significance given as",
                            choices = list("-log10(padj) slider" = "slider", "padj value" = "direct"),
                            selected = "slider", inline = TRUE
                        ),
                        conditionalPanel(
                            condition = "input.volcano_threshold_type == 'slider'",
                            sliderInput("significance_threshold",
                                label = "Significance, -log10(padj)", min = 0,
                                max = 100, value = 3
                            )
                        ),
                        conditionalPanel(
                            condition = "input.volcano_threshold_type == 'direct'",
                            numericInput("volcano_direct_padj",
                                label = "padj below",
                                min = 0, max = 1, value = 0.05, step = 0.001
                            )
                        ),
                        sliderInput("log_fold_change_threshold",
                            label = "Absolute log2 fold change above", min = 0,
                            max = 5, value = 3, step = 0.1
                        )
                    ),
                    column(
                        4,
                        h4(strong("3. Labels (optional)")),
                        textAreaInput("volcano_genes_of_interest",
                            label = "Genes to label",
                            placeholder = "Egfl6,Cnn1,Hoxc11,Lrp3,Nr1h3",
                            rows = 3,
                            width = "100%"
                        ),
                        helpText("Gene names or IDs separated by commas. Leave empty to label the most significant genes.")
                    )
                )
            ),
            div(style = "clear:both;"),
            conditionalPanel(
                condition = "input.select_avo_de_file &&input.select_avo_de_file != 'Select data'",
                div(
                    class = "BoxArea6",
                    fluidRow(
                        column(12,
                            column(2,div()),
                            column(8,plotOutput("curve_plot",height = "100%")),
                            column(2,div())
                        ),
                        column(12, fluidRow(
                        style = "margin-top:10px;",
                        column(3, selectInput("volcano_plot_format", "Download as",
                            choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"), selected = "pdf")),
                        column(2, numericInput("volcano_plot_width", "Width (in)", value = 10, min = 3, max = 20, step = 1)),
                        column(2, numericInput("volcano_plot_height", "Height (in)", value = 8, min = 3, max = 20, step = 1)),
                        column(2, numericInput("volcano_plot_dpi", "DPI", value = 300, min = 72, max = 600, step = 50)),
                        column(3, div(style = "margin-top:25px;",
                            downloadButton("download_volcano_plot", "Download plot", class = "btn-success", style = "width:100%; color:#fff;")))
                    )),
                        
                        div(style = "clear:both;"),
                        hr(),
                        column(12,div(
                            h3(textOutput("sig_genes_header")),
                            hr(),
                            withSpinner(
                                dataTableOutput("sig_gene_table")
                            )
                        ),
                        column(12,hr()),
                        column(12,
                            div(style = "clear:both;"),
                            fluidRow(
                                 conditionalPanel(condition = "input.gene_alias=='included'",
                                column(4,
                                   
                                    radioButtons("volcano_sel_gene_type", "Gene list by",
                                        c(
                                            "gene.id" = "gene.id",
                                            "gene.name" = "gene.name"
                                        ),
                                        selected = "gene.id"
                                        
                                    )
                                )),
                                column(4,
                                    radioButtons("volcano_input_genes_sep", "Genes separted by",
                                        c(
                                            "Comma" = ",",
                                            "Space" = " "
                                        ),
                                        selected = "Space"
                                    )
                                )
                                
                            ),
                            textAreaInput("volcano_gene_list", 'GeneList', rows = 3, width="100%")

                        ))
                    ),

                    
                )
            )
        )
        )
    )
)
