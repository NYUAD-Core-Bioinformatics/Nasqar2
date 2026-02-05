tabItem(
    tabName = "volcanoplotTab",
    fluidRow(
        column(
            11,
            box(
                title = "Volcano Plot", solidHeader = T, status = "primary", width = 12,
                fluidRow(
                    column(
                        6,
                        h4(strong("Select differentially expressed data sets:")),
                        wellPanel(
                            uiOutput("select_ui"),
                            hr(),
                            conditionalPanel(
                                condition = "input.select_avo_de_file &&input.select_avo_de_file != 'Select data'",
                                radioButtons("sig_genes_selection",
                                    label = h5("Significant Genes Selction"),
                                    choices = list("All significant genes" = 1, "Up regulated genes" = 2, "Down regulated genes" = 3),
                                    selected = 1
                                )
                            )
                        )
                    ),
                    column(
                        6,
                        conditionalPanel(
                            condition = "input.select_avo_de_file &&input.select_avo_de_file != 'Select data'",
                            h4(strong("Threshold Settings:")),
                            wellPanel(
                                radioButtons("volcano_threshold_type", 
                                    label = h5("Threshold input type:"), 
                                    choices = list("Significance slider(-log10(padj))" = "slider", "Direct padj value" = "direct"),
                                    selected = "slider"
                                ),
                                conditionalPanel(
                                    condition = "input.volcano_threshold_type == 'slider'",
                                    sliderInput("significance_threshold",
                                        label = h5("Significance threshold:"), min = 0,
                                        max = 100, value = 3
                                    )
                                ),
                                conditionalPanel(
                                    condition = "input.volcano_threshold_type == 'direct'",
                                    numericInput("volcano_direct_padj",
                                        label = h5("Direct padj threshold:"), 
                                        min = 0, max = 1, value = 0.05, step = 0.001
                                    )
                                ),
                                sliderInput("log_fold_change_threshold",
                                    label = h5("Fold Change threshold:"), min = 0,
                                    max = 5, value = 3, step = 0.1
                                ),
                                hr(),
                                h5(strong("Gene Labeling (Optional):")),
                                textAreaInput("volcano_genes_of_interest",
                                    label = "Genes to label (leave empty for auto-select top significant):",
                                    placeholder = "e.g., Egfl6,Cnn1,Hoxc11,Lrp3,Nr1h3",
                                    rows = 2,
                                    width = "100%"
                                ),
                                helpText("Enter gene names (or IDs) separated by commas. If specified, only these genes will be labeled in the plot."),
                                hr(),
                                h5(strong("Download Plot:")),
                                fluidRow(
                                    column(6,
                                        selectInput("volcano_plot_format",
                                            label = "File format:",
                                            choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                                            selected = "pdf"
                                        )
                                    ),
                                    column(6,
                                        numericInput("volcano_plot_dpi",
                                            label = "DPI (for PNG/JPEG/TIFF):",
                                            value = 300,
                                            min = 72,
                                            max = 600,
                                            step = 50
                                        )
                                    )
                                ),
                                fluidRow(
                                    column(6,
                                        numericInput("volcano_plot_width",
                                            label = "Width (inches):",
                                            value = 10,
                                            min = 3,
                                            max = 20,
                                            step = 1
                                        )
                                    ),
                                    column(6,
                                        numericInput("volcano_plot_height",
                                            label = "Height (inches):",
                                            value = 8,
                                            min = 3,
                                            max = 20,
                                            step = 1
                                        )
                                    )
                                ),
                                downloadButton("download_volcano_plot", "Download Plot", class = "btn-success", style = "width: 100%; margin-top: 10px;")
                            )
                        )
                    )
                )
            ),
            hr(),
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
                        
                        div(style = "clear:both;"),
                        hr(),
                        column(12,div(
                            h3(textOutput("sig_genes_header")),
                            hr(),
                            withSpinner(
                                dataTableOutput("sig_gene_table"),

                                # htmlOutput("enrichGo_volcano")
                                # ,

                                # conditionalPanel(
                                #     condition = "input.gene_alias=='included'",
                                #     wellPanel(
                                #         radioButtons("volcano_sel_gene_type", "Gene enrichment by",
                                #             c(
                                #                 "gene.id" = "gene.id",
                                #                 "gene.name" = "gene.name"
                                #             ),
                                #             selected = "gene.id"
                                #         ),
                                #         div(style = "clear:both;")
                                #     )
                                # ),
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
