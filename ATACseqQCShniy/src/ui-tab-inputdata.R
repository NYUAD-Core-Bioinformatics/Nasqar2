tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Upload bam file", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # h4("Upload Gene Counts"),
                h4("(select .bam/.bai)"),
                radioButtons("data_file_type", "Input source",
                    c(
                        "Upload BAM files" = "upload_bam_file",
                        "Example BAM data" = "example_bam_file",
                        "Use HPC scratch directory" = "hpc_scratch_directory"
                    ),
                    selected = "upload_bam_file"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='hpc_scratch_directory'",
                    textInput(
                        "scratch_directory",
                        "Directory containing BAM and BAI files",
                        value = ""
                    ),
                    actionButton("load_scratch_directory", "Load directory")
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='upload_bam_file'",
                    p("File with extensions .bam/.bai"),
                    fileInput("bam_files", "",
                        accept = c(
                            ".bai",
                            ".bam"
                        ), multiple = TRUE
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='example_bam_file'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://doi.org/10.1007/978-3-319-07212-8_3", target = "_blank", "this publication")
                    )
                )
            ),
            conditionalPanel(
                # "output.fileUploaded",
                condition = "output.bamfiles_uploaded",
                box(
                    title = "ATACseqQC  Parameters", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = T,
                    
                        column(
                            12,
                            selectInput("bs_genome_input", "Reference genome:", choices = NULL, selected = NULL)
                        ),
                        # column(
                        #     12,
                        #     selectInput("tx_db_input", "TxDb:", choices = c(""), selected = NULL)
                        # ),
                        # column(
                        #     12,
                        #     selectInput("phast_cons_input", "PhastCons:", choices = NULL, selected = NULL)
                        # ),
                       
                    
                    actionButton("initQC", "Initialize QC", class = "btn-info btn-success", style = "width: 100%"),
                     tags$div(class = "clearBoth")
                )
            )
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        ),
        column(
            6, 
                tags$div(
                    class = "BoxArea2",
                    withSpinner(dataTableOutput('bam_samples_table'))
                )
                
        )
    )
)
