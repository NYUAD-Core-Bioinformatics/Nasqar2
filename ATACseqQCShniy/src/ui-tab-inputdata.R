tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "1. Input data", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # One question decides what the rest of the box shows. The two
                # private-space choices are removed by the server on a public
                # instance.
                nasqar_project_step_ui("atac-project"),
                radioButtons("data_file_type", "Where is your data?",
                    c(
                        "Upload BAM files" = "upload_bam_file",
                        "BAM folder in the private space" = "private_directory",
                        "Example data" = "example_bam_file"
                    ),
                    selected = "upload_bam_file"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='private_directory'",
                    div(
                        style = "display:flex; gap:8px; align-items:flex-end;",
                        div(style = "flex:1;",
                        textInput(
                            "private_directory_path",
                            "Folder with the BAM and BAI files",
                            value = ""
                        )
                        ),
                        div(style = "margin-bottom:15px;", shinyFiles::shinyDirButton("browse_private_directory", "Browse folder...", "Choose a directory of BAM and BAI files in the private space"))
                    ),
                    actionButton("load_private_directory", "Load folder", icon = icon("folder-open"),
                                 class = "btn-primary", style = "color:#fff;")
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='upload_bam_file'",
                    fileInput("bam_files", "BAM files with their BAI index files (.bam and .bai)",
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
                ),
            ),
            conditionalPanel(
                # "output.fileUploaded",
                condition = "output.bamfiles_uploaded",
                box(
                    title = "2. Reference genome", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,
                    
                        column(
                            12,
                            selectInput("bs_genome_input", "Reference genome", choices = NULL, selected = NULL)
                        ),
                        # column(
                        #     12,
                        #     selectInput("tx_db_input", "TxDb:", choices = c(""), selected = NULL)
                        # ),
                        # column(
                        #     12,
                        #     selectInput("phast_cons_input", "PhastCons:", choices = NULL, selected = NULL)
                        # ),
                       
                    
                    actionButton("initQC", "Initialize QC", icon = icon("play"), class = "btn-success", style = "width: 100%; color:#fff;"),
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
