tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Upload fastq file", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # h4("Upload Gene Counts"),
    
                radioButtons("data_file_type", "Use example file or upload your own data (zipped or unzipped FASTQ files)",
                    c(
                        "Upload fastq File" = "upload_fastq_file",
                        "Example fastq file" = "example_fastq_file",
                        "Mount remote server" = "mount_remote_server"
                    ),
                    selected = "example_fastq_file"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='mount_remote_server'",
                    p("Upload ssh private key "),
                    fileInput("id_rsa", ""),
                    textInput("username", "User name", value = ""),
                    textInput("hostname", "Server name", value = ""),
                    textInput("mountpoint", "Directory to mount on remote server", value = ""),
                    actionButton("connect_remote_server", "Connect")
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='upload_fastq_file'",
                    p("File with extensions .fastq"),
                    fileInput("fastq_files", "",
                        accept = c(
                            ".fastq"
                        ), multiple = TRUE
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='example_fastq_file'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://mothur.org/wiki/miseq_sop/", target = "_blank", "this publication")
                    )
                )
            ),
            box(
                title = "FASTQ File Pattern Input", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,

                # Add radio buttons to choose between single-end and paired-end
                radioButtons("seq_type",
                    label = "Sequencing Type:",
                    choices = list("Single-End" = "single", "Paired-End" = "paired"),
                    selected = "paired"
                ), # Default to paired-end

                # Conditional panel for single-end sequencing (only shows forward pattern)
                conditionalPanel(
                    condition = "input.seq_type == 'single'",
                    textInput("forward_pattern",
                        label = "Forward Read Pattern:",
                        value = "_R1_001.fastq",
                        placeholder = "Enter pattern for forward reads (e.g., _R1_001.fastq)"
                    )
                ),

                # Conditional panel for paired-end sequencing (shows both forward and reverse patterns)
                conditionalPanel(
                    condition = "input.seq_type == 'paired'",
                    textInput("forward_pattern",
                        label = "Forward Read Pattern:",
                        value = "_R1_001.fastq",
                        placeholder = "Enter pattern for forward reads (e.g., _R1_001.fastq)"
                    ),
                    textInput("reverse_pattern",
                        label = "Reverse Read Pattern:",
                        value = "_R2_001.fastq",
                        placeholder = "Enter pattern for reverse reads (e.g., _R2_001.fastq)"
                    )
                ),
                actionButton("initFastq", "Load fastq files", class = "btn-info btn-success", style = "width: 100%"),
                tags$div(class = "clearBoth"),
            )


            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        ),
        column(
            6,
            conditionalPanel(condition = "output.fastqfiles_uploaded === true",
            box(
                title = "Fastq Files", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
            tags$div(
                class = "BoxArea2",
                withSpinner(dataTableOutput("fastq_samples_table"))
            )))
        )
    )
)
