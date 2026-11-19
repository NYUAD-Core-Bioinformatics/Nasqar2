tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "1. Input data", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                # One question decides what the rest of the tab shows. The two
                # private-space choices are removed by the server on a public
                # instance.
                radioButtons("data_file_type", "Where is your data?",
                    c(
                        "Upload FASTQ files" = "upload_fastq_file",
                        "FASTQ folder in the private space" = "private_directory",
                        "Example data (mothur MiSeq SOP)" = "example_fastq_file",
                        "Reopen a saved project" = "savedProject"
                    ),
                    selected = "example_fastq_file"
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_restore_ui(button_label = "Open project", stacked = TRUE),
                        helpText(style = "margin-top:10px;",
                                 "The FASTQ folder, the read layout and the settings come back, and the results are loaded from the project.",
                                 "Only projects made from a FASTQ folder in the private space can be reopened.")
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='private_directory'",
                    div(
                        style = "display:flex; gap:8px; align-items:flex-end;",
                        div(style = "flex:1;",
                        textInput(
                            "private_fastq_directory",
                            "FASTQ directory in the private space",
                            value = ""
                        )
                        ),
                        div(style = "margin-bottom:15px;", shinyFiles::shinyDirButton("browse_fastq_directory", "Browse folder...", "Choose a FASTQ directory in the private space"))
                    ),
                    helpText("Input FASTQ files are read in place.")
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='upload_fastq_file'",
                    fileInput("fastq_files", "FASTQ files (.fastq or .fastq.gz)",
                        accept = c(
                            ".fastq", ".gz"
                        ), multiple = TRUE
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type=='example_fastq_file'",
                    p(
                        "For details on this data, see ",
                        a(href = "https://mothur.org/wiki/miseq_sop/", target = "_blank", "the mothur MiSeq SOP")
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type!='savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_name_ui(
                            "dada2-project",
                            label = "Project name",
                            help = "Saved automatically in the private space under this name: the sequence tables, the taxonomy and the settings."
                        )
                    )
                ),
                 textOutput("status"),
                    textOutput("private_dada2_path"),
                    tags$style("#status { color: red; }")
            ),
            # a reopened project brings its own read layout
            conditionalPanel(
            condition = "input.data_file_type!='savedProject'",
            box(
                title = "2. Read layout", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,

                # Add radio buttons to choose between single-end and paired-end
                radioButtons("seq_type",
                    label = "Sequencing type",
                    choices = list("Single-End" = "single", "Paired-End" = "paired"),
                    selected = "paired"
                ), # Default to paired-end

                # Conditional panel for single-end sequencing (only shows forward pattern)
                
                    textInput("forward_pattern",
                        label = "Forward read file ending",
                        value = "_R1_001.fastq",
                        placeholder = "Enter pattern for forward reads (e.g., _R1_001.fastq)"
                    ),
                

                # Conditional panel for paired-end sequencing (shows both forward and reverse patterns)
                conditionalPanel(
                    condition = "input.seq_type == 'paired'",
                    
                    textInput("reverse_pattern",
                        label = "Reverse read file ending",
                        value = "_R2_001.fastq",
                        placeholder = "Enter pattern for reverse reads (e.g., _R2_001.fastq)"
                    )
                ),
                actionButton("initFastq", "Run QC", icon = icon("play"), class = "btn-success", style = "width: 100%; color:#fff;"),
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
            conditionalPanel(condition = "output.fastqfiles_uploaded === true",
            box(
                title = "FASTQ files found", solidHeader = T, status = "primary", width = 12, collapsible = T,
            tags$div(
                class = "BoxArea2",
                withSpinner(dataTableOutput("fastq_samples_table"))
            )))
        )
    )
)
