tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "1. Input data", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, id = "uploadbox",
                # One question decides what the rest of the tab shows. The two
                # private-space choices are removed by the server on a public
                # instance.
                radioButtons("data_file_type", "Where is your data?",
                    c(
                        "Upload a VCF file" = "upload_vcf",
                        "VCF in the private space" = "private_vcf_file",
                        "Example VCF (1000 Genomes chr22 subset, hg19)" = "example_vcf",
                        "Reopen a saved project" = "savedProject"
                    ),
                    selected = "example_vcf"
                ),
                conditionalPanel(
                    condition = "input.data_file_type == 'savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_restore_ui(button_label = "Open project", stacked = TRUE),
                        helpText(style = "margin-top:10px;",
                                 "The VCF, the reference genome, the filters and the annotation come back from the project without recomputing.")
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type == 'upload_vcf'",
                    p("Upload one VCF (.vcf, .vcf.gz or .vcf.bgz). Add its .tbi index in the same upload to skip re-indexing."),
                    fileInput("vcf_files", "", accept = c(".vcf", ".gz", ".bgz", ".tbi"), multiple = TRUE)
                ),
                conditionalPanel(
                    condition = "input.data_file_type == 'example_vcf'",
                    p(
                        "The example is the chr22 VCF shipped with the VariantAnnotation package",
                        "(1000 Genomes, GRCh37 coordinates). The hg19 reference is selected automatically."
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type == 'private_vcf_file'",
                    div(
                        style = "display:flex; gap:8px; align-items:flex-end;",
                        div(style = "flex:1;",
                        textInput("private_vcf_path", "VCF file path in the private space", value = "")
                        ),
                        div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_vcf_path", "Browse...", "Choose a VCF in the private space", multiple = FALSE))
                    ),
                    helpText(
                        "The VCF is read in place. If it is not bgzip-compressed and tabix-indexed,",
                        "a compressed and indexed copy is written once into the project directory."
                    )
                ),
                conditionalPanel(
                    condition = "input.data_file_type != 'savedProject'",
                    div(class = "nasqar-project-private",
                        nasqar_project_name_ui(
                            "variant-project",
                            label = "Project name",
                            help = paste(
                                "Saved automatically in the private space under this name: the indexed VCF when it",
                                "is loaded, then the filtered VCF, the annotation tables and their caches."
                            )
                        )
                    )
                ),
                textOutput("input_status"),
                textOutput("project_path_text"),
                tags$style("#input_status { color: darkred; }")
            ),
            # a reopened project brings its own reference genome
            conditionalPanel(
            condition = "input.data_file_type != 'savedProject'",
            box(
                title = "2. Reference genome", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, id = "genomebox",
                selectInput("genome_input", "Reference genome and gene model",
                    choices = c("Loading installed genomes..." = "")),
                uiOutput("rsid_note"),
                helpText(
                    "Consequences are predicted from the UCSC gene model of the selected assembly",
                    "(knownGene, refGene, ensGene or sgdGene). Contig names are converted between",
                    "chr1 and 1 styles automatically."
                ),
                tags$details(
                    id = "genome_install_panel", style = "margin-bottom: 12px;",
                    tags$summary(tags$strong("Install another reference genome")),
                    helpText(
                        "Reference packages for other species are downloaded from Bioconductor",
                        "(roughly 50 MB to 1 GB per genome) and installed into the writable R library,",
                        "so they remain available in later sessions. The server needs internet access."
                    ),
                    selectInput("genome_install_choice", NULL, choices = c("Loading catalog..." = "")),
                    actionButton("genome_install", "Install", class = "btn-default"),
                    tags$small(class = "text-muted", textOutput("genome_install_library", inline = TRUE)),
                    tags$p(style = "margin-top: 8px;", textOutput("genome_install_status", inline = TRUE)),
                    verbatimTextOutput("genome_install_log", placeholder = FALSE),
                    tags$details(
                        tags$summary("No internet on this server? Install from a login node instead"),
                        tags$style(HTML("#genome_install_offline { white-space: pre-wrap; word-break: break-all; }")),
                        verbatimTextOutput("genome_install_offline", placeholder = FALSE)
                    )
                ),
                actionButton("loadVcf", "Load VCF", icon = icon("play"), class = "btn-success", style = "width: 100%; color:#fff;"),
                tags$div(class = "clearBoth")
            )
            )
        ),
        column(
            6,
            conditionalPanel(
                condition = "output.vcf_ready === true",
                box(
                    title = "VCF overview", solidHeader = TRUE, status = "primary", width = 12,
                    collapsible = TRUE,
                    tags$div(class = "BoxArea2", withSpinner(DT::dataTableOutput("overview_table")))
                )
            )
        )
    )
)
