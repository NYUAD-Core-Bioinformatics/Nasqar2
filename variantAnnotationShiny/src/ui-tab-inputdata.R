tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Load VCF", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, id = "uploadbox",
                radioButtons("data_file_type", "Select VCF source",
                    c(
                        "Upload VCF file" = "upload_vcf",
                        "Example VCF (1000 Genomes chr22 subset, hg19)" = "example_vcf",
                        "Use a VCF in the private space" = "private_vcf_file"
                    ),
                    selected = "example_vcf"
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
                nasqar_project_ui(
                    "variant-project",
                    help = paste(
                        "Filtered VCF, annotation tables and caches are kept under this name in the",
                        "private space, so they survive the session and the project can be restored later."
                    )
                ),
                textOutput("input_status"),
                textOutput("project_path_text"),
                tags$style("#input_status { color: darkred; }")
            ),
            box(
                title = "Reference genome", solidHeader = TRUE, status = "primary", width = 12,
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
                actionButton("loadVcf", "Load VCF", class = "btn-info btn-success", style = "width: 100%"),
                tags$div(class = "clearBoth")
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
