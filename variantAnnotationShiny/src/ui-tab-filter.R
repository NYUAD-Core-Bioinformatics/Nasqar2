tabItem(
    tabName = "filter_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Record filters", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, id = "filterbox",
                checkboxInput("pass_only", "Keep only records with FILTER = PASS (or no filter applied)", value = TRUE),
                numericInput("min_qual", "Minimum QUAL (leave blank for no threshold)", value = NA, min = 0),
                numericInput("min_dp", "Minimum INFO/DP (leave blank for no threshold)", value = NA, min = 0),
                checkboxGroupInput("variant_types", "Variant types to keep",
                    choices = c("SNV", "Indel", "MNV", "Multiallelic", "Other"),
                    selected = c("SNV", "Indel", "MNV", "Multiallelic"), inline = TRUE),
                helpText("Multiallelic records are split into one row per alternate allele during annotation. 'Other' covers symbolic alleles such as <DEL> or breakends, which cannot be annotated.")
            ),
            box(
                title = "Genomic restriction (recommended for large files)", solidHeader = TRUE,
                status = "primary", width = 12, collapsible = TRUE,
                selectizeInput("chromosomes", "Chromosomes (empty = all)", choices = NULL, multiple = TRUE),
                textAreaInput("regions", "Regions, one per line (chr:start-end)", rows = 3,
                    placeholder = "chr17:7565097-7590856"),
                textAreaInput("gene_list", "Gene symbols, one per line or comma separated", rows = 3,
                    placeholder = "TP53\nBRCA1"),
                numericInput("gene_flank", "Flank added around each gene (bp)", value = 2000, min = 0, step = 500),
                textOutput("scan_estimate"),
                actionButton("applyFilter", "Apply filters", class = "btn-info btn-success", style = "width: 100%"),
                tags$div(class = "clearBoth")
            )
        ),
        column(
            6,
            conditionalPanel(
                condition = "output.filter_ready === true",
                box(
                    title = "Filter summary", solidHeader = TRUE, status = "primary", width = 12,
                    collapsible = TRUE,
                    tags$div(class = "BoxArea2", DT::dataTableOutput("filter_stats_table")),
                    textOutput("filter_notes")
                )
            )
        )
    )
)
