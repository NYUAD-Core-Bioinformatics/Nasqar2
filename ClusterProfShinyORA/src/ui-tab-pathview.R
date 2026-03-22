tabItem(
    tabName = "pathviewTab",
    h2(strong("Pathview Plot")),
    wellPanel(
        column(
            6,
            selectizeInput("pathwayIds",
                label = "Select Pathway ID",
                choices = NULL,
                multiple = F,
                options = list(
                    placeholder =
                        "Start typing pathway id or name"
                )
            )
        ),
        column(
            6,
            selectInput("geneid_type", "Gene ID type:", choices = c(""), selected = NULL)
        ),
        column(
            12,
            uiOutput("keggColorUrl")
        ),
        tags$hr(),
        column(
            12,
            tags$p(
                class = "text-muted",
                style = "font-size:12px;",
                icon("info-circle"),
                " The KEGG link above opens the pathway directly in your browser with genes colored by fold change — no computation needed.",
                tags$br(),
                "Use ", tags$strong("Generate Pathview"), " below to download a local PNG/PDF overlay instead."
            )
        ),
        column(
            6,
            actionButton("generatePathview", "Generate Pathview (local PNG/PDF)", class = "btn btn-info")
        ),
        conditionalPanel(
            "output.pathviewPlotsAvailable",
            column(
                3,
                downloadButton("downloadPathviewPng", "Download .png", class = "btn btn-warning", style = "margin: 7px;")
            ),
            column(
                3,
                downloadButton("downloadPathviewPdf", "Download .pdf", class = "btn btn-warning", style = "margin: 7px;")
            )
        ),
        tags$div(class = "clearBoth")
    ),
    fluidRow(
        column(12, uiOutput("pathwayGenesTableUI"))
    ),
    fluidRow(
        column(12, withSpinner(imageOutput(outputId = "pathview_plot", inline = T), type = 8))
    )
)
