tabItem(
    tabName = "pathviewTab",
    h2(strong("Pathview Plot")),
    wellPanel(
        fluidRow(
            column(
                9,
                selectizeInput("pathwayIds",
                    label = "Select Pathway ID",
                    choices = NULL,
                    multiple = F,
                    options = list(placeholder = "Start typing pathway id or name")
                )
            ),
            column(
                3,
                tags$br(),
                actionButton("generatePathview", "Generate Pathview", class = "btn btn-info")
            )
        ),
        fluidRow(
            column(12, uiOutput("keggColorUrl"))
        ),
        conditionalPanel(
            "output.pathviewPlotsAvailable",
            fluidRow(
                column(
                    3,
                    downloadButton("downloadPathviewPng", "Download .png", class = "btn btn-warning")
                ),
                column(
                    3,
                    downloadButton("downloadPathviewPdf", "Download .pdf", class = "btn btn-warning")
                )
            )
        )
    ),
    fluidRow(
        column(12, uiOutput("pathwayGenesTableUI"))
    ),
    fluidRow(
        column(12, withSpinner(imageOutput(outputId = "pathview_plot", inline = T), type = 8))
    )
)
