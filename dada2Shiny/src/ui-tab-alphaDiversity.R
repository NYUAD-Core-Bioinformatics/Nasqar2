tabItem(
    tabName = "alphaDiversityTab",
    fluidRow(
        column(
            6,
            box(
                title = "Upload QIIME Metadata File", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,

                # File input for uploading sample meta data
                fileInput("sampleData", "Choose QIIME Metadata File",
                    accept = c(".csv", ".tsv", ".txt")
                ),
                actionButton("runAlphaDiversity", "Run AlphaDiversity", class = "btn-info btn-success", style = "width: 100%")
            ),
        ),
        column(
            12,
            conditionalPanel(
                condition = "output.divergen_available",
                withSpinner(plotOutput("plotAlphaDiversity"))
            )
        ),
        column(
            12,
            conditionalPanel(
                condition = "output.divergen_available",
                withSpinner(plotOutput("plotOrdination"))
            )
            # withSpinner(plotOutput("plotOrdination"))
        ),
        column(
            12,
            conditionalPanel(
                condition = "output.divergen_available",
                withSpinner(plotOutput("plotBar"))
            )
            #     withSpinner(plotOutput("plotBar"))
        )
    )
)
