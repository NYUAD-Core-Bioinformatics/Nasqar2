tabItem(
    tabName = "trackReadsTab",
    fluidRow(
        column(
            6,
            box(
                title = "Assign Taxonomy", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                actionButton("assignTaxonomy", "Assign Taxonomy", class = "btn-info btn-success", style = "width: 100%")
            )
        )
    ),
    fluidRow(
        column(
            12,
            box(
                title = "Track Reads Summary", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                withSpinner(dataTableOutput("trackTable")),
                downloadButton("download_track_table", "Download Track Reads Table", class = "btn-primary", style = "margin-top: 10px;")
            )
        )
    ),
    fluidRow(
        column(
            12,
            box(
                title = "Download Processed Files", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                wellPanel(
                    fluidRow(
                        column(3, downloadButton("download_zip_denoisedF", "Download Denoised F as ZIP", class = "btn-block")),
                        column(3, downloadButton("download_zip_denoisedR", "Download Denoised R as ZIP", class = "btn-block")),
                        # column(3, downloadButton("download_zip_merged", "Download Merged as ZIP", class = "btn-block")),
                        column(3, downloadButton("download_zip_nonchim", "Download Non-chimeric as ZIP", class = "btn-block"))
                    )
                )
            )
        )
    )
)
