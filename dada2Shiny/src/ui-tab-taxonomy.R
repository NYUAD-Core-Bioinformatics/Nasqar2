tabItem(
    tabName = "taxanomyTab",
    
     fluidRow(
        column(
            12,
            box(
                title = "Taxonomy Table", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                column(
                12, p('Taxonomy output table is wrapped due to its width; please scroll to the right to view all columns.')
                ),
                column(
                12, withSpinner(dataTableOutput("taxonomyTable"))
                ),
                column(
                12,downloadButton("download_taxonomy_table", "Download Taxonomy Table", class = "btn-primary", style = "margin-top: 10px;")
                )
            )
        )
    )
)
