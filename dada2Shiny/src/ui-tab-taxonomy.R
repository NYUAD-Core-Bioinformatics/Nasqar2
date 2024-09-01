tabItem(
    tabName = "taxanomyTab",
    fluidRow(
        column(
            6,
            box(
                title = "Taxonomy", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,
                actionButton("assignTaxonomy", "Asssign Taxonomy", class = "btn-info btn-success", style = "width: 100%")
            ),
        ),
        column(12, withSpinner(dataTableOutput("taxonomyTable")))
    )
)
