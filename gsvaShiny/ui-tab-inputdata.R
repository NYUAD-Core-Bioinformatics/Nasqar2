tabItem(
    tabName = "input_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Upload Cellranger 10x gene counts", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "uploadbox",
                numericInput('sample_count', 'Num pof samples', 1, min=1, max=20, step=1),
               
                uiOutput("uploaders"),
                
                actionButton("upload", "Next")
                
                actionButton("filter", "Filter")
                
                
                
                
                
    
              
                
            )
        ),
        column(6,  withSpinner(dataTableOutput('samples_table')))
    )
)

