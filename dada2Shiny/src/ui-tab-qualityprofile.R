tabItem(
    tabName = "qualityprofile_tab",
    fluidRow(
        column(
            12,
      
                box(
                    title = "Inspect read quality profiles", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,

                    column(6,
                       
                        selectInput("sel_sample_qualityprofile_tab", "Salect Sample", choices = NULL, selected = NULL)
                      
                    ),
                    column(12,
                       
                        p('Assess the quality of the sequencing reads to determine where to truncate reads to remove poor-quality regions.')
                        
                    ),
                    column(12,
                        column(6,
                            withSpinner(plotOutput("plot_qualityprofile_fs"))
                        ),
                        column(6,
                            conditionalPanel("input.seq_type == 'paired'",
                                withSpinner(plotOutput("plot_qualityprofile_rs"))
                        ))
                    )
                )
        )
            
        
    )
)
