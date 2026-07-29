tabItem(
    tabName = "qualityprofile_tab",
    fluidRow(
        column(
            12,
      
                box(
                    title = "Inspect read quality profiles", solidHeader = T, status = "primary", width = 12, collapsible = T, id = "qc_parameters", collapsed = F,

                    column(6,
                       
                        selectInput("sel_sample_qualityprofile_tab", "Select Sample", choices = NULL, selected = NULL)
                      
                    ),
                    column(12,
                       
                        p('Assess the quality of the sequencing reads to determine where to truncate reads to remove poor-quality regions.')
                        
                    ),
                    column(12,
                        column(6,
                            h4("Forward-read quality profile"),
                            withSpinner(plotOutput("plot_qualityprofile_fs", height = "440px")),
                            publication_downloads("download_quality_forward")
                        ),
                        column(6,
                            conditionalPanel("input.seq_type == 'paired'",
                                h4("Reverse-read quality profile"),
                                withSpinner(plotOutput("plot_qualityprofile_rs", height = "440px")),
                                publication_downloads("download_quality_reverse")
                        ))
                    )
                )
        )
            
        
    )
)
