

tabItem(
    tabName = "librarycomplexity_tab",
    fluidRow(
        column(
            3,wellPanel(
            selectInput("sample_librarycomplexity", "Select Sample File", choices = NULL, selected = NULL))
        ),
        column(
            9,
            analysis_panel(
            "Library complexity",
            "Estimated sequencing-library complexity. Use a BAM file that retains duplicate reads.",
            withSpinner(plotOutput("plot_libcomplexity", height = "560px")),
            publication_downloads("download_library_complexity")
            )
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
