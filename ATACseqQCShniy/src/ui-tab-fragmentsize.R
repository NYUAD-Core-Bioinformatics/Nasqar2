

tabItem(
    tabName = "fragmentsize_tab",
    fluidRow(
        column(
            3,wellPanel(
            selectInput("sample_fragmentsize", "Select Sample File", choices = NULL, selected = NULL))
        ),
        column(
            9,
            analysis_panel(
                "Fragment-size distribution",
                "Distribution of aligned fragment lengths used to assess nucleosome periodicity.",
                withSpinner(plotOutput("plot_fragmentsize", height = "560px")),
                publication_downloads("download_fragment_size")
            )
      
            # actionButton("run_deseq2", "Run DESeq2",
            #              class = "btn btn-success",
            #              style = "width:100%;height:60px;"
            # ),
            # plotOutput("plot")
        )
    )
)
