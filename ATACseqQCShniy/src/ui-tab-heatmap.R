

tabItem(
    tabName = "heatmap_tab",
    fluidRow(

        conditionalPanel("output.multiple_bamfiles",

        column(
            3, wellPanel(
            selectInput("sel_chromosome_heatmap_tab",
                            label = "Chromosome", # or Ensembl ID",
                            choices = NULL,
                         )
            )
        ),
        
        column(
            9,
            analysis_panel(
                "Replicate correlation heatmap",
                "Pairwise ATAC-seq signal similarity across uploaded BAM files for the selected chromosome.",
                withSpinner(plotOutput("plot_heatmap", height = "620px")),
                publication_downloads("download_replicate_heatmap")
            )
        )
        ),
        conditionalPanel("!output.multiple_bamfiles",
            h3('Upload multiple bamfiles for heatmap!!')
        )
    )
)
