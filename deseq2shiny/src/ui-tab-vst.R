tabItem(
    tabName = "vstTab",
    fluidRow(
        column(
            11,
            h3("Variance Stabilizing Transformation"),
            hr(),
            p("This function calculates a variance stabilizing transformation (VST) from the fitted dispersion-mean relation(s) and then transforms the count data (normalized by division by the size factors or normalization factors), yielding a matrix of values which are now approximately homoskedastic (having constant variance along the range of mean values). The transformation also normalizes with respect to library size. The rlog is less sensitive to size factors, which can be an issue when size factors vary widely. These transformations are useful when checking for outliers or as input for machine learning techniques such as clustering or linear discriminant analysis."),
            hr(),
            wellPanel(
                box(
                    title = "Distance Heatmap", width = 12, solidHeader = T, status = "primary", height = "100%",
                    # a fixed, near-square frame keeps the cells square whatever the box width
                    div(style = "max-width:780px; margin:0 auto;",
                        withSpinner(plotlyOutput(outputId = "vsdPlot", width = "100%", height = "700px"))),
                    publication_downloads("download_vst_distance")
                ),
                box(
                    title = "PCA Plot", width = 12, solidHeader = T, status = "primary",
                    selectInput("vsdIntGroupsInput", "Group of interest", choices = c()),
                    # same frame as the distance heatmap above
                    div(style = "max-width:780px; margin:0 auto;",
                        withSpinner(plotlyOutput(outputId = "vsdPcaPlot", width = "100%", height = "600px"))),
                    publication_downloads("download_vst_pca")
                ),
                h4(p(class = "text-right", downloadButton("downloadVsdCsv", "Download vsd.csv", class = "btn btn-primary btn-sm"))),
                withSpinner(dataTableOutput("vsdData"))
            )
        )
    ) # fluidrow
)
