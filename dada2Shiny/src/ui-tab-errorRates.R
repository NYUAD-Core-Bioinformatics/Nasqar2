tabItem(
    tabName = "errorRatesTab",
    fluidRow(
        box(
            title = "Learn the Error Rates", 
            width = 12, 
            solidHeader = TRUE, 
            status = "primary",
            
            column(
                12,
                p('The plots display the results of the DADA2 error model, which compares the observed error frequencies (gray points) for specific nucleotide substitutions (e.g., A to C, G to T) against the predicted error rates (red line) based on the quality scores of the reads.'),
                p('By learning the error patterns, DADA2 can more accurately distinguish between true biological sequences and sequencing errors, improving the overall accuracy of sequence analysis. This model plays a key role in denoising the data for downstream analysis.'),
                p('For a more in depth explanation on how to interpret these plots, please refer to the DADA2 tutorial here [https://benjjneb.github.io/dada2/index.html]')
            ),
            column(
                6,
                h4("Forward-read error model"),
                withSpinner(plotOutput("plotErrors_errF", height = "520px")),
                publication_downloads("download_error_forward")

            
            ),
            column(
                6,
                conditionalPanel(
                    "input.seq_type == 'paired'",
                    h4("Reverse-read error model"),
                    withSpinner(plotOutput("plotErrors_errR", height = "520px")),
                    publication_downloads("download_error_reverse")
                )

            )
        )
    )
)
