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
                p('By learning the error patterns, DADA2 can more accurately distinguish between true biological sequences and sequencing errors, improving the overall accuracy of sequence analysis. This model plays a key role in denoising the data for downstream analysis.')
            ),
            column(
                6,
                withSpinner(plotOutput("plotErrors_errF"))

            
            ),
            column(
                6,
                conditionalPanel(
                    "input.seq_type == 'paired'",
                    withSpinner(plotOutput("plotErrors_errR"))
                )

            )
        )
    )
)
