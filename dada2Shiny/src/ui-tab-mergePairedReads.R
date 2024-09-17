tabItem(
    tabName = "margePairedReadsTab",
    fluidRow(
        box(
            title = "Merged Paired Reads",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            column(
                12,
                p('The merged paired reads output table provides information on the successful merging 
                of forward and reverse reads.'),
                p('Merging is performed by aligning the denoised forward reads with the reverse-complement of the corresponding denoised reverse reads, and then constructing the merged “contig” sequences. By default, merged sequences are only output if the forward and reverse reads overlap by at least 12 bases, and are identical to each other in the overlap region (but these conditions can be changed via function arguments).')
                
            ),
            column(
                4,
                selectInput("selSample4margePairedReadsTab", "Salect Sample", choices = NULL, selected = NULL)
            
            ),
            column(
                12, p('The merged paired reads output table is wrapped due to its width; please scroll to the right to view all columns.'),
                p('We provide you with the option to download the merged reads (download link at the bottom of this page)')
            )
        )
    ),
    fluidRow(
        column(
            12,
            box(
                title = "Sequence Abundance Estimation Table ", solidHeader = TRUE, status = "primary", width = 12, collapsible = TRUE, collapsed = FALSE,
                withSpinner(dataTableOutput("mergerTable")),
                downloadButton("download_merger_table", "Download Abundance Estimation Table", class = "btn-primary", style = "margin-top: 10px;")
            )
        )
    ),
)
