tabItem(
    tabName = "deseqTab",
    fluidRow(
        column(
            11,
            h3("Run DESeq2"),
            hr(),
            box(
                title = "DESeq2 run settings", solidHeader = T, status = "success", width = 12,
                column(
                    6,
                    wellPanel(
                        p("DESeq2 tests for differential expression with a negative binomial model, in three steps:"),
                        tags$ol(
                            tags$li("Estimation of size factors"),
                            tags$li("Estimation of dispersion"),
                            tags$li("Negative binomial GLM fitting and Wald statistics")
                        ),
                        checkboxInput("computeVST", "Compute the VST transformation", value = T),
                        checkboxInput("computeRlog", "Compute the rlog transformation (may take a long time)", value = F)
                    ),
                    actionButton("run_deseq2", "Run DESeq2",
                        class = "btn btn-success",
                        style = "width:100%;height:60px;color:#fff;"
                    )
                ),
                column(
                    6,
                    tags$div(
                        class = "BoxArea",
                        p(strong("Design formula: "), textOutput("ddsDesignFormula")),
                        conditionalPanel(
                            "output.hasExcludedSamples",
                            hr(),
                            p(strong("Excluded samples: "), style = "color: #d9534f;"),
                            p(textOutput("excludedSamplesList2"), style = "font-size: 12px; color: #d9534f; margin-left: 15px;")
                        ),
                        hr(),
                        p("First 5 rows of the sample metadata:"),
                        tableOutput("ddsColData")
                    )
                ),
                column(
                    12,
                    conditionalPanel("output.noReplicates", wellPanel(p('Experiments without replicates do not allow for estimation of the dispersion of counts
                                                                                                                                              around the expected value for each group, which is critical for differential expression analysis.
                                                                                                                       If an experimental design is supplied which does not contain the necessary degrees of freedom for
                                                                                                                       differential analysis, DESeq will provide a message to the user and follow the strategy outlined
                                                                                                                       in Anders and Huber (2010) under the section "Working without replicates", wherein all the samples
                                                                                                                       are considered as replicates of a single group for the estimation of dispersion. As noted in the
                                                                                                                       reference above: "Some overestimation of the variance may be expected, which will make that
                                                                                                                       approach conservative." Furthermore, "while one may not want to draw strong conclusions
                                                                                                                       from such an analysis, it may still be useful for exploration and hypothesis generation."')), style = "color:#f56a6a;")
                ),
                div(style = "clear:both;")
            ),
            box(
                title = "(Optional) Surrogate Variable Analysis (SVA)", solidHeader = T, status = "primary", width = 6,
                wellPanel(
                    p("If you suspect a hidden batch effect, estimate surrogate variables first. You can then add them to the design formula before running DESeq2.")
                ),
                actionButton("goto_svaTab", "Go to Surrogate Variable Analysis (SVA)",
                    class = "btn btn-primary",
                    style = "width:100%;height:60px;color:#fff;"
                )
            )
        )
    ) # fluidrow
)
