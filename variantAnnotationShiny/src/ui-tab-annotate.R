tabItem(
    tabName = "annotate_tab",
    fluidRow(
        column(
            4,
            box(
                title = "Annotation options", solidHeader = TRUE, status = "primary", width = 12,
                collapsible = TRUE, id = "annotatebox",
                numericInput("promoter_upstream", "Promoter: bases upstream of the TSS", value = 2000, min = 0, step = 100),
                numericInput("promoter_downstream", "Promoter: bases downstream of the TSS", value = 200, min = 0, step = 50),
                                helpText(
                    "Every variant is located against all transcripts (coding, splice site, intron,",
                    "UTR, promoter, intergenic). Coding variants receive predicted consequences and",
                    "amino-acid changes, dbSNP rsIDs are added when the reference data are installed,",
                    "and the per-variant view keeps the most severe transcript effect."
                ),
                actionButton("runAnnotation", "Annotate variants", class = "btn-info btn-success", style = "width: 100%"),
                tags$div(class = "clearBoth"),
                textOutput("cache_status")
            ),
            conditionalPanel(
                condition = "output.annotation_ready === true",
                box(
                    title = "Annotation report", solidHeader = TRUE, status = "primary", width = 12,
                    collapsible = TRUE,
                    tags$div(class = "variant-report", textOutput("annotation_report"))
                )
            )
        ),
        column(
            8,
            conditionalPanel(
                condition = "output.annotation_ready === true",
                box(
                    title = "Annotated variants", solidHeader = TRUE, status = "primary", width = 12,
                    collapsible = TRUE,
                    radioButtons("table_view", NULL,
                        c("One row per variant (most severe effect)" = "per_variant",
                          "One row per variant and transcript" = "per_transcript"),
                        inline = TRUE),
                    tags$p(class = "text-muted", textOutput("annotation_table_note", inline = TRUE)),
                    tags$div(class = "BoxArea2", withSpinner(DT::dataTableOutput("annotation_table")))
                )
            )
        )
    )
)
