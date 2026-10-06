tabItem(
    tabName = "handoff_tab",
    fluidRow(
        column(
            6,
            box(
                title = "Send genes to enrichment analysis", solidHeader = TRUE, status = "primary",
                width = 12, collapsible = TRUE,
                p(
                    "Genes overlapping the annotated variants are written to the NASQAR2 exchange",
                    "area and opened in ClusterProfShinyORA for GO and KEGG over-representation analysis."
                ),
                radioButtons("handoff_id_type", "Gene identifier",
                    c("Gene symbol" = "SYMBOL", "Entrez Gene ID" = "ENTREZID"), inline = TRUE),
                checkboxInput("handoff_coding_only",
                    "Only genes with coding or splice-site variants", value = TRUE),
                textOutput("handoff_gene_count"),
                helpText(
                    "In ClusterProfShinyORA choose the organism Human and set Keytype to the same",
                    "identifier (SYMBOL or ENTREZID) before running the enrichment."
                ),
                uiOutput("handoff_link")
            )
        )
    )
)
