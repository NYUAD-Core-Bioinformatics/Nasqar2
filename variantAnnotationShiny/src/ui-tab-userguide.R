tabItem(
    tabName = "userGuideTab",
    fluidRow(
        tabBox(
            width = 12,
            tabPanel("User guide", includeMarkdown("user_guide.md")),
            # background on VCF files and on what each tab computes, for users
            # who are new to variant data
            tabPanel("VCF primer", includeMarkdown("vcf_primer.md"))
        )
    )
)
