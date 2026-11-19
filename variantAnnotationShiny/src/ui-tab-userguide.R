# The guide states the limits of this instance with their actual values:
# {{...}} placeholders in the markdown are filled in before it is rendered.
guide_with_limits <- function(path) {
    text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    limits <- c(
        "{{summary_limit}}" = format(yield_size_limit(), big.mark = ",", scientific = FALSE),
        "{{table_preview_rows}}" = format(table_preview_rows(), big.mark = ",", scientific = FALSE)
    )
    for (placeholder in names(limits)) {
        text <- gsub(placeholder, limits[[placeholder]], text, fixed = TRUE)
    }
    # rendered from text, in the application directory, so that the figures
    # (www/...) are found and embedded as includeMarkdown() does for a file
    html <- markdown::markdownToHTML(text = text, fragment.only = TRUE)
    Encoding(html) <- "UTF-8"
    HTML(html)
}

tabItem(
    tabName = "userGuideTab",
    fluidRow(
        tabBox(
            width = 12,
            tabPanel("User guide", guide_with_limits("user_guide.md")),
            # background on VCF files and on what each tab computes, for users
            # who are new to variant data
            tabPanel("VCF primer", includeMarkdown("vcf_primer.md"))
        )
    )
)
