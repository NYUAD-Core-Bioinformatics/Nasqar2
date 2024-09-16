        tabItem(
            tabName = "userGuideTab",
            fluidRow(
                box(
                    title = "DADA2 Pipeline User Guide", 
                    width = 12, 
                    solidHeader = TRUE, 
                    status = "primary",
                    includeMarkdown("user_guide.md")  # Load the user guide from a Markdown file
                )
            )
        )