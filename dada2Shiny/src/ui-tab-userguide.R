        tabItem(
            tabName = "userGuideTab",
            fluidRow(
                box(
                    title = "User Guide", 
                    width = 12, 
                    solidHeader = TRUE, 
                    status = "primary",
                    includeMarkdown("user_guide.md")  # Load the user guide from a Markdown file
                )
            )
        )