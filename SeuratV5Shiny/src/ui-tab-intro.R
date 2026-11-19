tabItem(tabName = "introTab",
        fluidRow(

          box(title = "User Guide", width = 12, solidHeader = T, status = "primary",
              column(12,
                     includeMarkdown("intro.Rmd")
                     )
              )
        )
)
