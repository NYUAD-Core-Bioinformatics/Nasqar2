tabItem(
  tabName = "taxanomyTab",
  
  # DADA2 Taxonomy Assignment Box
  fluidRow(
    column(
      12,
      box(
        title = "DADA2 Taxonomy Assignment", 
        solidHeader = TRUE, 
        status = "primary", 
        width = 12, 
        collapsible = TRUE, 
        collapsed = FALSE,
        column(
          12,
          p("Assigning taxonomy is an important step, especially in 16S/18S/ITS amplicon sequencing, to classify sequence variants. 
          The DADA2 package uses a robust classifier method for this purpose."),
          p("It takes as input a set of sequences and a training set of reference sequences with known taxonomy. You can choose from pre-formatted training sets such as RDP, GreenGenes, Silva, or upload your own custom database.")
        ),
        
        # Section 1: Reference Database Selection
        hr(),  # Add horizontal line for section separation
        column(
          12,
          h4("Select or Upload Reference Database"),  # Section header
          p("Choose the reference database you would like to use for taxonomy assignment or upload a custom file."),
          p("For more details on preparing a custom reference database, visit: ",
            a("How to Prepare a Reference Database", href = "https://benjjneb.github.io/dada2/training.html", target = "_blank")
          ),
          column(
            6,
            radioButtons("database_choice", "Select Reference Database:",
              choices = list("SILVA v132 (Default)" = "silva",
                             "Upload Custom" = "custom"
              ),
              selected = "silva"
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.database_choice == 'custom'",
              fileInput("custom_silva_nr99", "Upload SILVA nr99 (Reference Sequence File):", 
                        multiple = FALSE,
                        accept = c(".fa", ".gz")
              )
            )
          )
        ),
        
        # Section 2: Optional Species-Level Assignment
        hr(),  # Add horizontal line for section separation
        column(
          12,
          h4("Optional Species-Level Assignment"),  # Section header
          p("Optionally, you can assign species to sequence variants by matching them with known reference strains."),
          column(
            6,
            radioButtons("species_assignment_choice", "Species-Level Assignment:",
              choices = list("SILVA Species (Default)" = "silva_species",
                             "No Species Assignment" = "none",
                             "Upload Custom" = "custom_species"
              ),
              selected = "silva_species"
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.species_assignment_choice == 'custom_species'",
              fileInput("custom_silva_species", "Upload SILVA Species Assignment (Species Assignment File):", 
                        multiple = FALSE,
                        accept = c(".fa", ".gz")
              )
            )
          )
        ),
        
        # Run Taxonomy Assignment
        hr(),  # Add horizontal line for section separation
        column(
          12,
          actionButton("assignTaxonomy", "Run Assign Taxonomy", class = "btn-primary", style = "width: 100%")
        )
      )
    )
  ),
  
  # Conditional Panel to show Taxonomy Table only if taxonomy is ready
  conditionalPanel(
    condition = "output.taxonomy_ready == true",
    fluidRow(
      column(
        12,
        box(
          title = "Taxonomy Table", 
          solidHeader = TRUE, 
          status = "primary", 
          width = 12, 
          collapsible = TRUE, 
          collapsed = FALSE,
          column(
            12, 
            p('Taxonomy output table is wrapped due to its width; please scroll to the right to view all columns.')
          ),
          column(
            12, 
            withSpinner(dataTableOutput("taxonomyTable"))
          ),
          column(
            12,
            downloadButton("download_taxonomy_table", "Download Taxonomy Table", class = "btn-primary", style = "margin-top: 10px;")
          )
        )
      )
    )
  )
)
