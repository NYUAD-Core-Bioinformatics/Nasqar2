tabItem(tabName = "datainput",
         hr(),
         fluidRow(column(3,
                         box(title = "1. Input data", solidHeader = T, status = "primary", width = 12, collapsible = T,id = "uploadbox",

                             # One question decides what the rest of the box
                             # shows. The two private-space choices are removed
                             # by the server on a public instance.
                             nasqar_project_step_ui("seurat-project"),
                             radioButtons('data_file_type','Where is your data?',
                                          c(
                                            'Upload a count matrix (.csv)'="uploadNonUmi",
                                            'Upload 10X files'="upload10x",
                                            'Data in the private space'="privateSpace",
                                            'Example data (PBMC)'="examplecounts"
                                          ),selected = "uploadNonUmi"),
                             conditionalPanel(
                               condition="input.data_file_type=='privateSpace'",
                               div(
                                   style = "display:flex; gap:8px; align-items:flex-end;",
                                   div(style = "flex:1;",
                                   textInput(
                                     "private_data_path",
                                     "10X folder or CSV/TSV count matrix in the private space",
                                     value = ""
                                   )
                                   ),
                                   div(style = "margin-bottom:15px;", shinyFiles::shinyDirButton("browse_private_dir", "Browse folder...", "Choose a 10X directory in the private space")),
                                   div(style = "margin-bottom:15px;", shinyFiles::shinyFilesButton("browse_private_file", "Browse...", "Choose a count matrix in the private space", multiple = FALSE))
                               ),
                               actionButton("load_private_data", "Load data", icon = icon("play"), class = "btn btn-primary", style = "color:#fff;"),
                               helpText("The input is read directly; it is not copied through the browser.")
                             ),
                             conditionalPanel(condition="input.data_file_type=='upload10x'",
                                              helpText("Select the three 10X files together: one .mtx.gz file and two .tsv.gz files.")
                             ),
                             conditionalPanel(condition="input.data_file_type=='uploadNonUmi'",
                                              helpText("One count matrix as a CSV file: genes in rows, cells in columns.")
                             ),
                             conditionalPanel(condition = "input.data_file_type=='uploadNonUmi' || input.data_file_type=='upload10x'",
                                              fileInput('datafile', 'File(s) to upload', multiple = TRUE)
                             )

                         ),
                         conditionalPanel("output.fileUploaded",
                                          box(title = "2. Initial parameters", solidHeader = T, status = "primary", width = 12, collapsible = T,

                                              textInput("projectname",
                                                        value = "Project1", label = "Project name"),
                                              div(class = "nasqar-project-private",
                                                  helpText("The Seurat object and the outputs are saved automatically in the private space under this name.")),
                                              numericInput("mincells",
                                                           label="Minimum number of cells per gene",min=1,max=200,value=3),
                                              numericInput("mingenes",
                                                           label="Minimum number of genes per cell",min=1,max=Inf,value=200),
                                              actionButton("upload_data","Next Step: QC & Filter Cells", class = "button button-3d button-block button-pill button-caution", style = "width: 100%")

                                          )
                         )
         ),#column
         column(9,
                bsCollapse(id="input_collapse_panel",open="data_panel",multiple = FALSE,
                           bsCollapsePanel(title="Data Contents Table:",value="data_panel",
                                           p("Note: if there are more than 20 columns, only the first 20 will show here"),
                                           textOutput("inputInfo"),
                                           withSpinner(dataTableOutput('countdataDT'))
                           )
                )#bscollapse
         )#column
         )#fluidrow
)#tabpanel
