require(shinydashboard)
require(shinyjs)
require(shinyBS)
require(shinycssloaders)
require(DT)
require(shiny)
library(sodium)

library(clusterProfiler)
library(DOSE)
library(GOplot)
library(enrichplot)
library(pathview)
library(wordcloud2)

# BiocInstaller::biocLite(c("org.Hs.eg.db","org.Mm.eg.db","org.Rn.eg.db","org.Sc.sgd.db","org.Dm.eg.db","org.At.tair.db","org.Dr.eg.db","org.Bt.eg.db","org.Ce.eg.db","org.Gg.eg.db","org.Cf.eg.db","org.Ss.eg.db","org.Mmu.eg.db","org.EcK12.eg.db","org.Xl.eg.db","org.Pt.eg.db","org.Ag.eg.db","org.Pf.plasmo.db","org.EcSakai.eg.db"))


# UI helper: small PNG / PDF / SVG download strip placed below a plotOutput
plot_dl_buttons <- function(plot_id) {
    tags$div(
        style = "margin-top:6px;",
        downloadButton(paste0("dl_", plot_id, "_png"), "PNG",
                       class = "btn btn-xs btn-default"),
        tags$span("\u00a0"),
        downloadButton(paste0("dl_", plot_id, "_pdf"), "PDF",
                       class = "btn btn-xs btn-default"),
        tags$span("\u00a0"),
        downloadButton(paste0("dl_", plot_id, "_svg"), "SVG",
                       class = "btn btn-xs btn-default")
    )
}

ui <- tagList(
    dashboardPage(
        #skin = "purple",
        dashboardHeader(title = "ClusterProfShiny (ORA)"),
        dashboardSidebar(
            sidebarMenu(
                id = "tabs",
                menuItem("User Guide", tabName = "introTab", icon = icon("info-circle")),
                menuItem("Input Data", tabName = "datainput", icon = icon("upload")),
                menuItem("enrichGo", tabName = "enrichGoTab", icon = icon("th")),
                menuItem("enrichKegg", tabName = "enrichKeggTab", icon = icon("th")),
                menuItem("Go Plots", tabName = "goplotsTab", icon = icon("bar-chart")),
                menuItem("KEGG Plots", tabName = "keggPlotsTab", icon = icon("bar-chart")),
                menuItem("Pathview Plots", tabName = "pathviewTab", icon = icon("bar-chart")),
                menuItem("Word Clouds", tabName = "wordcloudTab", icon = icon("bar-chart"))
            )
        ),
        dashboardBody(
            shinyjs::useShinyjs(),
            extendShinyjs(script = "custom.js", functions = c("addStatusIcon", "collapse")),
            tags$head(
                tags$style(HTML(
                    " .shiny-output-error-validation {color: darkred; }"
                )),
                tags$style(
                    type = "text/css",
                    "#pathview_plot img {max-width: 100%; width: 100%; height: auto}"
                ),
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                tags$script(HTML("
                    function resizeGoUpset() {
                        var el = document.getElementById('genesInGoTerm');
                        if (el && typeof Plotly !== 'undefined') Plotly.Plots.resize(el);
                    }
                    function resizeKeggUpset() {
                        var el = document.getElementById('genesInKeggPathway');
                        if (el && typeof Plotly !== 'undefined') Plotly.Plots.resize(el);
                    }
                    /* Resize when plot tabs become visible */
                    $(document).on('shown.bs.tab', function() {
                        setTimeout(resizeGoUpset, 150);
                        setTimeout(resizeKeggUpset, 150);
                    });
                    /* Also resize after either interactive plot updates */
                    $(document).on('shiny:value', function(e) {
                        if (e.name === 'genesInGoTerm') {
                            setTimeout(resizeGoUpset, 150);
                        }
                        if (e.name === 'genesInKeggPathway') {
                            setTimeout(resizeKeggUpset, 150);
                        }
                    });
                ")),
                tags$style(HTML("
                    .help-tip {
                        position: relative;
                        display: inline-block;
                        color: #3c8dbc;
                        cursor: help;
                        vertical-align: middle;
                        margin-left: 4px;
                    }
                    .help-tip-content {
                        display: none !important;
                        position: absolute;
                        z-index: 9999;
                        width: 300px;
                        background: #fff;
                        color: #333;
                        border: 1px solid #ccc;
                        border-radius: 5px;
                        padding: 10px 14px;
                        font-size: 13px;
                        font-weight: normal;
                        line-height: 1.55;
                        box-shadow: 0 3px 10px rgba(0,0,0,0.15);
                        left: 22px;
                        top: -6px;
                        white-space: normal;
                    }
                    .help-tip:hover .help-tip-content {
                        display: block !important;
                    }
                "))
            ),
            tabItems(
                source("ui-tab-intro.R", local = TRUE)$value,
                source("ui-tab-inputdata.R", local = TRUE)$value,
                source("ui-tab-enrichGo.R", local = TRUE)$value,
                source("ui-tab-enrichKegg.R", local = TRUE)$value,
                source("ui-tab-goplots.R", local = TRUE)$value,
                source("ui-tab-keggplots.R", local = TRUE)$value,
                source("ui-tab-pathview.R", local = TRUE)$value,
                source("ui-tab-wordcloud.R", local = TRUE)$value
            )
        )
    ),
    tags$footer(
        wellPanel(
            HTML(
                '
        <p align="center" width="4">Core Bioinformatics, Center for Genomics and Systems Biology, NYU Abu Dhabi</p>
        <p align="center" width="4">Github: <a href="https://github.com/nyuad-corebio/Nasqar2/tree/main//ClusterProfShiny/">https://github.com/nyuad-corebio/Nasqar2/tree/main//ClusterProfShiny/</a></p>
        <p align="center" width="4">Maintained by: <a href="mailto:nabil.rahiman@nyu.edu">Nabil Rahiman</a> </p>
        <p align="center" width="4">Using ClusterProfiler</p>
        <p align="center" width="4"><strong>Acknowledgements: </strong></p>
        <p align="center" width="4">1) <a href="https://github.com/GuangchuangYu/clusterProfiler" target="_blank">GuangchuangYu/clusterProfiler</a></p>
        <p align="center" width="4">2) <a href="https://learn.gencore.bio.nyu.edu/rna-seq-analysis/over-representation-analysis/" target="_blank">Mohammed Khalfan - Over Representation Analysis Tutorial </a></p>
        <p align="center" width="4">Copyright (C) 2019, code licensed under GPLv3</p>'
            )
        ),
        tags$script(src = "imgModal.js")
    )
)
