tabItem(
    tabName = "enrichKeggTab",

    # ── CSS: no DT row highlight (selection driven entirely by checkboxes) ─────
    tags$style(HTML('
#enrichKEGGTable table tr.selected td,
#enrichKEGGTable table tr.selected {
    background-color: inherit !important;
    color:            inherit !important;
}
    ')),

    # ── JS: checkbox-based row selection → custom input "kegg_checked_rows" ───
    #
    # DT row selection is disabled (selection = "none").
    # Checkboxes write a 1-based data-row-index array to kegg_checked_rows.
    # The array is stable across sort / filter redraws.
    # Server-side pill × deselect sends the "kegg_uncheck_row" custom message.
    tags$script(HTML('
var keggChecked  = new Set();   /* 0-based DATA row indices */
var keggReinit   = false;       /* suppress draw.dt restore during re-init  */

/* Table about to be replaced (new enrichment) ─ reset state */
$(document).on("destroy.dt", "#enrichKEGGTable table", function() {
  keggReinit  = true;
  keggChecked = new Set();
  Shiny.setInputValue("kegg_checked_rows", [], {priority: "event"});
});

/* Initialisation complete ─ clear reinit flag */
$(document).on("init.dt", "#enrichKEGGTable table", function() {
  keggReinit = false;
});

/* Sort / filter redraw ─ restore checkbox visual state */
$(document).on("draw.dt", "#enrichKEGGTable table", function() {
  if (keggReinit) return;
  var dtApi = $(this).DataTable();
  dtApi.rows().every(function(dataIdx) {
    $(this.node()).find("input.kegg-row-cb")
                  .prop("checked", keggChecked.has(dataIdx));
  });
});

/* Checkbox toggled by user */
$(document).on("change", "#enrichKEGGTable input.kegg-row-cb", function() {
  var dtApi   = $("#enrichKEGGTable table").DataTable();
  var dataIdx = dtApi.row($(this).closest("tr")).index();
  if ($(this).is(":checked")) { keggChecked.add(dataIdx);    }
  else                        { keggChecked.delete(dataIdx); }
  var arr = Array.from(keggChecked)
                 .map(function(i) { return i + 1; })
                 .sort(function(a, b) { return a - b; });
  Shiny.setInputValue("kegg_checked_rows", arr, {priority: "event"});
});

/* Server-side pill × deselect ─ uncheck specific row */
Shiny.addCustomMessageHandler("kegg_uncheck_row", function(msg) {
  var dataIdx = msg.dataIdx;   /* 0-based, sent from R */
  keggChecked.delete(dataIdx);
  var dtApi = $("#enrichKEGGTable table").DataTable();
  if (dtApi) {
    dtApi.rows().every(function(rowIdx) {
      if (rowIdx === dataIdx) {
        $(this.node()).find("input.kegg-row-cb").prop("checked", false);
      }
    });
  }
  var arr = Array.from(keggChecked)
                 .map(function(i) { return i + 1; })
                 .sort(function(a, b) { return a - b; });
  Shiny.setInputValue("kegg_checked_rows", arr, {priority: "event"});
});
    ')),

    conditionalPanel(
        "output.enrichKEGGAvailable",
        column(
            2,
            h3(strong("enrichKEGG Results")),
            hr(),
            checkboxInput("showGeneidKegg", "Show geneID column", value = F),
            downloadButton("downloadEnrichKEGGCSV", "Save Results as CSV File", class = "btn btn-info", style = "margin: 7px;"),
            actionButton("gotoKeggPlots", "enrichKEGG Plots", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            actionButton("gotoPathview", "Generate Pathview Plot", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            actionButton("gotoWordcloud1", "Word Cloud", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            wellPanel(h4(strong("Output warning:"), tags$a(href = "#", bubbletooltip = "Description ...", icon("info-circle"))),
                htmlOutput("warningText"),
                style = "background-color: #f9d8d3;"
            )
        ),
        column(
            10,
            # ── Selected-pathway pills ───────────────────────────────────────
            uiOutput("kegg_selected_pills"),
            tags$div(
                class = "BoxArea2",
                withSpinner(dataTableOutput("enrichKEGGTable"))
            ),
            tags$div(class = "clearBoth")
        ),
        tags$div(class = "clearBoth")
    )
)
