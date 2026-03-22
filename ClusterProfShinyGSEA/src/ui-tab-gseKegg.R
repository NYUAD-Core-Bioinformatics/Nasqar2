tabItem(
    tabName = "gseKeggTab",

    # ── CSS: no DT row highlight (selection driven entirely by checkboxes) ─────
    tags$style(HTML('
#gseKEGGTable table tr.selected td,
#gseKEGGTable table tr.selected {
    background-color: inherit !important;
    color:            inherit !important;
}
    ')),

    # ── JS: checkbox-based row selection → custom input "kegg_checked_rows" ───
    tags$script(HTML('
var keggChecked  = new Set();
var keggReinit   = false;

$(document).on("destroy.dt", "#gseKEGGTable table", function() {
  keggReinit  = true;
  keggChecked = new Set();
  Shiny.setInputValue("kegg_checked_rows", [], {priority: "event"});
});

$(document).on("init.dt", "#gseKEGGTable table", function() {
  keggReinit = false;
});

$(document).on("draw.dt", "#gseKEGGTable table", function() {
  if (keggReinit) return;
  var dtApi = $(this).DataTable();
  dtApi.rows().every(function(dataIdx) {
    $(this.node()).find("input.kegg-row-cb")
                  .prop("checked", keggChecked.has(dataIdx));
  });
});

$(document).on("change", "#gseKEGGTable input.kegg-row-cb", function() {
  var dtApi   = $("#gseKEGGTable table").DataTable();
  var dataIdx = dtApi.row($(this).closest("tr")).index();
  if ($(this).is(":checked")) { keggChecked.add(dataIdx);    }
  else                        { keggChecked.delete(dataIdx); }
  var arr = Array.from(keggChecked)
                 .map(function(i) { return i + 1; })
                 .sort(function(a, b) { return a - b; });
  Shiny.setInputValue("kegg_checked_rows", arr, {priority: "event"});
});

Shiny.addCustomMessageHandler("kegg_uncheck_row", function(msg) {
  var dataIdx = msg.dataIdx;
  keggChecked.delete(dataIdx);
  var dtApi = $("#gseKEGGTable table").DataTable();
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
        "output.gseKEGGAvailable",
        column(
            2,
            h3(strong("gseKEGG Results")),
            hr(),
            checkboxInput("showAllColumns_kegg", "Show all columns", value = F),
            downloadButton("downloadgseKEGGCSV", "Save Results as CSV File", class = "btn btn-info", style = "margin: 7px;"),
            actionButton("gotoKeggPlots", "gseKEGG Plots", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            actionButton("gotoPathview", "Generate Pathview Plot", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
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
                withSpinner(dataTableOutput("gseKEGGTable"))
            ),
            tags$div(class = "clearBoth")
        ),
        tags$div(class = "clearBoth")
    )
)
