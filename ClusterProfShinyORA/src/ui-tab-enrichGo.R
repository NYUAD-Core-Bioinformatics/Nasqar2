tabItem(
    tabName = "enrichGoTab",

    # ── CSS: suppress DT row highlight; checkboxes are the only indicator ─────
    tags$style(HTML('
#enrichGoTable table tr.selected td,
#enrichGoTable table tr.selected {
    background-color: inherit !important;
    color:            inherit !important;
}
    ')),

    # ── JS: checkbox-based row selection → custom input "go_checked_rows" ────
    tags$script(HTML('
var goChecked = new Set();
var goReinit  = false;

$(document).on("destroy.dt", "#enrichGoTable table", function() {
  goReinit  = true;
  goChecked = new Set();
  Shiny.setInputValue("go_checked_rows", [], {priority: "event"});
});

$(document).on("init.dt", "#enrichGoTable table", function() {
  goReinit = false;
});

$(document).on("draw.dt", "#enrichGoTable table", function() {
  if (goReinit) return;
  var dtApi = $(this).DataTable();
  dtApi.rows().every(function(dataIdx) {
    $(this.node()).find("input.go-row-cb")
                  .prop("checked", goChecked.has(dataIdx));
  });
});

$(document).on("change", "#enrichGoTable input.go-row-cb", function() {
  var dtApi   = $("#enrichGoTable table").DataTable();
  var dataIdx = dtApi.row($(this).closest("tr")).index();
  if ($(this).is(":checked")) { goChecked.add(dataIdx);    }
  else                        { goChecked.delete(dataIdx); }
  var arr = Array.from(goChecked)
                 .map(function(i) { return i + 1; })
                 .sort(function(a, b) { return a - b; });
  Shiny.setInputValue("go_checked_rows", arr, {priority: "event"});
});

Shiny.addCustomMessageHandler("go_uncheck_row", function(msg) {
  var dataIdx = msg.dataIdx;
  goChecked.delete(dataIdx);
  var dtApi = $("#enrichGoTable table").DataTable();
  if (dtApi) {
    dtApi.rows().every(function(rowIdx) {
      if (rowIdx === dataIdx) {
        $(this.node()).find("input.go-row-cb").prop("checked", false);
      }
    });
  }
  var arr = Array.from(goChecked)
                 .map(function(i) { return i + 1; })
                 .sort(function(a, b) { return a - b; });
  Shiny.setInputValue("go_checked_rows", arr, {priority: "event"});
});
    ')),

    conditionalPanel(
        "output.enrichGoAvailable",
        column(
            2,
            h3(strong("enrichGo Results")),
            hr(),
            checkboxInput("showGeneidGo", "Show geneID column", value = F),
            downloadButton("downloadEnrichGoCSV", "Save Results as CSV File", class = "btn btn-info", style = "margin: 7px;"),
            actionButton("gotoGoPlots", "enrichGo Plots", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            actionButton("gotoWordcloud", "Word Cloud", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;")
        ),
        column(
            10,
            # ── Selected-term pills (replaces "Selected entries" table) ──────
            uiOutput("go_selected_pills"),
            tags$div(
                class = "BoxArea2",
                withSpinner(dataTableOutput("enrichGoTable")),
                tags$div(class = "clearBoth")
            ),
            tags$div(class = "clearBoth")
        ),
        tags$div(class = "clearBoth")
    )
)
