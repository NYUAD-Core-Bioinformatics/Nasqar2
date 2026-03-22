tabItem(
    tabName = "gseGoTab",

    # ── CSS: suppress DT row highlight; checkboxes are the only indicator ─────
    tags$style(HTML('
#gseGoTable table tr.selected td,
#gseGoTable table tr.selected {
    background-color: inherit !important;
    color:            inherit !important;
}
    ')),

    # ── JS: checkbox-based row selection → custom input "go_checked_rows" ────
    tags$script(HTML('
var goChecked = new Set();
var goReinit  = false;

$(document).on("destroy.dt", "#gseGoTable table", function() {
  goReinit  = true;
  goChecked = new Set();
  Shiny.setInputValue("go_checked_rows", [], {priority: "event"});
});

$(document).on("init.dt", "#gseGoTable table", function() {
  goReinit = false;
});

$(document).on("draw.dt", "#gseGoTable table", function() {
  if (goReinit) return;
  var dtApi = $(this).DataTable();
  dtApi.rows().every(function(dataIdx) {
    $(this.node()).find("input.go-row-cb")
                  .prop("checked", goChecked.has(dataIdx));
  });
});

$(document).on("change", "#gseGoTable input.go-row-cb", function() {
  var dtApi   = $("#gseGoTable table").DataTable();
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
  var dtApi = $("#gseGoTable table").DataTable();
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
        "output.gseGoAvailable",
        column(
            2,
            h3(strong("gseGo Results")),
            hr(),
            checkboxInput("showAllColumns", "Show all columns", value = F),
            downloadButton("downloadgseGoCSV", "Save Results as CSV File", class = "btn btn-info", style = "margin: 7px;"),
            actionButton("gotoGoPlots", "gseGo Plots", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;"),
            actionButton("gotoPubmed", "Search PubMed Trends", class = "btn btn-warning", icon = icon("chart-area"), style = "margin: 7px;")
        ),
        column(
            10,
            # ── Selected-term pills ──────────────────────────────────────────
            uiOutput("go_selected_pills"),
            tags$div(
                class = "BoxArea2",
                withSpinner(dataTableOutput("gseGoTable")),
                tags$div(class = "clearBoth")
            ),
            tags$div(class = "clearBoth")
        ),
        tags$div(class = "clearBoth")
    )
)
