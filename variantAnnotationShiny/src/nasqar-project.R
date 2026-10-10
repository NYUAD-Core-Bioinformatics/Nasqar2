# NASQAR2 private-space projects: one convention for every NASQAR2 application.
#
#   <project root>/<App>/<name>/        e.g. /runtime/projects/VariantAnnotation/ncb-934
#       state.rds                       what was loaded, the settings, the stage reached
#       ... application outputs ...
#
# The root comes from NASQAR_PROJECT_ROOT (set by run-nasqar2.sh and the HPC
# launcher). A per-application legacy variable (e.g. NASQAR_VARIANT_PROJECT_DIR)
# still wins when set, so existing deployments keep their layout. When neither
# is set, or the directory is missing or read-only, there is no private space:
# the application works in temporary storage and offers no restore.
#
# This file is maintained in common/nasqar-project.R and copied into each
# application's src/ by common/sync-project-helper.sh. Edit the common copy.

nasqar_project_app_root <- function(app, legacy_env = NULL) {
    candidates <- character(0)
    if (!is.null(legacy_env)) {
        legacy <- Sys.getenv(legacy_env, unset = "")
        if (nzchar(legacy)) candidates <- c(candidates, legacy)
    }
    root <- Sys.getenv("NASQAR_PROJECT_ROOT", unset = "")
    if (nzchar(root)) candidates <- c(candidates, file.path(root, app))
    for (path in candidates) {
        if (!dir.exists(path)) {
            dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
        }
        if (dir.exists(path) && file.access(path, 2) == 0) {
            return(normalizePath(path, winslash = "/", mustWork = FALSE))
        }
    }
    NULL
}

nasqar_project_available <- function(app, legacy_env = NULL) {
    !is.null(nasqar_project_app_root(app, legacy_env))
}

nasqar_project_sanitize <- function(name, fallback = "project") {
    name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(as.character(name %||% "")))
    name <- gsub("^[.-]+|[.-]+$", "", name)
    if (!nzchar(name)) fallback else name
}

# Directory of a named project; a temporary directory when there is no
# private space. The name is sanitised to a safe single path component.
nasqar_project_dir <- function(app, name, legacy_env = NULL, create = TRUE,
                               fallback = "project") {
    root <- nasqar_project_app_root(app, legacy_env)
    safe <- nasqar_project_sanitize(name, fallback)
    path <- if (is.null(root)) file.path(tempdir(), app, safe) else file.path(root, safe)
    if (create) dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    path
}

nasqar_project_state_file <- function(dir) file.path(dir, "state.rds")

# A project name that does not clash with an existing project of another
# session. Every session starts with the same default name, so the first
# write of a session would otherwise overwrite whatever that name holds.
# `owned` is the directory this session already writes to (or restored):
# writing there again is fine. Otherwise an existing project gets a numeric
# suffix (name-2, name-3, ...).
nasqar_project_unique_name <- function(app, name, legacy_env = NULL, owned = NULL,
                                       fallback = "project") {
    root <- nasqar_project_app_root(app, legacy_env)
    safe <- nasqar_project_sanitize(name, fallback)
    if (is.null(root)) return(safe)
    usable <- function(candidate) {
        dir <- file.path(root, candidate)
        !is.null(owned) && identical(normalizePath(dir, winslash = "/", mustWork = FALSE),
                                     normalizePath(owned, winslash = "/", mustWork = FALSE)) ||
            !file.exists(nasqar_project_state_file(dir))
    }
    if (usable(safe)) return(safe)
    i <- 2L
    while (!usable(paste0(safe, "-", i))) i <- i + 1L
    paste0(safe, "-", i)
}

# Roots a Browse dialog may show: the read-only data root and the private
# space itself, so results saved by one application (a merged count matrix, a
# Seurat object, a DESeq2 table) can be picked as the input of another.
nasqar_browse_roots <- function() {
    roots <- character(0)
    # NASQAR_PRIVATE_ROOT: the private folder as a whole (data/ and projects/
    # inside it); browsing starts there and can go into either.
    private_root <- Sys.getenv("NASQAR_PRIVATE_ROOT", unset = "")
    if (nzchar(private_root) && dir.exists(private_root)) {
        roots["NASQAR2 private space"] <- normalizePath(private_root, winslash = "/", mustWork = FALSE)
        return(roots)
    }
    data_root <- Sys.getenv("NASQAR_DATA_ROOT", unset = "")
    if (nzchar(data_root) && dir.exists(data_root)) {
        roots["Private data root"] <- normalizePath(data_root, winslash = "/", mustWork = FALSE)
    }
    project_root <- Sys.getenv("NASQAR_PROJECT_ROOT", unset = "")
    if (nzchar(project_root) && dir.exists(project_root)) {
        roots["Private space (projects)"] <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
    }
    roots
}

# Resolve a path a user typed or browsed to in a private instance: it must
# exist and lie inside the private space (data root or projects). `directory`
# is TRUE for a directory, FALSE for a file, NA for either. `what` names the
# expected input in messages ("counts file", "FASTQ directory", ...).
nasqar_private_path <- function(path, directory = FALSE, what = if (isTRUE(directory)) "directory" else "file") {
    if (!nzchar(Sys.getenv("NASQAR_DATA_ROOT", unset = ""))) {
        stop("This instance has no private space configured.", call. = FALSE)
    }
    if (is.null(path) || !nzchar(trimws(path))) {
        stop(sprintf("Enter the path of a %s in the private space.", what), call. = FALSE)
    }
    resolved <- tryCatch(normalizePath(trimws(path), winslash = "/", mustWork = TRUE),
                         error = function(error) NULL)
    if (is.null(resolved)) stop(sprintf("The %s does not exist or is not readable: %s", what, trimws(path)), call. = FALSE)
    if (!nasqar_path_inside_roots(resolved)) stop(sprintf("The %s must be in the private space.", what), call. = FALSE)
    if (isTRUE(directory) && !dir.exists(resolved)) stop(sprintf("Select a directory for the %s.", what), call. = FALSE)
    if (isFALSE(directory) && dir.exists(resolved)) stop(sprintf("Select a file for the %s, not a directory.", what), call. = FALSE)
    resolved
}

# TRUE when a resolved path lies inside one of the browse roots.
nasqar_path_inside_roots <- function(resolved, roots = nasqar_browse_roots()) {
    for (var in c("NASQAR_DATA_ROOT", "NASQAR_PROJECT_ROOT")) {   # always allowed, even when nested
        extra <- Sys.getenv(var, unset = "")
        if (nzchar(extra) && dir.exists(extra)) roots[var] <- normalizePath(extra, winslash = "/", mustWork = FALSE)
    }
    if (!length(roots)) return(FALSE)
    any(vapply(roots, function(root) {
        identical(resolved, root) || startsWith(resolved, paste0(root, "/"))
    }, logical(1)))
}

# Merge fields into the project's state.rds (created on first call).
nasqar_project_state_write <- function(dir, app, name, stage, ...) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    state <- nasqar_project_state_read(dir)
    now <- Sys.time()
    if (is.null(state)) {
        state <- list(app = app, name = name, created = now,
                      nasqar_state_version = 1L)
    }
    updates <- list(...)
    for (field in names(updates)) state[[field]] <- updates[[field]]
    state$stage <- stage
    state$updated <- now
    temporary <- tempfile(pattern = "state-", tmpdir = dir)
    saveRDS(state, temporary)
    file.rename(temporary, nasqar_project_state_file(dir))
    nasqar_project_json_write(dir, state)
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) session$userData$nasqar_project_last_dir <- dir
    invisible(state)
}

# Human-readable companion of state.rds: the recorded settings plus a snapshot
# of every UI input at the time of writing, so the choices behind a result can
# be read without R (methods sections, provenance, sharing with colleagues).
nasqar_project_json_write <- function(dir, state) {
    session <- shiny::getDefaultReactiveDomain()
    input <- if (!is.null(session)) session$userData$nasqar_project_input else NULL
    snapshot <- if (!is.null(input)) nasqar_input_snapshot(input) else NULL
    record <- list(
        application = state$app, project = state$name, stage = state$stage,
        created = format(state$created, "%Y-%m-%d %H:%M:%S %Z"),
        updated = format(state$updated, "%Y-%m-%d %H:%M:%S %Z"),
        settings = state[setdiff(names(state), c("app", "name", "stage", "created", "updated", "nasqar_state_version"))],
        inputs = snapshot,
        nasqar2 = list(state_version = state$nasqar_state_version, r = R.version.string)
    )
    json <- tryCatch(
        jsonlite::toJSON(record, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null",
                         digits = NA, force = TRUE),
        error = function(error) NULL
    )
    if (is.null(json)) return(invisible(NULL))
    temporary <- tempfile(pattern = "state-", tmpdir = dir, fileext = ".json")
    writeLines(json, temporary)
    file.rename(temporary, file.path(dir, "state.json"))
    invisible(NULL)
}

# Current values of the ordinary UI inputs: scalars and short vectors, without
# uploads, browse-dialog internals, table selections, buttons and the project
# controls themselves.
nasqar_input_snapshot <- function(input) {
    values <- if (inherits(input, "reactivevalues")) {
        tryCatch(shiny::reactiveValuesToList(input), error = function(error) list())
    } else {
        as.list(input)
    }
    skip <- "^(nasqar_|browse_|shinyjs|keepAlive|\\.clientdata)|(_rows_|_cell_|_columns_|_state$|_search$|_clicked$|_selected$|_hover$|_brush$|_dblclick$)"
    out <- list()
    for (id in sort(names(values))) {
        if (grepl(skip, id)) next
        value <- values[[id]]
        if (is.null(value) || is.function(value) || is.environment(value)) next
        if (is.data.frame(value) || is.list(value)) next          # uploads, shinyFiles selections
        if (!is.atomic(value) || length(value) > 50L) next
        if (inherits(value, "shinyActionButtonValue")) next
        out[[id]] <- value
    }
    out
}

nasqar_project_state_read <- function(dir) {
    file <- nasqar_project_state_file(dir)
    if (!file.exists(file)) return(NULL)
    tryCatch(readRDS(file), error = function(error) NULL)
}

# Size, mtime and path of an input file, used to detect that a restored
# project's source data changed underneath it.
nasqar_file_signature <- function(path) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
    info <- file.info(path)
    list(path = normalizePath(path, winslash = "/", mustWork = FALSE),
         size = unname(info$size), mtime = as.numeric(info$mtime))
}

nasqar_signature_matches <- function(signature, path) {
    current <- nasqar_file_signature(path)
    !is.null(signature) && !is.null(current) &&
        identical(signature$size, current$size) &&
        isTRUE(abs(signature$mtime - current$mtime) < 1)
}

# All projects of an application that have a state.rds, newest first.
nasqar_project_list <- function(app, legacy_env = NULL) {
    empty <- data.frame(name = character(0), stage = character(0), input = character(0),
                        updated = as.POSIXct(character(0)), dir = character(0),
                        stringsAsFactors = FALSE)
    root <- nasqar_project_app_root(app, legacy_env)
    if (is.null(root)) return(empty)
    dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
    rows <- lapply(dirs, function(dir) {
        state <- nasqar_project_state_read(dir)
        if (is.null(state)) return(NULL)
        data.frame(
            name = basename(dir),
            stage = as.character(state$stage %||% ""),
            input = as.character(state$input_label %||% ""),
            updated = as.POSIXct(state$updated %||% file.info(dir)$mtime),
            dir = dir,
            stringsAsFactors = FALSE
        )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (!length(rows)) return(empty)
    listing <- do.call(rbind, rows)
    listing[order(listing$updated, decreasing = TRUE), , drop = FALSE]
}

# Choices for a select input: value = project name, label = name, input, stage, date.
nasqar_project_choices <- function(listing) {
    if (!nrow(listing)) return(character(0))
    labels <- sprintf("%s  (%s%s, %s)", listing$name,
                      ifelse(nzchar(listing$input), paste0(listing$input, ", "), ""),
                      listing$stage, format(listing$updated, "%Y-%m-%d %H:%M"))
    stats::setNames(listing$name, labels)
}

# ---------------------------------------------------------------------------
# Project outputs: downloads are also kept in the project, with a gallery
# ---------------------------------------------------------------------------

nasqar_output_category <- function(filename) {
    if (grepl("\\.(png|pdf|svg|jpe?g|tiff?)$", filename, ignore.case = TRUE)) "plots" else "downloads"
}

# Copy a file produced for download into the current project (the project last
# written or restored in this session) and record it in state.rds/state.json.
nasqar_project_save_output <- function(file, filename, session = shiny::getDefaultReactiveDomain()) {
    dir <- if (!is.null(session)) session$userData$nasqar_project_last_dir else NULL
    if (is.null(dir) || !dir.exists(dir) || !file.exists(file)) return(invisible(NULL))
    category <- nasqar_output_category(filename)
    target_dir <- file.path(dir, category)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    target <- file.path(target_dir, basename(filename))
    if (!file.copy(file, target, overwrite = TRUE)) return(invisible(NULL))
    state <- nasqar_project_state_read(dir)
    if (!is.null(state)) {
        outputs <- state$outputs
        outputs <- outputs[vapply(outputs, function(o) !identical(o$path, file.path(category, basename(filename))), logical(1))]
        outputs[[length(outputs) + 1L]] <- list(name = basename(filename), path = file.path(category, basename(filename)),
                                                category = category, saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        nasqar_project_state_write(dir, state$app, state$name, state$stage, outputs = outputs)
    }
    invisible(target)
}

# Drop-in replacement for shiny::downloadHandler: identical for the browser,
# plus the copy into the project in a private instance.
nasqar_download_handler <- function(filename, content, contentType = NULL, outputArgs = list()) {
    session <- shiny::getDefaultReactiveDomain()
    shiny::downloadHandler(
        filename = filename,
        content = function(file) {
            result <- content(file)
            name <- tryCatch(if (is.function(filename)) filename() else filename, error = function(e) basename(file))
            tryCatch(nasqar_project_save_output(file, name, session), error = function(e) NULL)
            result
        },
        contentType = contentType,
        outputArgs = outputArgs
    )
}

nasqar_project_outputs_ui <- function() {
    shiny::div(
        id = "nasqar_project_outputs_panel",
        shiny::tags$hr(),
        shiny::tags$strong("Saved plots and files of this project"),
        shiny::helpText("Every figure or table you download is also kept in the project folder (plots/ and downloads/)."),
        shiny::uiOutput("nasqar_project_outputs")
    )
}

# Gallery of the current project's plots/ and downloads/ (polled, so files
# saved a moment ago appear without reloading). Files are served under a
# session-specific resource path, private instances only.
nasqar_project_outputs_server <- function(input, output, session) {
    listing <- shiny::reactivePoll(3000, session,
        checkFunc = function() {
            dir <- session$userData$nasqar_project_last_dir
            if (is.null(dir)) return("")
            files <- list.files(file.path(dir, c("plots", "downloads")), full.names = TRUE)
            paste(dir, paste(files, file.info(files)$mtime, collapse = ";"))
        },
        valueFunc = function() {
            dir <- session$userData$nasqar_project_last_dir
            if (is.null(dir) || !dir.exists(dir)) return(NULL)
            rows <- lapply(c("plots", "downloads"), function(category) {
                files <- list.files(file.path(dir, category), full.names = TRUE)
                if (!length(files)) return(NULL)
                data.frame(category = category, name = basename(files), path = files,
                           modified = file.info(files)$mtime, stringsAsFactors = FALSE)
            })
            rows <- rows[!vapply(rows, is.null, logical(1))]
            list(dir = dir, files = if (length(rows)) do.call(rbind, rows) else NULL)
        })
    # Only plots/ and downloads/ are served (never state files or caches), under
    # session-specific prefixes that are unregistered when the session ends.
    prefix <- paste0("nasqar-project-", session$token)
    served <- new.env(parent = emptyenv())
    serve_category <- function(dir, category) {
        name <- paste0(prefix, "-", category)
        path <- file.path(dir, category)
        if (!identical(served[[category]], path) && dir.exists(path)) {
            shiny::addResourcePath(name, path)
            served[[category]] <- path
        }
        name
    }
    session$onSessionEnded(function() {
        for (category in c("plots", "downloads")) {
            if (!is.null(served[[category]])) {
                tryCatch(shiny::removeResourcePath(paste0(prefix, "-", category)), error = function(e) NULL)
            }
        }
    })
    output$nasqar_project_outputs <- shiny::renderUI({
        current <- listing()
        if (is.null(current)) return(shiny::helpText("No project yet: load data first."))
        if (is.null(current$files)) return(shiny::helpText("Nothing saved yet: downloads will appear here."))
        files <- current$files[order(current$files$modified, decreasing = TRUE), , drop = FALSE]
        cards <- lapply(seq_len(nrow(files)), function(i) {
            f <- files[i, ]
            href <- paste0(serve_category(current$dir, f$category), "/", utils::URLencode(f$name, reserved = TRUE))
            preview <- if (grepl("\\.(png|jpe?g)$", f$name, ignore.case = TRUE)) {
                shiny::tags$img(src = href, style = "max-width:100%; max-height:140px; display:block; margin:auto;")
            } else {
                shiny::div(style = "height:140px; display:flex; align-items:center; justify-content:center; color:#888;",
                           shiny::icon(if (f$category == "plots") "file-image" else "file-alt", class = "fa-3x"))
            }
            shiny::div(style = "width:200px; border:1px solid #ddd; border-radius:4px; padding:6px;",
                preview,
                shiny::tags$a(href = href, target = "_blank", download = f$name, f$name,
                              style = "display:block; margin-top:4px; word-break:break-all; font-size:12px;"),
                shiny::tags$small(class = "text-muted", format(f$modified, "%Y-%m-%d %H:%M")))
        })
        shiny::div(style = "display:flex; flex-wrap:wrap; gap:10px;", cards)
    })
}

# ---------------------------------------------------------------------------
# Shared UI and server pieces
# ---------------------------------------------------------------------------

# Project name box plus the restore picker. Both only appear in a private
# instance; the server side removes them otherwise.
nasqar_project_ui <- function(default_name, label = "Persistent project name",
                              help = "Outputs are kept under this name in the private space, so they survive the session and can be restored later.",
                              name_input = TRUE) {
    shiny::tagList(
        shiny::div(
            id = "nasqar_project_panel",
            shiny::tags$hr(),
            shiny::tags$strong("Reload a previous analysis"),
            shiny::helpText(
                "Every analysis run on this instance is saved as a project in the private space.",
                "Pick one and press Restore: its input, settings and results come back",
                "without recomputing (you can then change settings and rerun)."
            ),
            shiny::div(
                style = "display:flex; gap:8px; align-items:flex-end;",
                shiny::div(style = "flex:1;",
                    shiny::selectInput("nasqar_project_restore", "Saved projects on this instance",
                                       choices = c("No saved projects yet" = ""))),
                shiny::div(style = "margin-bottom:15px; display:flex; gap:6px;",
                    shiny::actionButton("nasqar_project_restore_btn", "Restore", class = "btn-primary"),
                    shiny::actionButton("nasqar_project_refresh", NULL, icon = shiny::icon("sync"),
                                        class = "btn-default", title = "Refresh the list"))
            ),
            shiny::tags$small(class = "text-muted", shiny::textOutput("nasqar_project_root_note", inline = TRUE)),
            if (isTRUE(name_input)) shiny::tags$hr(),
            if (isTRUE(name_input)) shiny::tags$strong("Start or continue a project"),
            if (isTRUE(name_input)) shiny::textInput("nasqar_project_name", label, value = default_name),
            if (isTRUE(name_input)) shiny::helpText(help),
            nasqar_project_outputs_ui()
        )
    )
}

# Wire the picker. on_restore(state, dir) is called with the saved state; it
# returns NULL on success or an error message. Returns a function that
# refreshes the picker, to call after the application writes a new state.
nasqar_project_server <- function(input, output, session, app, legacy_env = NULL,
                                  on_restore) {
    session$userData$nasqar_project_input <- input   # for state.json snapshots
    root <- nasqar_project_app_root(app, legacy_env)
    if (is.null(root)) {
        shiny::removeUI(selector = "#nasqar_project_panel")
        return(invisible(function() NULL))
    }
    # Keep state.json current: whenever any input changes (settled for 3 s),
    # rewrite the snapshot of the project last written in this session, so the
    # file reflects the settings behind what is on screen, not only the moment
    # a stage completed. Only the JSON is touched; state.rds stays as recorded.
    input_signature <- shiny::reactive({
        snapshot <- nasqar_input_snapshot(input)
        paste(names(snapshot), vapply(snapshot, function(v) paste(format(v), collapse = ","), character(1)), collapse = ";")
    })
    settled <- shiny::debounce(input_signature, 3000)
    shiny::observeEvent(settled(), {
        dir <- session$userData$nasqar_project_last_dir
        if (is.null(dir) || !dir.exists(dir)) return()
        state <- nasqar_project_state_read(dir)
        if (!is.null(state)) nasqar_project_json_write(dir, state)
    }, ignoreInit = TRUE)
    refresh <- function(select = NULL) {
        choices <- nasqar_project_choices(nasqar_project_list(app, legacy_env))
        if (!length(choices)) {
            shiny::updateSelectInput(session, "nasqar_project_restore",
                                     choices = c("No saved projects yet" = ""))
        } else {
            shiny::updateSelectInput(session, "nasqar_project_restore", choices = choices,
                                     selected = if (!is.null(select) && select %in% choices) select else choices[[1]])
        }
    }
    output$nasqar_project_root_note <- shiny::renderText(paste("Projects are stored under", root))
    nasqar_project_outputs_server(input, output, session)
    shiny::observeEvent(input$nasqar_project_refresh, refresh(), ignoreInit = TRUE)
    refresh()
    shiny::observeEvent(input$nasqar_project_restore_btn, {
        name <- input$nasqar_project_restore
        if (is.null(name) || !nzchar(name)) return()
        dir <- nasqar_project_dir(app, name, legacy_env, create = FALSE)
        state <- nasqar_project_state_read(dir)
        if (is.null(state)) {
            shiny::showNotification("This project has no saved state to restore.", type = "error")
            return()
        }
        problem <- tryCatch(on_restore(state, dir), error = function(error) conditionMessage(error))
        if (!is.null(problem) && nzchar(problem)) {
            shiny::showNotification(paste("Could not restore the project:", problem), type = "error", duration = 15)
        } else {
            # the restored project is now the current one: settings changes refresh its state.json
            session$userData$nasqar_project_last_dir <- dir
            nasqar_project_json_write(dir, state)
            if (!is.null(input$nasqar_project_name)) {
                shiny::updateTextInput(session, "nasqar_project_name", value = name)
            }
            shiny::showNotification(sprintf("Project '%s' restored (%s).", name, state$stage %||% "saved"),
                                    type = "message", duration = 8)
        }
    })
    invisible(refresh)
}

# ---------------------------------------------------------------------------
# Input checking: a wrong file must explain itself, never crash the application
# ---------------------------------------------------------------------------

# TRUE when the first bytes (after gzip decompression, which R applies
# transparently to .gz tables) are not text: NUL bytes or mostly control bytes.
nasqar_file_is_binary <- function(path) {
    bytes <- tryCatch({
        con <- gzfile(path, "rb")
        on.exit(close(con))
        readBin(con, "raw", 4096L)
    }, error = function(error) raw(0))
    if (!length(bytes)) return(FALSE)
    values <- as.integer(bytes)
    any(values == 0L) || mean(values < 32L & !values %in% c(9L, 10L, 13L)) > 0.1
}

# First lines of a file as text, for the "this is what was received" preview.
nasqar_file_head <- function(path, n = 6L, width = 160L) {
    lines <- tryCatch(readLines(path, n = n, warn = FALSE), error = function(error) character(0))
    lines <- iconv(lines, to = "UTF-8", sub = "?")
    ifelse(nchar(lines) > width, paste0(substr(lines, 1, width), " ..."), lines)
}

# Report an unusable input: a modal saying what the application expects and
# showing the first lines received, then a validation error. A validation
# error is displayed in outputs and is silent in observers, so the session
# survives whatever was uploaded.
nasqar_input_problem <- function(message, expected, path = NULL, label = "file") {
    preview <- if (!is.null(path) && file.exists(path)) nasqar_file_head(path) else character(0)
    session <- shiny::getDefaultReactiveDomain()
    if (!is.null(session)) {
        shiny::showModal(shiny::modalDialog(
            title = paste("This", label, "cannot be used"),
            shiny::p(shiny::strong(message)),
            shiny::p(sprintf("What the application expects of a %s:", label)),
            shiny::tags$ul(lapply(expected, shiny::tags$li)),
            if (length(preview)) shiny::tagList(
                shiny::p(sprintf("First lines of the %s received:", label)),
                shiny::tags$pre(style = "max-height:240px; overflow:auto; font-size:11px; white-space:pre;",
                                paste(preview, collapse = "\n"))
            ),
            size = "l", easyClose = TRUE, footer = shiny::modalButton("Close")
        ))
    }
    shiny::validate(shiny::need(FALSE, paste0(message, " Expected: ", paste(expected, collapse = "; "), ".")))
}

# Read a delimited table (tab, comma or semicolon, detected from the header
# line) and check it with `check(df)`, which returns NULL or a message.
# Every failure goes through nasqar_input_problem(). `...` reaches read.csv.
nasqar_read_table <- function(path, expected, check = NULL, label = "file", header = TRUE, ...) {
    if (is.null(path) || !file.exists(path)) {
        nasqar_input_problem("No file was received.", expected, NULL, label)
    }
    if (nasqar_file_is_binary(path)) {
        nasqar_input_problem("The file is not a plain-text table (it looks like a binary file such as .rds, .xlsx or .bam).",
                             expected, NULL, label)
    }
    first_line <- nasqar_file_head(path, n = 1L, width = .Machine$integer.max)
    if (!length(first_line) || !nzchar(trimws(first_line))) {
        nasqar_input_problem("The file is empty.", expected, path, label)
    }
    separators <- c("\t", ",", ";")
    fields <- vapply(separators, function(sep) length(strsplit(first_line, sep, fixed = TRUE)[[1]]), integer(1))
    sep <- separators[which.max(fields)]
    df <- tryCatch(utils::read.csv(path, sep = sep, header = header, stringsAsFactors = FALSE, ...),
                   error = function(error) error)
    if (inherits(df, "error")) {
        nasqar_input_problem(paste("The file could not be parsed:", conditionMessage(df)), expected, path, label)
    }
    if (!is.null(check)) {
        problem <- check(df)
        if (!is.null(problem)) nasqar_input_problem(problem, expected, path, label)
    }
    df
}

# Check for a counts-style table: `id_columns` identifier column(s) followed
# by numeric sample columns. Returns NULL or a message.
nasqar_check_count_table <- function(df, id_columns = 1L, min_samples = 1L, unique_ids = TRUE) {
    if (!nrow(df)) return("The table has a header but no rows.")
    if (ncol(df) < id_columns + min_samples) {
        return(sprintf("Found %d column(s); need %d identifier column(s) plus at least %d sample column(s).",
                       ncol(df), id_columns, min_samples))
    }
    samples <- df[, -seq_len(id_columns), drop = FALSE]
    not_numeric <- names(samples)[!vapply(samples, is.numeric, logical(1))]
    if (length(not_numeric)) {
        shown <- head(not_numeric, 5)
        return(sprintf("Sample column(s) %s%s are not numeric; every column after the identifier(s) must hold counts.",
                       paste(shQuote(shown), collapse = ", "), if (length(not_numeric) > 5) ", ..." else ""))
    }
    if (unique_ids && anyDuplicated(df[[1]])) {
        dup <- unique(df[[1]][duplicated(df[[1]])])
        return(sprintf("The first column must hold unique identifiers; %d are repeated (e.g. %s).",
                       length(dup), paste(shQuote(head(dup, 3)), collapse = ", ")))
    }
    NULL
}

# ---------------------------------------------------------------------------
# Services that need the internet (KEGG, pathway maps): on an offline server,
# e.g. a cluster compute node, say so instead of showing a curl error.
# ---------------------------------------------------------------------------

nasqar_network_error <- function(error) {
    grepl("curl|connection|reset by peer|timed? ?out|could not resolve|resolve host|network|recv failure|cannot open url|failed to connect|unreachable|no route",
          conditionMessage(error), ignore.case = TRUE)
}

nasqar_offline_message <- function(service = "KEGG") {
    sprintf(paste("%s needs internet access, and this server could not reach it: either it is offline",
                  "(cluster compute nodes usually are) or the connection failed. Results that do not need %s",
                  "remain available; otherwise rerun on a machine with internet access."), service, service)
}

# Run `expr` with R's download timeout capped at `seconds`, so an offline
# server fails fast instead of hanging the session.
nasqar_with_timeout <- function(expr, seconds = 20) {
    old <- getOption("timeout")
    options(timeout = min(old, seconds))
    on.exit(options(timeout = old), add = TRUE)
    expr
}

if (!exists("%||%", mode = "function")) {
    `%||%` <- function(x, y) if (is.null(x)) y else x
}
