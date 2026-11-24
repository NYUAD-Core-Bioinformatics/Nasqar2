# NASQAR2 private-space projects: one convention for every NASQAR2 application.
#
#   <project root>/<project>/<App>/     e.g. /private/projects/avo-study/DESeq2Shiny
#       state.rds                       what was loaded, the settings, the stage reached
#       ... application outputs ...
#
# A project is one study: every application that works on it keeps its part in
# its own folder under the project's name, so the count matrix, the
# differential expression and the enrichment of one study sit together.
#
# The root comes from NASQAR_CONTAINER_PROJECT_ROOT (set by run-nasqar2.sh and the Slurm
# launcher). Projects made before this layout, <project root>/<App>/<name>,
# are still listed and reopened where they are; renaming one moves it into the
# new layout. When it is not set, or the directory is missing or read-only,
# there is no private space: the application works in temporary storage and offers no restore.
#
# This file is maintained in common/nasqar-project.R and copied into each
# application's src/ by common/sync-project-helper.sh. Edit the common copy.

# Folder names of the applications; a project cannot be called like one of them.
nasqar_project_apps <- c("DESeq2Shiny", "ClusterProfShinyORA", "ClusterProfShinyGSEA", "VariantAnnotation",
                         "DADA2", "ATACseqQC", "GeneCountMerger", "mergeFPKMs", "Monocle3", "SeuratV5")

nasqar_usable_dir <- function(path) {
    if (!nzchar(path)) return(NULL)
    if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (dir.exists(path) && file.access(path, 2) == 0) normalizePath(path, winslash = "/", mustWork = FALSE) else NULL
}

# The shared project root (NASQAR_CONTAINER_PROJECT_ROOT), or NULL.
nasqar_project_root <- function() nasqar_usable_dir(Sys.getenv("NASQAR_CONTAINER_PROJECT_ROOT", unset = ""))

# Where this application's projects were kept before the project-first layout.
nasqar_project_app_root <- function(app) {
    root <- nasqar_project_root()
    if (is.null(root)) NULL else nasqar_usable_dir(file.path(root, app))
}

nasqar_project_available <- function(app) {
    !is.null(nasqar_project_root())
}

nasqar_project_sanitize <- function(name, fallback = "project") {
    name <- gsub("[^A-Za-z0-9._-]+", "-", trimws(as.character(name %||% "")))
    name <- gsub("^[.-]+|[.-]+$", "", name)
    if (name %in% nasqar_project_apps) name <- paste0(name, "-project")   # not an application's folder name
    if (!nzchar(name)) fallback else name
}

# The project name a project directory stands for: <project>/<App> in the
# project-first layout, <App>/<name> or <dir>/<name> otherwise.
nasqar_project_name <- function(dir) {
    dir <- sub("/+$", "", dir)
    if (basename(dir) %in% nasqar_project_apps) basename(dirname(dir)) else basename(dir)
}

# The directory of a named project of this application, existing or not: the
# old-layout folder when the project was made there, the project-first one
# otherwise. NULL when there is no private space.
nasqar_project_locate <- function(app, safe) {
    root <- nasqar_project_root()
    if (is.null(root)) return(NULL)
    current <- file.path(root, safe, app)
    old <- file.path(root, app, safe)
    if (!file.exists(nasqar_project_state_file(current)) && file.exists(nasqar_project_state_file(old))) old else current
}

# Directory of a named project; a temporary directory when there is no
# private space. The name is sanitised to a safe single path component.
nasqar_project_dir <- function(app, name, create = TRUE,
                               fallback = "project") {
    safe <- nasqar_project_sanitize(name, fallback)
    path <- nasqar_project_locate(app, safe)
    if (is.null(path)) path <- file.path(tempdir(), app, safe)
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
nasqar_project_unique_name <- function(app, name, owned = NULL,
                                       fallback = "project") {
    safe <- nasqar_project_sanitize(name, fallback)
    if (!nasqar_project_available(app)) return(safe)
    usable <- function(candidate) {
        dir <- nasqar_project_locate(app, candidate)
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
# When a project is open in the session, its folder comes first ("This
# project"), so a Browse dialog opens there; the rest of the private space is
# one choice away in the same dialog. Applications pass the function itself as
# shinyFiles roots, so the list follows the project chosen.
nasqar_browse_roots <- function() {
    roots <- nasqar_browse_roots_fixed()
    session <- shiny::getDefaultReactiveDomain()
    project <- if (!is.null(session)) session$userData$nasqar_project_browse_dir else NULL
    if (!is.null(project) && dir.exists(project)) {
        roots <- c("This project" = normalizePath(project, winslash = "/", mustWork = FALSE), roots)
    }
    roots
}

nasqar_browse_roots_fixed <- function() {
    roots <- character(0)
    data_root <- Sys.getenv("NASQAR_CONTAINER_DATA_ROOT", unset = "")
    if (nzchar(data_root) && dir.exists(data_root)) {
        roots["Private data root"] <- normalizePath(data_root, winslash = "/", mustWork = FALSE)
    }
    project_root <- Sys.getenv("NASQAR_CONTAINER_PROJECT_ROOT", unset = "")
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
    if (!nzchar(Sys.getenv("NASQAR_CONTAINER_DATA_ROOT", unset = ""))) {
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
nasqar_path_inside_roots <- function(resolved, roots = nasqar_browse_roots_fixed()) {
    for (var in c("NASQAR_CONTAINER_DATA_ROOT", "NASQAR_CONTAINER_PROJECT_ROOT")) {   # always allowed, even when nested
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
    dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)   # the real folder, not a renamed project's alias
    state <- nasqar_project_state_read(dir)
    now <- Sys.time()
    if (is.null(state)) {
        state <- list(app = app, name = name, created = now,
                      nasqar_state_version = 1L)
    }
    state$name <- nasqar_project_name(dir)
    session <- shiny::getDefaultReactiveDomain()
    updates <- list(...)
    # a project renamed in this session is still reached under its old path:
    # record the real one
    for (alias in if (!is.null(session)) session$userData$nasqar_project_aliases else NULL) {
        updates <- nasqar_rewrite_paths(updates, alias, dir)
    }
    for (field in names(updates)) state[[field]] <- updates[[field]]
    state$stage <- stage
    state$updated <- now
    temporary <- tempfile(pattern = "state-", tmpdir = dir)
    saveRDS(state, temporary)
    file.rename(temporary, nasqar_project_state_file(dir))
    nasqar_project_json_write(dir, state)
    if (!is.null(session)) nasqar_project_set_current(session, dir)
    invisible(state)
}

# The project this session writes to. Kept in session$userData for plain
# reads and mirrored in a reactive value for the on-screen project badge.
nasqar_project_set_current <- function(session, dir) {
    session$userData$nasqar_project_last_dir <- dir
    current <- session$userData$nasqar_project_current
    if (is.function(current) && !identical(shiny::isolate(current()), dir)) current(dir)
    invisible(dir)
}

# TRUE for a symbolic link (Sys.readlink() gives NA for a path that does not exist).
nasqar_is_link <- function(path) {
    target <- Sys.readlink(path)
    !is.na(target) & nzchar(target)
}

# "&project=<name>" for a link that hands data to another application, so the
# receiving application continues in the same project ("" outside a project).
nasqar_project_handoff_query <- function(session = shiny::getDefaultReactiveDomain()) {
    dir <- if (!is.null(session)) session$userData$nasqar_project_last_dir else NULL
    if (is.null(dir) || is.null(nasqar_project_root())) return("")
    paste0("&project=", utils::URLencode(nasqar_project_name(dir), reserved = TRUE))
}

# Replace the directory prefix `old` by `new` in every path found in x
# (character vectors, at any depth of a list).
nasqar_rewrite_paths <- function(x, old, new) {
    if (is.character(x)) {
        inside <- !is.na(x) & (x == old | startsWith(x, paste0(old, "/")))
        x[inside] <- paste0(new, substring(x[inside], nchar(old) + 1L))
        return(x)
    }
    if (is.list(x) && !is.data.frame(x)) {
        attributes_kept <- attributes(x)
        x <- lapply(x, nasqar_rewrite_paths, old = old, new = new)
        attributes(x) <- attributes_kept
    }
    x
}

# A project directory that may be renamed or deleted: a real directory (not
# the alias of a renamed project) directly under the application's root, with
# a state file. Returns its normalised path, or stops with a message.
nasqar_project_checked_dir <- function(app, name) {
    if (!nasqar_project_available(app)) stop("This instance has no private space.", call. = FALSE)
    safe <- nasqar_project_sanitize(name, "")
    if (!nzchar(safe) || !identical(safe, as.character(name))) stop("Not a project name.", call. = FALSE)
    dir <- nasqar_project_locate(app, safe)
    if (nasqar_is_link(dir) || !dir.exists(dir) || !file.exists(nasqar_project_state_file(dir))) {
        stop(sprintf("There is no saved project named '%s'.", safe), call. = FALSE)
    }
    dir
}

# Remove a project's folder <root>/<project> once no application has anything
# left in it (file.remove() only removes an empty directory).
nasqar_project_prune <- function(dir) {
    root <- nasqar_project_root()
    parent <- dirname(dir)
    if (!is.null(root) && identical(dirname(parent), root) && !basename(parent) %in% nasqar_project_apps &&
        dir.exists(parent) && !length(list.files(parent, all.files = TRUE, no.. = TRUE))) {
        suppressWarnings(file.remove(parent))
    }
    invisible(NULL)
}

# Rename a project: move this application's folder under the new project name
# and correct the paths recorded in its state. A project of the old layout
# moves into the project-first one. Returns the new directory.
nasqar_project_rename <- function(app, name, new_name) {
    old_dir <- nasqar_project_checked_dir(app, name)
    safe <- nasqar_project_sanitize(new_name, "")
    if (!nzchar(safe)) stop("Enter a name using letters, digits, '.', '_' or '-'.", call. = FALSE)
    if (identical(safe, nasqar_project_name(old_dir))) stop("That is already the name of this project.", call. = FALSE)
    new_dir <- file.path(nasqar_project_root(), safe, app)
    taken <- nasqar_project_locate(app, safe)
    if (file.exists(new_dir) || nasqar_is_link(new_dir) || file.exists(nasqar_project_state_file(taken)) || nasqar_is_link(taken)) {
        stop(sprintf("Project '%s' already has an analysis of this application.", safe), call. = FALSE)
    }
    dir.create(dirname(new_dir), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!file.rename(old_dir, new_dir)) stop("The project folder could not be renamed.", call. = FALSE)
    state <- nasqar_project_state_read(new_dir)
    if (!is.null(state)) {
        state <- nasqar_rewrite_paths(state, old_dir, new_dir)
        state$name <- safe
        state$renamed_from <- c(state$renamed_from, nasqar_project_name(old_dir))
        saveRDS(state, nasqar_project_state_file(new_dir))
        tryCatch(nasqar_project_json_write(new_dir, state), error = function(e) NULL)
    }
    new_dir
}

# Delete this application's part of a project for good.
nasqar_project_delete <- function(app, name) {
    dir <- nasqar_project_checked_dir(app, name)
    unlink(dir, recursive = TRUE, force = TRUE)
    if (dir.exists(dir)) stop("The project folder could not be deleted completely.", call. = FALSE)
    nasqar_project_prune(dir)
    invisible(TRUE)
}

# Number of files and total size of a project, for the delete confirmation.
nasqar_project_size <- function(dir) {
    files <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    bytes <- sum(file.info(files)$size, na.rm = TRUE)
    units <- c("bytes", "KB", "MB", "GB", "TB")
    i <- if (bytes > 0) min(length(units), floor(log(bytes, 1024)) + 1L) else 1L
    sprintf("%d files, %s %s", length(files), format(round(bytes / 1024^(i - 1), if (i > 1) 1 else 0), nsmall = 0), units[i])
}

# A project renamed while it is open stays reachable under its old path for
# the rest of the session, through a symbolic link beside it: whatever the
# application still holds in memory (open files, cached paths) keeps working,
# and nasqar_project_state_write() records the real path. The link goes when
# the session ends; one left behind by a killed process is removed by the
# next session (the marker names the process that made it).
nasqar_project_alias_marker <- function(target_dir, alias_dir) {
    file.path(target_dir, paste0(".nasqar-alias-", basename(dirname(alias_dir)), "--", basename(alias_dir)))
}

nasqar_project_alias_create <- function(alias_dir, target_dir) {
    if (file.exists(alias_dir) || nasqar_is_link(alias_dir)) unlink(alias_dir)
    dir.create(dirname(alias_dir), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    # relative, so the link also holds outside the container that made it
    relative <- if (identical(dirname(alias_dir), dirname(target_dir))) basename(target_dir)
                else file.path("..", basename(dirname(target_dir)), basename(target_dir))
    ok <- suppressWarnings(file.symlink(relative, alias_dir))
    if (isTRUE(ok)) writeLines(as.character(Sys.getpid()), nasqar_project_alias_marker(target_dir, alias_dir))
    isTRUE(ok)
}

nasqar_project_alias_remove <- function(alias_dir) {
    if (!nasqar_is_link(alias_dir)) return(invisible(FALSE))
    target <- normalizePath(alias_dir, winslash = "/", mustWork = FALSE)
    unlink(nasqar_project_alias_marker(target, alias_dir))
    unlink(alias_dir)
    nasqar_project_prune(alias_dir)
    invisible(TRUE)
}

# Every folder that may be a project of this application, in any layout.
nasqar_project_candidates <- function(app) {
    root <- nasqar_project_root()
    if (is.null(root)) return(character(0))
    studies <- setdiff(list.files(root), nasqar_project_apps)
    current <- file.path(root, studies, app)
    c(current[file.exists(current) | nasqar_is_link(current)], list.files(file.path(root, app), full.names = TRUE))
}

nasqar_project_alias_cleanup <- function(app) {
    for (path in nasqar_project_candidates(app)) {
        if (!nasqar_is_link(path)) next
        marker <- nasqar_project_alias_marker(normalizePath(path, winslash = "/", mustWork = FALSE), path)
        pid <- if (file.exists(marker)) suppressWarnings(readLines(marker, n = 1, warn = FALSE)) else ""
        alive <- length(pid) == 1 && grepl("^[0-9]+$", pid) && dir.exists(file.path("/proc", pid))
        if (!alive) nasqar_project_alias_remove(path)
    }
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

# Names of the applications as people know them.
nasqar_project_app_labels <- c(
    DESeq2Shiny = "DESeq2", ClusterProfShinyORA = "ORA", ClusterProfShinyGSEA = "GSEA",
    VariantAnnotation = "Variant Annotation", DADA2 = "DADA2", ATACseqQC = "ATACseqQC",
    GeneCountMerger = "Gene Count Merger", mergeFPKMs = "Merge FPKMs", Monocle3 = "Monocle 3",
    SeuratV5 = "Seurat v5")

nasqar_project_app_label <- function(app) {
    unname(ifelse(app %in% names(nasqar_project_app_labels), nasqar_project_app_labels[app], app))
}

# Every project of the instance, newest first, each with the folders of the
# applications that saved to it (apps: folder name -> directory; stages: the
# stage each reached). Projects of the old layout are included.
nasqar_project_studies <- function(app) {
    rows <- list()
    add <- function(name, a, dir) {
        if (!dir.exists(dir) || nasqar_is_link(dir)) return()
        state <- nasqar_project_state_read(dir)
        if (is.null(state)) return()
        entry <- rows[[name]] %||% list(name = name, apps = character(0), stages = character(0),
                                        updated = as.POSIXct(0, origin = "1970-01-01"))
        if (a %in% names(entry$apps)) return()   # the project-first folder wins over an old one
        entry$apps[a] <- dir
        entry$stages[a] <- as.character(state$stage %||% "saved")
        updated <- tryCatch(as.POSIXct(state$updated %||% file.info(dir)$mtime), error = function(e) file.info(dir)$mtime)
        if (isTRUE(updated > entry$updated)) entry$updated <- updated
        rows[[name]] <<- entry
    }
    root <- nasqar_project_root()
    if (is.null(root)) return(rows)
    for (study in setdiff(list.files(root), nasqar_project_apps)) {
        folder <- file.path(root, study)
        if (!dir.exists(folder) || nasqar_is_link(folder)) next
        for (a in intersect(list.files(folder), nasqar_project_apps)) add(study, a, file.path(folder, a))
    }
    for (a in intersect(list.files(root), nasqar_project_apps)) {          # old layout
        for (dir in list.files(file.path(root, a), full.names = TRUE)) add(basename(dir), a, dir)
    }
    if (!length(rows)) return(rows)
    rows[order(vapply(rows, function(r) as.numeric(r$updated), numeric(1)), decreasing = TRUE)]
}

# The newest file of another application's part of a project, e.g.
# nasqar_project_newest(apps, "GeneCountMerger", "^merged-counts\\.csv$").
nasqar_project_newest <- function(apps, from, pattern, subdir = NULL) {
    if (!from %in% names(apps)) return(NULL)
    folder <- unname(apps[[from]])
    if (!is.null(subdir)) folder <- file.path(folder, subdir)
    files <- list.files(folder, pattern = pattern, full.names = TRUE)
    if (!length(files)) return(NULL)
    files[which.max(file.info(files)$mtime)]
}

# The folder a Browse dialog opens in for a project: the project's own folder,
# or for a project of the old layout the folder of one of its applications.
nasqar_project_study_dir <- function(study, app) {
    if (is.null(study)) return(NULL)
    root <- nasqar_project_root()
    folder <- if (!is.null(root)) file.path(root, study$name) else NULL
    if (!is.null(folder) && dir.exists(folder) && !nasqar_is_link(folder)) return(folder)
    if (app %in% names(study$apps)) return(unname(study$apps[[app]]))
    if (length(study$apps)) unname(study$apps[[1]]) else NULL
}

# All projects of an application that have a state.rds, newest first.
nasqar_project_list <- function(app) {
    empty <- data.frame(name = character(0), stage = character(0), input = character(0),
                        updated = as.POSIXct(character(0)), dir = character(0),
                        stringsAsFactors = FALSE)
    if (!nasqar_project_available(app)) return(empty)
    dirs <- nasqar_project_candidates(app)
    dirs <- dirs[dir.exists(dirs) & !nasqar_is_link(dirs)]   # not the alias of a renamed project
    # a name used in both layouts: the project-first folder is the project
    dirs <- dirs[!duplicated(vapply(dirs, nasqar_project_name, character(1)))]
    rows <- lapply(dirs, function(dir) {
        state <- nasqar_project_state_read(dir)
        if (is.null(state)) return(NULL)
        data.frame(
            name = nasqar_project_name(dir),
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
    # remembered so nasqar_project_save_all() can write the same files
    if (!is.null(session)) {
        registry <- session$userData$nasqar_download_registry
        registry[[length(registry) + 1L]] <- list(filename = filename, content = content)
        session$userData$nasqar_download_registry <- registry
    }
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

# Write what the download buttons of the session would deliver straight into
# the current project (plots/ and downloads/), without a browser download.
# Each handler is run as if clicked; one whose figure or table does not exist
# yet fails its own checks and is skipped. keep(name) selects the files to
# write, rename(name) gives the name to store them under (e.g. without a
# timestamp, so saving again replaces the file), progress(i, n, name) reports
# each one. Returns the names saved.
nasqar_project_save_all <- function(session = shiny::getDefaultReactiveDomain(),
                                    keep = function(name) TRUE, rename = identity, progress = NULL) {
    dir <- session$userData$nasqar_project_last_dir
    if (is.null(dir) || !dir.exists(dir)) return(character(0))
    registry <- session$userData$nasqar_download_registry
    saved <- character(0)
    shiny::isolate(for (i in seq_along(registry)) {
        entry <- registry[[i]]
        name <- tryCatch(if (is.function(entry$filename)) entry$filename() else entry$filename,
                         error = function(e) NULL)
        if (is.null(name) || length(name) != 1 || is.na(name) || !nzchar(name)) next
        name <- basename(name)
        if (!isTRUE(keep(name))) next
        name <- basename(rename(name))
        if (name %in% saved) next
        if (is.function(progress)) progress(i, length(registry), name)
        temporary <- tempfile(fileext = paste0(".", tools::file_ext(name)))
        written <- tryCatch({
            entry$content(temporary)
            file.exists(temporary) && file.info(temporary)$size > 0
        }, error = function(e) FALSE)
        while (grDevices::dev.cur() > 1 && !written) grDevices::dev.off()   # a device left open by a failed plot
        if (isTRUE(written) && !is.null(tryCatch(nasqar_project_save_output(temporary, name, session),
                                                 error = function(e) NULL))) {
            saved <- c(saved, name)
        }
        unlink(temporary)
    })
    saved
}

# Project subfolders listed on the Project files page: figures and tables, and
# the uploaded input files an application keeps (inputs/).
nasqar_project_gallery_dirs <- c("plots", "downloads", "inputs")

# Body of a "Project files" box: the Save all to project button over the
# list of saved files. The button is wired by nasqar_project_outputs_server().
# save_all = FALSE leaves the button out (an application that saves its one
# result by itself has nothing more to write).
nasqar_project_files_ui <- function(save_all = TRUE) {
    if (!isTRUE(save_all)) return(nasqar_project_outputs_ui(heading = FALSE))
    shiny::tagList(
        shiny::actionButton("nasqar_project_save_all_btn", "Save all to project", icon = shiny::icon("floppy-disk"),
                            class = "btn-primary", style = "color:#fff; margin-bottom:6px;"),
        shiny::helpText("Writes every figure and table that exists right now into the project without downloading them.",
                        "Saving again replaces the earlier copies."),
        shiny::tags$hr(),
        nasqar_project_outputs_ui(heading = FALSE)
    )
}

nasqar_project_outputs_ui <- function(heading = TRUE) {
    shiny::div(
        id = "nasqar_project_outputs_panel",
        if (isTRUE(heading)) shiny::tags$hr(),
        if (isTRUE(heading)) shiny::tags$strong("Saved plots and files of this project"),
        shiny::helpText("Every figure or table you download is also kept in the project folder (plots/ and downloads/); uploaded input files the application keeps are in inputs/. Click a file to preview it."),
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
            files <- list.files(file.path(dir, nasqar_project_gallery_dirs), full.names = TRUE)
            paste(dir, paste(files, file.info(files)$mtime, collapse = ";"))
        },
        valueFunc = function() {
            dir <- session$userData$nasqar_project_last_dir
            if (is.null(dir) || !dir.exists(dir)) return(NULL)
            rows <- lapply(nasqar_project_gallery_dirs, function(category) {
                files <- list.files(file.path(dir, category), full.names = TRUE)
                if (!length(files)) return(NULL)
                data.frame(category = category, name = basename(files), path = files,
                           modified = file.info(files)$mtime, stringsAsFactors = FALSE)
            })
            rows <- rows[!vapply(rows, is.null, logical(1))]
            list(dir = dir, files = if (length(rows)) do.call(rbind, rows) else NULL)
        })
    # Only plots/, downloads/ and inputs/ are served (never state files or caches), under
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
        for (category in nasqar_project_gallery_dirs) {
            if (!is.null(served[[category]])) {
                tryCatch(shiny::removeResourcePath(paste0(prefix, "-", category)), error = function(e) NULL)
            }
        }
    })
    # Preview of one saved file in a dialog: figures and PDFs as they are, the
    # first rows of a table, the contents of a zip, the first lines of a script.
    # Only names inside the current project's plots/, downloads/ and inputs/ are accepted.
    shiny::observeEvent(input$nasqar_project_preview, {
        dir <- session$userData$nasqar_project_last_dir
        parts <- strsplit(as.character(input$nasqar_project_preview), "/", fixed = TRUE)[[1]]
        if (is.null(dir) || length(parts) != 2 || !parts[1] %in% nasqar_project_gallery_dirs) return()
        name <- basename(parts[2])
        path <- file.path(dir, parts[1], name)
        if (!file.exists(path)) return()
        href <- paste0(serve_category(dir, parts[1]), "/", utils::URLencode(name, reserved = TRUE))
        extension <- tolower(tools::file_ext(name))
        first_lines <- function(n) shiny::tags$pre(style = "max-height:60vh; overflow:auto;",
            paste(tryCatch(readLines(path, n = n, warn = FALSE), error = function(e) "This file cannot be read as text."),
                  collapse = "\n"))
        body <- if (extension %in% c("png", "jpg", "jpeg", "svg")) {
            shiny::tags$img(src = href, style = "max-width:100%; display:block; margin:auto;")
        } else if (extension == "pdf") {
            shiny::tags$iframe(src = href, style = "width:100%; height:70vh; border:0;")
        } else if (extension %in% c("csv", "tsv")) {
            rows <- tryCatch(utils::read.table(path, sep = if (extension == "csv") "," else "\t", header = TRUE,
                                               nrows = 25, check.names = FALSE, quote = "\"", comment.char = "",
                                               fill = TRUE, stringsAsFactors = FALSE),
                             error = function(e) NULL)
            if (is.null(rows)) first_lines(25) else shiny::tagList(
                shiny::helpText(sprintf("First %d rows, %d columns.", nrow(rows), ncol(rows))),
                shiny::div(style = "overflow:auto; max-height:60vh;",
                    shiny::tags$table(class = "table table-condensed table-striped", style = "font-size:12px;",
                        shiny::tags$thead(shiny::tags$tr(lapply(names(rows), shiny::tags$th))),
                        shiny::tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
                            shiny::tags$tr(lapply(rows[i, , drop = TRUE], function(value) shiny::tags$td(format(value))))
                        })))))
        } else if (extension == "zip") {
            contents <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
            if (is.null(contents)) shiny::helpText("This archive cannot be listed.") else shiny::tagList(
                shiny::helpText(sprintf("%d files in this archive:", nrow(contents))),
                shiny::tags$ul(lapply(contents$Name, shiny::tags$li)))
        } else if (extension %in% c("r", "rmd", "txt", "md", "json", "log")) {
            first_lines(80)
        } else {
            shiny::helpText("There is no preview for this file type; download it to open it.")
        }
        shiny::showModal(shiny::modalDialog(
            title = name, size = "l", easyClose = TRUE, body,
            footer = shiny::tagList(
                shiny::tags$a(href = href, target = "_blank", download = name, class = "btn btn-default",
                              shiny::icon("download"), " Download"),
                shiny::modalButton("Close"))
        ))
    }, ignoreInit = TRUE)

    # Save all to project (the button of nasqar_project_files_ui): figures and
    # tables, stored without the timestamp of their download name so that
    # saving again replaces them.
    shiny::observeEvent(input$nasqar_project_save_all_btn, {
        if (is.null(session$userData$nasqar_project_last_dir)) {
            shiny::showNotification("There is no project yet: load data first.", type = "warning", duration = 6)
            return()
        }
        saved <- shiny::withProgress(message = "Saving to the project...", value = 0, {
            nasqar_project_save_all(
                session,
                keep = function(name) tolower(tools::file_ext(name)) %in% c("pdf", "png", "csv", "tsv", "txt", "xlsx"),
                rename = function(name) {
                    name <- gsub("_[0-9]{8}_[0-9]{6}", "", name)
                    gsub("[-_]?[0-9]{4}-[0-9]{2}-[0-9]{2}", "", name)
                },
                progress = function(i, n, name) shiny::setProgress(value = i / n, detail = name))
        })
        shiny::showNotification(
            if (length(saved)) sprintf("%d files saved to the project.", length(saved))
            else "Nothing to save yet: run the analysis and draw the plots you want first.",
            type = if (length(saved)) "message" else "warning", duration = 8)
    }, ignoreInit = TRUE)

    output$nasqar_project_outputs <- shiny::renderUI({
        current <- listing()
        if (is.null(current)) return(shiny::helpText("No project yet: load data first."))
        if (is.null(current$files)) return(shiny::helpText("No files yet. Your analysis is saved automatically; figures, tables and exports you download are also kept here."))
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
            open_preview <- sprintf(
                "Shiny.setInputValue('nasqar_project_preview', %s, {priority: 'event'}); return false;",
                jsonlite::toJSON(paste0(f$category, "/", f$name), auto_unbox = TRUE))
            shiny::div(style = "width:200px; border:1px solid #ddd; border-radius:4px; padding:6px;",
                shiny::tags$a(href = "#", onclick = open_preview, title = "Preview",
                              style = "display:block; cursor:zoom-in;", preview),
                shiny::div(style = "margin-top:4px; word-break:break-all; font-size:12px;", f$name,
                           if (f$category == "inputs") shiny::span(class = "label label-default", style = "margin-left:4px;", "input")),
                shiny::div(style = "display:flex; justify-content:space-between; align-items:center; margin-top:2px;",
                    shiny::tags$small(class = "text-muted", format(f$modified, "%Y-%m-%d %H:%M")),
                    shiny::span(style = "font-size:12px; white-space:nowrap;",
                        shiny::tags$a(href = "#", onclick = open_preview, "Preview"), " \u00b7 ",
                        shiny::tags$a(href = href, target = "_blank", download = f$name, "Download"))))
        })
        shiny::div(style = "display:flex; flex-wrap:wrap; gap:10px;", cards)
    })
}

# ---------------------------------------------------------------------------
# Shared UI and server pieces
# ---------------------------------------------------------------------------

# The restore picker and the project name box. Both only appear in a private
# instance; the server side removes them otherwise. nasqar_project_ui() is the
# stacked panel; an application that lays the pieces out itself calls
# nasqar_project_restore_ui() / nasqar_project_name_ui() and wraps each in an
# element of class "nasqar-project-private" so it is removed the same way.
nasqar_project_restore_ui <- function(button_label = "Restore", help = TRUE, stacked = FALSE) {
    refresh_button <- shiny::actionButton("nasqar_project_refresh", NULL, icon = shiny::icon("sync"),
                                          class = "btn-default", title = "Refresh the list")
    manage_buttons <- shiny::tagList(
        shiny::actionButton("nasqar_project_rename_btn", NULL, icon = shiny::icon("pen"),
                            class = "btn-default", title = "Rename the selected project"),
        shiny::actionButton("nasqar_project_delete_btn", NULL, icon = shiny::icon("trash"),
                            class = "btn-default", title = "Delete the selected project"))
    if (isTRUE(stacked)) {
        # Full-width picker with the project name on one line and its details
        # (input, stage, date) on a second, then the button underneath: the
        # one-line labels wrap badly in a narrow column.
        split_label <- "var m = /^(.*?)\\s{2}\\((.*)\\)$/.exec(item.label || ''), name = m ? m[1] : (item.label || ''), meta = m ? m[2].split(', ').join(' · ') : '';"
        render <- I(paste0(
            "{option: function(item, escape) {", split_label,
            " return '<div style=\"padding:7px 12px;\"><div style=\"font-weight:600;\">' + escape(name) + '</div>'",
            " + (meta ? '<div style=\"font-size:12px; opacity:0.7;\">' + escape(meta) + '</div>' : '') + '</div>'; },",
            " item: function(item, escape) {", split_label,
            " return '<div style=\"line-height:1.35;\"><div style=\"font-weight:600;\">' + escape(name) + '</div>'",
            " + (meta ? '<div style=\"font-size:12px; opacity:0.7;\">' + escape(meta) + '</div>' : '') + '</div>'; }}"))
        return(shiny::tagList(
            shiny::selectizeInput("nasqar_project_restore", "Saved project",
                                  choices = c("No saved projects yet" = ""), width = "100%",
                                  options = list(render = render)),
            shiny::div(
                style = "display:flex; gap:8px;",
                shiny::actionButton("nasqar_project_restore_btn", button_label, icon = shiny::icon("folder-open"),
                                    class = "btn-primary", style = "flex:1; color:#fff;"),
                manage_buttons,
                refresh_button
            )
        ))
    }
    shiny::tagList(
        if (isTRUE(help)) shiny::helpText(
            "Every analysis run on this instance is saved as a project in the private space.",
            "Pick one and press ", paste0(button_label, ":"), " its input, settings and results come back",
            "without recomputing (you can then change settings and rerun)."
        ),
        shiny::div(
            style = "display:flex; gap:8px; align-items:flex-end;",
            shiny::div(style = "flex:1;",
                shiny::selectInput("nasqar_project_restore", "Saved projects on this instance",
                                   choices = c("No saved projects yet" = ""))),
            shiny::div(style = "margin-bottom:15px; display:flex; gap:6px;",
                shiny::actionButton("nasqar_project_restore_btn", button_label, class = "btn-primary"),
                manage_buttons, refresh_button)
        ),
        shiny::tags$small(class = "text-muted", shiny::textOutput("nasqar_project_root_note", inline = TRUE))
    )
}

# The first question of a private instance: a new project or an existing one.
# Place it at the top of the input step. The project name the application
# saves under is the hidden "nasqar_project_name" input, kept by the server.
nasqar_project_step_ui <- function(default_name = "project") {
    small_button <- function(id, icon, title) {
        shiny::actionButton(id, NULL, icon = shiny::icon(icon), class = "btn-default btn-sm", title = title)
    }
    shiny::div(
        class = "nasqar-project-private", id = "nasqar_project_step",
        shiny::div(
            id = "nasqar_project_step_controls",
            shiny::radioButtons("nasqar_project_mode", "Project",
                                c("Create a new project" = "new", "Open an existing project" = "existing"),
                                selected = "new"),
            shiny::conditionalPanel(
                "input.nasqar_project_mode == 'new'",
                shiny::textInput("nasqar_project_new_name", NULL, value = default_name, width = "100%",
                                 placeholder = "Name of the new project")),
            shiny::conditionalPanel(
                "input.nasqar_project_mode == 'existing'",
                shiny::selectizeInput("nasqar_project_open", NULL, choices = c("No projects yet" = ""), width = "100%",
                                      options = list(render = nasqar_project_picker_render())),
                shiny::div(style = "display:flex; gap:6px; margin:-6px 0 8px;",
                           small_button("nasqar_project_rename_btn", "pen", "Rename this application's analysis in the project"),
                           small_button("nasqar_project_delete_btn", "trash", "Delete this application's analysis in the project"),
                           small_button("nasqar_project_refresh", "sync", "Refresh the list")))
        ),
        shiny::uiOutput("nasqar_project_status"),
        shiny::div(style = "display:none;", shiny::textInput("nasqar_project_name", NULL, value = default_name)),
        shiny::tags$script(shiny::HTML(
            "Shiny.addCustomMessageHandler('nasqar-project-lock', function(lock) {",
            "  $('#nasqar_project_step_controls').toggle(!lock); });")),
        shiny::tags$hr(style = "margin:12px 0;")
    )
}

# Two-line options for a project picker: the name, then "apps · date".
nasqar_project_picker_render <- function() {
    split_label <- "var m = /^(.*?)\\s{2}\\((.*)\\)$/.exec(item.label || ''), name = m ? m[1] : (item.label || ''), meta = m ? m[2].split(', ').join(' · ') : '';"
    I(paste0(
        "{option: function(item, escape) {", split_label,
        " return '<div style=\"padding:7px 12px;\"><div style=\"font-weight:600;\">' + escape(name) + '</div>'",
        " + (meta ? '<div style=\"font-size:12px; opacity:0.7;\">' + escape(meta) + '</div>' : '') + '</div>'; },",
        " item: function(item, escape) {", split_label,
        " return '<div style=\"line-height:1.35;\"><div style=\"font-weight:600;\">' + escape(name) + '</div>'",
        " + (meta ? '<div style=\"font-size:12px; opacity:0.7;\">' + escape(meta) + '</div>' : '') + '</div>'; }}"))
}

nasqar_project_name_ui <- function(default_name, label = "Persistent project name",
                                   help = "Outputs are kept under this name in the private space, so they survive the session and can be restored later.") {
    shiny::tagList(
        shiny::textInput("nasqar_project_name", label, value = default_name),
        shiny::helpText(help)
    )
}

nasqar_project_ui <- function(default_name, label = "Persistent project name",
                              help = "Outputs are kept under this name in the private space, so they survive the session and can be restored later.",
                              name_input = TRUE) {
    shiny::tagList(
        shiny::div(
            id = "nasqar_project_panel",
            shiny::tags$hr(),
            shiny::tags$strong("Reload a previous analysis"),
            nasqar_project_restore_ui(),
            if (isTRUE(name_input)) shiny::tags$hr(),
            if (isTRUE(name_input)) shiny::tags$strong("Start or continue a project"),
            if (isTRUE(name_input)) nasqar_project_name_ui(default_name, label, help),
            nasqar_project_outputs_ui()
        )
    )
}

# Wire the picker. on_restore(state, dir) is called with the saved state; it
# returns NULL on success or an error message. Returns a function that
# refreshes the picker, to call after the application writes a new state.
# input_default: list(text = id of the private-space path box, source = id of
# the "Where is your data?" radio, value = its private-space choice,
# find = function(apps) path) — when an existing project is opened, the path
# box is filled with find()'s file from the project's other applications
# (apps: folder name -> directory), e.g. DESeq2 takes Gene Count Merger's table.
nasqar_project_server <- function(input, output, session, app,
                                  on_restore, input_default = NULL) {
    session$userData$nasqar_project_input <- input   # for state.json snapshots
    if (!nasqar_project_available(app)) {
        shiny::removeUI(selector = "#nasqar_project_panel")
        shiny::removeUI(selector = ".nasqar-project-private", multiple = TRUE)
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
    this_label <- nasqar_project_app_label(app)
    studies <- shiny::reactiveVal(list())
    refresh <- function(select = NULL) {
        # callers name the project by its folder: accept the folder of either layout
        if (identical(select, app) && !is.null(session$userData$nasqar_project_last_dir)) {
            select <- nasqar_project_name(session$userData$nasqar_project_last_dir)
        }
        all <- nasqar_project_studies(app)
        studies(all)
        if (!length(all)) {
            shiny::updateSelectizeInput(session, "nasqar_project_open", choices = c("No projects yet" = ""))
            return(invisible())
        }
        choices <- stats::setNames(names(all), vapply(all, function(s) sprintf(
            "%s  (%s, %s)", s$name, paste(nasqar_project_app_label(names(s$apps)), collapse = " + "),
            format(s$updated, "%Y-%m-%d %H:%M")), character(1)))
        current_choice <- shiny::isolate(input$nasqar_project_open)
        selected <- if (!is.null(select) && select %in% choices) select
                    else if (!is.null(current_choice) && current_choice %in% choices) current_choice
                    else choices[[1]]
        shiny::updateSelectizeInput(session, "nasqar_project_open", choices = choices, selected = selected)
    }
    output$nasqar_project_root_note <- shiny::renderText(paste(
        "Projects are stored under", nasqar_project_root()))
    # A handoff from another application names the project to continue in.
    handoff_project_set <- FALSE
    shiny::observe({
        if (handoff_project_set || is.null(input$nasqar_project_mode)) return()   # until the step exists
        handoff_project_set <<- TRUE
        project <- shiny::parseQueryString(shiny::isolate(session$clientData$url_search))$project
        if (!is.null(project) && nzchar(project)) {
            project <- nasqar_project_sanitize(project, "project")
            if (project %in% names(shiny::isolate(studies()))) {
                shiny::updateRadioButtons(session, "nasqar_project_mode", selected = "existing")
                shiny::updateSelectizeInput(session, "nasqar_project_open", selected = project)
            } else {
                shiny::updateTextInput(session, "nasqar_project_new_name", value = project)
            }
        }
    })

    # ---- the project step: new or existing, and the name actually used --------
    wanted <- shiny::reactive({
        mode <- input$nasqar_project_mode %||% "new"
        name <- if (identical(mode, "existing")) input$nasqar_project_open else input$nasqar_project_new_name
        name <- nasqar_project_sanitize(name %||% "", "")
        if (nzchar(name)) list(mode = mode, name = name) else NULL
    })
    # One analysis of an application per project: when the project already
    # holds one (not this session's), new data goes to a new project, name-2.
    effective_name <- shiny::reactive({
        w <- wanted()
        if (is.null(w)) return(NULL)
        current()
        nasqar_project_unique_name(app, w$name, owned = session$userData$nasqar_project_last_dir)
    })
    shiny::observe({
        name <- effective_name()
        if (is.null(name) || !is.null(current())) return()   # locked once data is in a project
        if (!identical(name, shiny::isolate(input$nasqar_project_name))) {
            shiny::updateTextInput(session, "nasqar_project_name", value = name)
        }
    })
    selected_study <- shiny::reactive({
        w <- wanted()
        if (is.null(w)) NULL else studies()[[w$name]]
    })
    # Browse dialogs open in the chosen project (or, once data is loaded, in
    # the project being saved to).
    shiny::observe({
        dir <- current()
        session$userData$nasqar_project_browse_dir <- if (!is.null(dir)) {
            if (basename(dir) %in% nasqar_project_apps || is.null(nasqar_project_root())) dir else dirname(dir)
        } else {
            w <- wanted()
            if (!is.null(w) && identical(w$mode, "existing")) nasqar_project_study_dir(selected_study(), app) else NULL
        }
    })
    suggested <- shiny::reactiveVal(NULL)
    if (!is.null(input_default)) {
        last_fill <- ""
        shiny::observe({
            w <- wanted()
            study <- selected_study()
            if (is.null(w) || !identical(w$mode, "existing") || is.null(study) || !is.null(current())) {
                suggested(NULL)
                return()
            }
            # the project already has this analysis: nothing loads until the
            # user continues it or loads new data on purpose
            if (app %in% names(study$apps)) {
                suggested(NULL)
                return()
            }
            path <- tryCatch(input_default$find(study$apps), error = function(e) NULL)
            path <- if (length(path) && !is.na(path[1]) && file.exists(path[1])) path[1] else NULL
            suggested(path)
            if (is.null(path)) return()
            typed <- shiny::isolate(input[[input_default$text]]) %||% ""
            if (nzchar(typed) && !identical(typed, last_fill)) return()   # the user chose something else
            last_fill <<- path
            shiny::updateTextInput(session, input_default$text, value = path)
            if (!is.null(input_default$source)) {
                shiny::updateRadioButtons(session, input_default$source, selected = input_default$value)
            }
        })
    }
    shiny::observe({
        session$sendCustomMessage("nasqar-project-lock", !is.null(current()))
    })
    output$nasqar_project_status <- shiny::renderUI({
        dir <- current()
        if (!is.null(dir)) {
            return(shiny::div(
                style = "font-size:13px; margin-bottom:4px;",
                shiny::tags$label("Project"),
                shiny::div(shiny::icon("folder-open"), " Saving to ", shiny::tags$strong(nasqar_project_name(dir))),
                shiny::helpText("To give it another name, use Rename in the bottom-right corner. To work on another project, reload the page.")))
        }
        w <- wanted()
        if (is.null(w)) {
            return(shiny::helpText(if (identical(input$nasqar_project_mode, "existing"))
                "There are no projects yet: create a new one." else "Enter a name for the new project."))
        }
        name <- effective_name()
        study <- selected_study()
        has_this <- !is.null(study) && app %in% names(study$apps)
        if (identical(w$mode, "existing") && !is.null(study)) {
            contents <- shiny::tags$ul(style = "margin-bottom:6px; padding-left:18px;", lapply(names(study$apps), function(a) {
                shiny::tags$li(nasqar_project_app_label(a),
                               shiny::span(class = "text-muted", sprintf(" (%s)", study$stages[[a]])))
            }))
            intro <- shiny::div(style = "font-size:13px;", "This project holds:", contents)
            if (has_this) {
                return(shiny::tagList(intro, shiny::div(
                    style = "border-left:3px solid #3c8dbc; background:#f4f8fb; padding:10px 12px; font-size:13px;",
                    shiny::p(style = "margin-bottom:8px;", shiny::tags$strong(this_label), " has already been run in this project."),
                    shiny::actionButton("nasqar_project_continue_btn", paste("Continue the", this_label, "analysis"),
                                        icon = shiny::icon("folder-open"), class = "btn-primary",
                                        style = "color:#fff; width:100%; white-space:normal;"),
                    shiny::p(style = "margin:8px 0 0;", "Or load new data below: it is saved as a new project, ",
                             shiny::tags$strong(name), "."))))
            }
            path <- suggested()
            return(shiny::tagList(intro, shiny::helpText(
                this_label, " is saved in this project.",
                if (!is.null(path)) shiny::tagList(" Its input is filled in from the project: ",
                                                   shiny::tags$code(basename(dirname(path)), "/", basename(path)),
                                                   ". You can pick any other file in the private space.")
                else " Browse opens in the project folder; you can pick any file in the private space.")))
        }
        if (has_this) {
            return(shiny::div(class = "text-warning", style = "font-size:13px;",
                "Project ", shiny::tags$strong(w$name), " already has a ", this_label, " analysis, so this one is saved as ",
                shiny::tags$strong(name), ". To continue that analysis, choose Open an existing project."))
        }
        if (!is.null(study)) {
            return(shiny::helpText("Project ", shiny::tags$strong(w$name), " already exists: ", this_label, " is added to it."))
        }
        shiny::helpText("Saved in the private space as ", shiny::tags$code(file.path("projects", name, app)), ".")
    })

    # ---- current project badge, rename and delete -------------------------
    nasqar_project_alias_cleanup(app)
    current <- shiny::reactiveVal(session$userData$nasqar_project_last_dir)
    session$userData$nasqar_project_current <- current
    is_current <- function(dir) {
        open <- session$userData$nasqar_project_last_dir
        !is.null(open) && identical(normalizePath(open, winslash = "/", mustWork = FALSE),
                                    normalizePath(dir, winslash = "/", mustWork = FALSE))
    }
    shiny::insertUI("body", "beforeEnd", immediate = TRUE, ui = shiny::uiOutput("nasqar_project_badge"))
    output$nasqar_project_badge <- shiny::renderUI({
        dir <- current()
        if (is.null(dir) || !dir.exists(dir)) return(NULL)
        shiny::div(
            style = paste("position:fixed; right:14px; bottom:12px; z-index:1040; background:#fff; border:1px solid #c9d3dc;",
                          "border-radius:16px; padding:5px 6px 5px 12px; font-size:13px; box-shadow:0 1px 4px rgba(0,0,0,.15);",
                          "display:flex; align-items:center; gap:8px; max-width:60vw;"),
            shiny::span(shiny::icon("folder-open"), " Saving to project ",
                        shiny::tags$strong(style = "word-break:break-all;", nasqar_project_name(dir))),
            shiny::actionButton("nasqar_project_rename_current", "Rename", icon = shiny::icon("pen"),
                                class = "btn-default btn-xs", style = "border-radius:12px;"))
    })
    ask_new_name <- function(name) {
        session$userData$nasqar_project_rename_target <- name
        shiny::showModal(shiny::modalDialog(
            title = "Rename project", easyClose = TRUE,
            shiny::p("This analysis moves from project ", shiny::tags$strong(name), " to the name you give, with everything it holds. Nothing is run again."),
            shiny::helpText("Give the name of a project another application already saved to, and the analyses sit together in one folder."),
            shiny::textInput("nasqar_project_rename_to", "New name", value = name, width = "100%"),
            shiny::helpText("Letters, digits, '.', '_' and '-'. Other characters become '-'."),
            footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton("nasqar_project_rename_ok", "Rename", class = "btn-primary", style = "color:#fff;"))))
    }
    shiny::observeEvent(input$nasqar_project_rename_current, {
        dir <- session$userData$nasqar_project_last_dir
        if (!is.null(dir)) ask_new_name(nasqar_project_name(normalizePath(dir, winslash = "/", mustWork = FALSE)))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$nasqar_project_rename_btn, {
        name <- input$nasqar_project_open
        if (is.null(name) || !nzchar(name)) return(shiny::showNotification("There is no saved project to rename.", type = "warning"))
        if (!app %in% names(studies()[[name]]$apps)) {
            return(shiny::showNotification(sprintf("Project '%s' has no %s analysis to rename.", name, this_label), type = "warning"))
        }
        ask_new_name(name)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$nasqar_project_rename_ok, {
        name <- session$userData$nasqar_project_rename_target
        if (is.null(name)) return()
        old_dir <- tryCatch(nasqar_project_checked_dir(app, name), error = function(error) NULL)
        was_current <- !is.null(old_dir) && is_current(old_dir)
        new_dir <- tryCatch(nasqar_project_rename(app, name, input$nasqar_project_rename_to),
                            error = function(error) {
                                shiny::showNotification(conditionMessage(error), type = "error", duration = 10)
                                NULL
                            })
        if (is.null(new_dir)) return()
        shiny::removeModal()
        if (was_current) {
            # every path this session knows the project by now leads to the new folder
            aliases <- unique(c(session$userData$nasqar_project_aliases, old_dir))
            linked <- vapply(aliases, nasqar_project_alias_create, logical(1), target_dir = new_dir)
            session$userData$nasqar_project_aliases <- aliases
            nasqar_project_set_current(session, new_dir)
            if (!is.null(input$nasqar_project_name)) {
                shiny::updateTextInput(session, "nasqar_project_name", value = nasqar_project_name(new_dir))
            }
            if (!all(linked)) shiny::showNotification(
                "The project is renamed. Reload the page and reopen it before you continue: this storage does not let the open session follow the new name.",
                type = "warning", duration = NULL)
        }
        refresh(nasqar_project_name(new_dir))
        shiny::showNotification(sprintf("Project '%s' is now '%s'.", name, nasqar_project_name(new_dir)), type = "message", duration = 8)
    }, ignoreInit = TRUE)
    session$onSessionEnded(function() {
        for (alias in session$userData$nasqar_project_aliases) nasqar_project_alias_remove(alias)
    })
    shiny::observeEvent(input$nasqar_project_delete_btn, {
        name <- input$nasqar_project_open
        if (is.null(name) || !nzchar(name)) return(shiny::showNotification("There is no saved project to delete.", type = "warning"))
        if (!app %in% names(studies()[[name]]$apps)) {
            return(shiny::showNotification(sprintf("Project '%s' has no %s analysis to delete.", name, this_label), type = "warning"))
        }
        dir <- tryCatch(nasqar_project_checked_dir(app, name), error = function(error) NULL)
        if (is.null(dir)) return(shiny::showNotification("This project no longer exists.", type = "warning"))
        if (is_current(dir)) {
            return(shiny::showNotification(
                "This project is open in this session. Reload the page first, then delete it.", type = "warning", duration = 10))
        }
        session$userData$nasqar_project_delete_target <- name
        shiny::showModal(shiny::modalDialog(
            title = "Delete project", easyClose = TRUE,
            shiny::p("Delete this application's analysis in project ", shiny::tags$strong(name), " (", nasqar_project_size(dir), ")?"),
            shiny::p(shiny::tags$strong("This removes its saved analysis, figures and tables for good. It cannot be undone.")),
            shiny::helpText("What other applications saved in the same project, and your input data, are not touched."),
            footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton("nasqar_project_delete_ok", "Delete project", icon = shiny::icon("trash"),
                                    class = "btn-danger", style = "color:#fff;"))))
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$nasqar_project_delete_ok, {
        name <- session$userData$nasqar_project_delete_target
        session$userData$nasqar_project_delete_target <- NULL
        if (is.null(name)) return()
        problem <- tryCatch({ nasqar_project_delete(app, name); NULL },
                            error = function(error) conditionMessage(error))
        shiny::removeModal()
        refresh()
        shiny::showNotification(if (is.null(problem)) sprintf("Project '%s' deleted.", name) else problem,
                                type = if (is.null(problem)) "message" else "error", duration = 8)
    }, ignoreInit = TRUE)
    nasqar_project_outputs_server(input, output, session)
    shiny::observeEvent(input$nasqar_project_refresh, refresh(), ignoreInit = TRUE)
    refresh()
    restore <- function(name) {
        if (is.null(name) || !nzchar(name)) return()
        dir <- nasqar_project_dir(app, name, create = FALSE)
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
            nasqar_project_set_current(session, dir)
            nasqar_project_json_write(dir, state)
            if (!is.null(input$nasqar_project_name)) {
                shiny::updateTextInput(session, "nasqar_project_name", value = name)
            }
            shiny::showNotification(sprintf("Project '%s' restored (%s).", name, state$stage %||% "saved"),
                                    type = "message", duration = 8)
        }
    }
    shiny::observeEvent(input$nasqar_project_restore_btn, restore(input$nasqar_project_restore), ignoreInit = TRUE)
    shiny::observeEvent(input$nasqar_project_continue_btn, restore(input$nasqar_project_open), ignoreInit = TRUE)
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
