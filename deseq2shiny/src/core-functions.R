# Pure helpers shared by the Shiny application and automated tests.

parse_design_formula <- function(formula_text, metadata_names) {
  if (is.null(formula_text) || length(formula_text) != 1L ||
      !nzchar(trimws(formula_text))) {
    stop("Design formula is required.", call. = FALSE)
  }

  expression <- tryCatch(
    str2lang(formula_text),
    error = function(e) stop("Invalid design formula syntax.", call. = FALSE)
  )

  if (!is.call(expression) || !identical(as.character(expression[[1L]]), "~") ||
      length(expression) != 2L) {
    stop("Design formula must be a one-sided formula such as '~ condition'.",
         call. = FALSE)
  }

  allowed_operators <- c("+", "-", "*", ":", "/", "^", "(")

  validate_term <- function(term) {
    if (is.symbol(term)) {
      term_name <- as.character(term)
      if (!(term_name %in% metadata_names)) {
        stop(sprintf("Unknown metadata column in design formula: %s", term_name),
             call. = FALSE)
      }
      return(invisible(TRUE))
    }

    if (is.numeric(term)) {
      if (length(term) != 1L || !is.finite(term)) {
        stop("Invalid numeric value in design formula.", call. = FALSE)
      }
      return(invisible(TRUE))
    }

    if (is.call(term)) {
      operator <- as.character(term[[1L]])
      if (length(operator) != 1L || !(operator %in% allowed_operators)) {
        stop(sprintf("Function calls are not allowed in design formulas: %s",
                     paste(deparse(term), collapse = " ")),
             call. = FALSE)
      }
      lapply(as.list(term)[-1L], validate_term)
      return(invisible(TRUE))
    }

    stop("Unsupported value in design formula.", call. = FALSE)
  }

  validate_term(expression[[2L]])
  stats::as.formula(expression, env = baseenv())
}

validate_count_matrix <- function(count_data) {
  count_frame <- as.data.frame(count_data, check.names = FALSE)
  if (nrow(count_frame) == 0L || ncol(count_frame) == 0L) {
    stop("Count data must contain at least one gene and one sample.",
         call. = FALSE)
  }

  numeric_columns <- lapply(names(count_frame), function(column_name) {
    original <- count_frame[[column_name]]
    numeric_values <- suppressWarnings(as.numeric(as.character(original)))

    if (anyNA(numeric_values)) {
      stop(sprintf("Count column '%s' contains missing or non-numeric values.",
                   column_name),
           call. = FALSE)
    }
    if (any(!is.finite(numeric_values))) {
      stop(sprintf("Count column '%s' contains non-finite values.", column_name),
           call. = FALSE)
    }
    if (any(numeric_values < 0)) {
      stop(sprintf("Count column '%s' contains negative values.", column_name),
           call. = FALSE)
    }
    if (any(numeric_values != floor(numeric_values))) {
      stop(sprintf("Count column '%s' contains fractional values.", column_name),
           call. = FALSE)
    }
    if (any(numeric_values > .Machine$integer.max)) {
      stop(sprintf("Count column '%s' exceeds the supported integer range.",
                   column_name),
           call. = FALSE)
    }

    as.integer(numeric_values)
  })

  result <- do.call(cbind, numeric_columns)
  colnames(result) <- names(count_frame)
  rownames(result) <- rownames(count_frame)
  result
}

is_categorical_factor <- function(factor_data, sample_count) {
  clean_data <- factor_data[!is.na(factor_data)]
  if (length(clean_data) == 0L ||
      !(is.factor(factor_data) || is.character(factor_data))) {
    return(FALSE)
  }

  level_count <- length(unique(clean_data))
  level_count >= 2L && level_count < sample_count
}

safe_file_name <- function(value, extension = NULL) {
  value <- paste(as.character(value), collapse = "_")
  value <- basename(gsub("\\\\", "/", value))
  value <- gsub("[^A-Za-z0-9._-]+", "_", value)
  value <- gsub("^\\.+", "", value)
  if (!nzchar(value)) {
    value <- "result"
  }
  if (!is.null(extension) && !endsWith(tolower(value), tolower(extension))) {
    value <- paste0(value, extension)
  }
  value
}

validate_exchange_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("Invalid NASQAR exchange path.", call. = FALSE)
  }

  resolved <- normalizePath(path, mustWork = TRUE)
  configured_root <- Sys.getenv("NASQAR_EXCHANGE_DIR", unset = "")

  if (nzchar(configured_root)) {
    allowed_root <- normalizePath(configured_root, mustWork = TRUE)
    prefix <- paste0(allowed_root, .Platform$file.sep)
    if (!startsWith(resolved, prefix)) {
      stop("Exchange file is outside the configured NASQAR directory.",
           call. = FALSE)
    }
  } else {
    # Backward-compatible constraint for GeneCountMerger handoffs.
    temp_parent <- normalizePath(dirname(tempdir()), mustWork = TRUE)
    temp_prefix <- paste0(temp_parent, .Platform$file.sep)
    if (!startsWith(resolved, temp_prefix)) {
      stop("Exchange file is outside the temporary NASQAR directory.",
           call. = FALSE)
    }
    relative_path <- substring(resolved, nchar(temp_parent) + 2L)
    path_parts <- strsplit(relative_path, .Platform$file.sep, fixed = TRUE)[[1L]]
    uuid_csv <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\\.csv$"

    if (length(path_parts) != 2L ||
        !grepl("^Rtmp[A-Za-z0-9]+$", path_parts[[1L]]) ||
        !grepl(uuid_csv, path_parts[[2L]])) {
      stop("Exchange path does not match a NASQAR temporary CSV.",
           call. = FALSE)
    }
  }

  info <- file.info(resolved)
  if (is.na(info$isdir) || info$isdir || info$size > 600 * 1024^2) {
    stop("Exchange file is invalid or too large.", call. = FALSE)
  }
  resolved
}

snapshot_saved_results <- function(file_list) {
  if (is.null(file_list) || length(file_list) == 0L) {
    return(list())
  }

  lapply(file_list, function(path) {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  })
}

materialize_saved_results <- function(result_snapshot, destination_dir) {
  if (is.null(result_snapshot) || length(result_snapshot) == 0L) {
    return(list())
  }

  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  result_paths <- list()
  for (result_name in names(result_snapshot)) {
    filename <- safe_file_name(result_name, ".csv")
    path <- file.path(destination_dir, filename)
    result_data <- result_snapshot[[result_name]]
    if (is.character(result_data) && length(result_data) == 1L &&
        file.exists(result_data)) {
      result_data <- utils::read.csv(
        result_data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    if (!is.data.frame(result_data)) {
      stop(sprintf("Saved result '%s' is invalid.", result_name),
           call. = FALSE)
    }
    utils::write.csv(result_data, path, row.names = FALSE)
    result_paths[[result_name]] <- path
  }
  result_paths
}

validate_state_object <- function(state_object) {
  if (!is.list(state_object)) {
    stop("State object must be a list.", call. = FALSE)
  }
  if (utils::object.size(state_object) > 2 * 1024^3) {
    stop("Expanded state object exceeds the 2 GB safety limit.",
         call. = FALSE)
  }

  if (!is.null(state_object$dataCounts) &&
      !(is.matrix(state_object$dataCounts) &&
        is.numeric(state_object$dataCounts))) {
    stop("State count data is invalid.", call. = FALSE)
  }
  if (!is.null(state_object$fileContent) &&
      !is.data.frame(state_object$fileContent)) {
    stop("State file content is invalid.", call. = FALSE)
  }
  if (!is.null(state_object$DF) && !is.data.frame(state_object$DF)) {
    stop("State metadata is invalid.", call. = FALSE)
  }
  if (!is.null(state_object$saved_inputs) &&
      !is.list(state_object$saved_inputs)) {
    stop("State input settings are invalid.", call. = FALSE)
  }
  if (!is.null(state_object$filelist_file_list)) {
    if (!is.list(state_object$filelist_file_list) ||
        !all(vapply(
          state_object$filelist_file_list,
          function(value) {
            is.data.frame(value) ||
              (is.character(value) && length(value) == 1L)
          },
          logical(1)
        ))) {
      stop("State saved-result data is invalid.", call. = FALSE)
    }
  }
  if (!is.null(state_object$contrast_specs) &&
      !is.list(state_object$contrast_specs)) {
    stop("State contrast specifications are invalid.", call. = FALSE)
  }

  state_object
}
