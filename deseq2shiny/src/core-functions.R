# Pure helpers shared by the Shiny application and automated tests.

encryptUrlParam <- function(paramStr) {
  public_key_hex <- Sys.getenv(
    "NASQAR_PUBLIC_KEY_HEX",
    unset = "42b3781d6907cd426b9c05cac7155cce15bb9385a602716f619529485dab6c28"
  )
  public_key <- sodium::hex2bin(public_key_hex)
  message <- serialize(paramStr, NULL)
  sodium::bin2hex(sodium::simple_encrypt(message, public_key))
}

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

align_metadata_to_counts <- function(metadata, count_sample_names) {
  if (!is.data.frame(metadata)) {
    metadata <- as.data.frame(metadata)
  }
  metadata_names <- rownames(metadata)
  if (is.null(metadata_names) || any(!nzchar(metadata_names))) {
    stop("Metadata must have a non-empty row name for every sample.",
         call. = FALSE)
  }
  if (anyDuplicated(metadata_names)) {
    stop("Metadata sample names must be unique.", call. = FALSE)
  }
  if (anyDuplicated(count_sample_names)) {
    stop("Count-matrix sample names must be unique.", call. = FALSE)
  }

  missing_metadata <- setdiff(count_sample_names, metadata_names)
  extra_metadata <- setdiff(metadata_names, count_sample_names)
  if (length(missing_metadata) > 0L || length(extra_metadata) > 0L) {
    details <- c(
      if (length(missing_metadata) > 0L) {
        paste("Missing metadata:", paste(missing_metadata, collapse = ", "))
      },
      if (length(extra_metadata) > 0L) {
        paste("Not present in counts:", paste(extra_metadata, collapse = ", "))
      }
    )
    stop(
      paste(
        "Metadata samples must exactly match count-matrix columns.",
        paste(details, collapse = " ")
      ),
      call. = FALSE
    )
  }

  metadata[count_sample_names, , drop = FALSE]
}

has_gene_alias_column <- function(count_data) {
  count_frame <- as.data.frame(count_data, check.names = FALSE)
  if (ncol(count_frame) < 3L) {
    return(FALSE)
  }

  normalized_name <- tolower(gsub("[^a-z0-9]", "", names(count_frame)[[2L]]))
  normalized_name %in% c("genename", "genenames", "genesymbol", "symbol")
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

nasqar_exchange_dir <- function(create = FALSE) {
  configured <- Sys.getenv("NASQAR_EXCHANGE_DIR", unset = "")
  path <- if (nzchar(configured)) {
    configured
  } else {
    file.path(dirname(tempdir(check = TRUE)), "nasqar_exchange")
  }
  if (create) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  }
  normalizePath(path, mustWork = create)
}

new_exchange_file <- function(extension = ".csv") {
  file.path(
    nasqar_exchange_dir(create = TRUE),
    paste0(uuid::UUIDgenerate(), extension)
  )
}

validate_exchange_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("Invalid NASQAR exchange path.", call. = FALSE)
  }

  resolved <- normalizePath(path, mustWork = TRUE)
  allowed_root <- nasqar_exchange_dir(create = TRUE)
  prefix <- paste0(allowed_root, .Platform$file.sep)
  uuid_csv <- paste0(
    "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-",
    "[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\\.csv$"
  )
  if (!startsWith(resolved, prefix) ||
      !grepl(uuid_csv, basename(resolved))) {
    stop("Exchange path does not match a NASQAR2 exchange CSV.",
         call. = FALSE)
  }

  info <- file.info(resolved)
  if (is.na(info$isdir) || info$isdir || info$size > 600 * 1024^2) {
    stop("Exchange file is invalid or too large.", call. = FALSE)
  }
  resolved
}

publication_downloads <- function(id) {
    shiny::tags$div(
        class = "publication-downloads",
        shiny::tags$span(class = "publication-downloads__label", "Download figure"),
        shiny::downloadButton(
            paste0(id, "_png"), "PNG (300 DPI)",
            class = "btn btn-default btn-sm"
        ),
        shiny::downloadButton(
            paste0(id, "_pdf"), "PDF",
            class = "btn btn-default btn-sm"
        ),
        shiny::downloadButton(
            paste0(id, "_svg"), "SVG",
            class = "btn btn-default btn-sm"
        )
    )
}

draw_publication_plot <- function(plot_function) {
    plot_object <- plot_function()
    if (inherits(plot_object, c("gg", "ggplot", "grob", "gTree", "gtable"))) {
        print(plot_object)
    }
    invisible(plot_object)
}

register_publication_downloads <- function(
    output,
    id,
    filename,
    plot_function,
    width = 8,
    height = 6
) {
    formats <- list(
        png = list(
            extension = "png",
            content_type = "image/png",
            open = function(file) {
                grDevices::png(
                    file,
                    width = width,
                    height = height,
                    units = "in",
                    res = 300,
                    type = if (capabilities("cairo")) "cairo" else getOption("bitmapType")
                )
            }
        ),
        pdf = list(
            extension = "pdf",
            content_type = "application/pdf",
            open = function(file) {
                grDevices::pdf(file, width = width, height = height, useDingbats = FALSE)
            }
        ),
        svg = list(
            extension = "svg",
            content_type = "image/svg+xml",
            open = function(file) grDevices::svg(file, width = width, height = height)
        )
    )
    for (format in names(formats)) {
        config <- formats[[format]]
        local({
            local_format <- format
            local_config <- config
            output[[paste0(id, "_", local_format)]] <- shiny::downloadHandler(
                filename = function() paste0(filename(), ".", local_config$extension),
                contentType = local_config$content_type,
                content = function(file) {
                    local_config$open(file)
                    on.exit(grDevices::dev.off(), add = TRUE)
                    draw_publication_plot(plot_function)
                }
            )
        })
    }
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
