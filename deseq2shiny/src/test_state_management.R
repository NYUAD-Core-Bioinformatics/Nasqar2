# Executable regression tests for validation and portable state helpers.

source("core-functions.R")

expect_error <- function(expression) {
  error_raised <- FALSE
  tryCatch(
    force(expression),
    error = function(e) {
      error_raised <<- TRUE
    }
  )
  if (!error_raised) {
    stop("Expected an error, but the expression succeeded.", call. = FALSE)
  }
}

cat("Running DESeq2Shiny core regression tests\n")

metadata_names <- c("condition", "batch")
design <- parse_design_formula("~ condition * batch", metadata_names)
stopifnot(
  inherits(design, "formula"),
  identical(attr(terms(design), "term.labels"),
            c("condition", "batch", "condition:batch"))
)

expect_error(
  parse_design_formula(
    "~ I({ system('id'); condition })",
    metadata_names
  )
)
expect_error(parse_design_formula("~ missing_column", metadata_names))

valid_counts <- data.frame(
  sample_a = c("0", "4", "12"),
  sample_b = c(2, 8, 20),
  row.names = c("gene1", "gene2", "gene3"),
  check.names = FALSE
)
count_matrix <- validate_count_matrix(valid_counts)
stopifnot(
  is.matrix(count_matrix),
  typeof(count_matrix) == "integer",
  identical(dim(count_matrix), c(3L, 2L)),
  identical(rownames(count_matrix), rownames(valid_counts))
)
expect_error(validate_count_matrix(data.frame(sample = c(1, 2.5))))
expect_error(validate_count_matrix(data.frame(sample = c(1, -2))))
expect_error(validate_count_matrix(data.frame(sample = c(1, "invalid"))))

three_level_factor <- factor(c("A", "A", "B", "C", "C"))
stopifnot(is_categorical_factor(three_level_factor, 5L))
stopifnot(!is_categorical_factor(seq_len(5), 5L))

stopifnot(
  identical(safe_file_name("../../A vs B", ".csv"), "A_vs_B.csv")
)

exchange_filename <- "550e8400-e29b-41d4-a716-446655440000.csv"
exchange_path <- file.path(
  nasqar_exchange_dir(create = TRUE),
  exchange_filename
)
write.csv(data.frame(gene = "g1", count = 1L), exchange_path,
          row.names = FALSE)
stopifnot(identical(validate_exchange_path(exchange_path),
                    normalizePath(exchange_path)))
expect_error(validate_exchange_path("/etc/passwd"))
outside_exchange <- file.path(tempdir(), exchange_filename)
write.csv(data.frame(gene = "g1"), outside_exchange, row.names = FALSE)
expect_error(validate_exchange_path(outside_exchange))

source_result <- file.path(tempdir(), "source-result.csv")
write.csv(
  data.frame(
    gene = c("g1", "g2"),
    log2FoldChange = c(1.5, -2),
    padj = c(0.01, NA)
  ),
  source_result,
  row.names = FALSE
)
snapshot <- snapshot_saved_results(list(comparison = source_result))
validated_state <- validate_state_object(list(
  dataCounts = count_matrix,
  fileContent = data.frame(gene = rownames(count_matrix)),
  DF = data.frame(condition = factor(c("A", "B"))),
  saved_inputs = list(alpha = 0.1),
  filelist_file_list = snapshot,
  contrast_specs = list()
))
stopifnot(is.list(validated_state))
expect_error(validate_state_object(list(dataCounts = "not a matrix")))

state_file <- tempfile(fileext = ".RData")
state_object <- validated_state
save(state_object, file = state_file)
isolated_state_environment <- new.env(parent = emptyenv())
loaded_names <- load(state_file, envir = isolated_state_environment)
stopifnot(identical(loaded_names, "state_object"))
invisible(validate_state_object(get(
  "state_object",
  envir = isolated_state_environment,
  inherits = FALSE
)))

restored_dir <- tempfile("restored-state-")
restored_paths <- materialize_saved_results(snapshot, restored_dir)
restored_result <- read.csv(restored_paths$comparison)
stopifnot(
  identical(names(restored_paths), "comparison"),
  identical(restored_result$gene, c("g1", "g2")),
  is.na(restored_result$padj[[2L]])
)

cat("All DESeq2Shiny core regression tests passed\n")
