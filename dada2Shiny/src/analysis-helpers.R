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

align_sample_metadata <- function(metadata, sample_names) {
    metadata <- as.data.frame(metadata, check.names = FALSE)
    if (is.null(rownames(metadata)) ||
        !setequal(sample_names, rownames(metadata))) {
        stop("Metadata sample names must match the DADA2 sample names.",
             call. = FALSE)
    }
    metadata[sample_names, , drop = FALSE]
}

top_taxa_names <- function(abundance, limit = 20L) {
    taxa_names <- names(abundance)
    abundance <- as.numeric(abundance)
    names(abundance) <- taxa_names
    if (length(abundance) == 0L) {
        return(character())
    }
    ordered <- names(sort(abundance, decreasing = TRUE))
    utils::head(ordered, min(as.integer(limit), length(ordered)))
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
