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

register_publication_png <- function(output, id, filename, source_file) {
    output[[paste0(id, "_png")]] <- shiny::downloadHandler(
        filename = function() paste0(filename(), ".png"),
        contentType = "image/png",
        content = function(file) {
            if (!file.copy(source_file(), file, overwrite = TRUE)) {
                stop("Could not prepare the figure download.", call. = FALSE)
            }
        }
    )
}
