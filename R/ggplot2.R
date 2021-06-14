
#' @export
#' @importFrom tikzDevice tikz
#' @importFrom fs dir_create path_ext_set path
#' @importFrom grDevices dev.off
save_as_tikz <- function(plot,
                         filename,
                         width = 3.3,
                         height = width / GOLDEN_RATIO,
                         sanitize = TRUE,
                         ...,
                         dir = ".") {
    dir_create(dir)
    filepath <- path_ext_set(path(dir, filename), "tex")

    tikz(file = filepath, sanitize = sanitize, width = width, height = height, ...)
    print(plot)
    dev.off()

    plot
}


#' @export
set_publication_theme <- function(type = "acmart", base_size = 8, ...) {

    if (type == "acmart") {
        set_acmart_theme(base_size = base_size, ...)
    }
    else {
        stop(sprintf("Unsupported theme type %s", type))
    }
}


#' @importFrom ggplot2 theme_minimal theme theme_set
set_acmart_theme <- function(base_size = 8, ...) {

    initialize_linux_libertine_font()

    new_theme <-
        theme_minimal(base_size = base_size,
                      base_family = "Linux Libertine") +
        theme(...)

    theme_set(new_theme)
}
