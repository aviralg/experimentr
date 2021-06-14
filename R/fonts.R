
#' @importFrom sysfonts font_add
#' @importFrom showtext showtext_auto
#' @importFrom fs path
initialize_font <- function (family,
                             regular,
                             bold = NULL,
                             italic = NULL,
                             bolditalic = NULL,
                             symbol = NULL,
                             dir = system.file("fonts", family,
                                               package = "experimentr",
                                               mustWork = TRUE)) {

    if (!is.null(regular)) {
        regular <- path(dir, regular);
    }

    if (!is.null(bold)) {
        bold <- path(dir, bold);
    }

    if (!is.null(italic)) {
        italic <- path(dir, italic);
    }

    if (!is.null(bolditalic)) {
        bolditalic <- path(dir, bolditalic);
    }

    if (!is.null(symbol)) {
        symbol <- path(dir, symbol);
    }

    font_add(family,
             regular,
             bold,
             italic,
             bolditalic,
             symbol)

    showtext_auto()
}

initialize_linux_libertine_font <- function() {
    initialize_font("Linux Libertine",
                    "LinLibertine_R.otf",
                    "LinLibertine_RB.otf",
                    "LinLibertine_RI.otf",
                    "LinLibertine_RBI.otf")
}
