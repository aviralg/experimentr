
#' @importFrom tools file_path_sans_ext
replace_extension <- function(paths, extension) {
    paths <- file_path_sans_ext(paths)
    paste(paths, extension, sep = "")
}
