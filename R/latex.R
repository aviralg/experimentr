
#' @export
LatexMacroGenerator <-
    R6Class("LatexMacroGenerator",
            list(
                #' @importFrom fs dir_create
                 initialize = function(dir = "./", ext = "tex") {
                     stopifnot(is.character(dir), length(dir) == 1)
                     stopifnot(is.character(ext), length(ext) == 1)

                     private$dir <- dir
                     private$ext <- ext

                     if (!dir_exists(dir)) {
                         cat("Creating directory ", dir, "\n")
                         dir_create(dir)
                     }
                 },

                 get_dir = function() {
                     private$dir
                 },

                 get_ext = function() {
                     private$ext
                 },

                 from_table = function(table, label_colname = "label", value_colname = "value", prefix = "") {
                     self$from_vectors(table[[label_colname]], table[[value_colname]], prefix = prefix)
                 },

                 from_list = function(list, prefix = "") {
                     self$from_vectors(names(list), unlist(unname(list)), prefix)
                 },

                 from_args = function(..., prefix = "") {
                     self$from_list(list(...), prefix = prefix)
                 },

                 from_vectors = function(names, values, prefix = "") {
                     private$process(names, values, prefix = prefix)
                 }
            ),
            private = list(
                dir = NA_character_,
                ext = NA_character_,

                #' @importFrom purrr map2
                process = function(names, values, prefix) {
                    names <- paste0(prefix, names)
                    map2_chr(names, values, private$writer)
                },

                #' @importFrom fs path
                #' @importFrom glue glue
                #' @importFrom readr write_file
                writer = function(name, value) {

                    filename <- path(self$get_dir(), name, ext = self$get_ext())

                    macro <- glue("\\[name]", .open = "[", .close = "]")

                    content <- glue("\\newcommand{[macro]}{[value]\\xspace}",
                                    .open = "[",
                                    .close = "]")

                    write_file(content, filename)

                    macro
                }
            )
            )

#' @export
#' @importFrom fs dir_ls
#' @importFrom purrr map_chr
#' @importFrom readr read_file write_lines
merge_macros <- function(srcdir, outfile = NULL, ext = "tex") {
    files <- path_abs(dir_ls(srcdir, glob = paste0("*.", ext), recurse = TRUE, type = "file"))

    if(!is.null(outfile)) {
        files <- setdiff(files, outfile)
    }

    contents <- map_chr(files, read_file)

    if(!is.null(outfile)) {
        write_lines(contents, outfile)
    }

    contents
}


#' @export
#' @importFrom purrr map_chr
#' @importFrom readr write_file
#' @importFrom magrittr `%>%`
latex_table <- function(table, formatter, outfile = NULL) {
    latex <-
        table %>%
        pmap_chr(formatter) %>%
        paste0("\\\\") %>%
        paste(collapse = "\n")


    if(!is.null(outfile)) {
        write_file(latex, outfile)
    }

    latex
}

#' @export
#' @importFrom stringr str_replace_all fixed
#' @importFrom magrittr `%>%`
latex_sanitize <- function(input) {
    stopifnot(is.character(input))

    input %>%
        str_replace_all(fixed("#"), "\\#") %>%
        str_replace_all(fixed("%"), "\\%") %>%
        str_replace_all(fixed("$"), "\\$") %>%
        str_replace_all(fixed("_"), "\\_") %>%
        str_replace_all(fixed(","), "\\,") %>%
        str_replace_all(fixed("^"), "\\^") %>%
        str_replace_all(fixed("&"), "\\&") %>%
        str_replace_all(fixed("{"), "\\{") %>%
        str_replace_all(fixed("}"), "\\}") %>%
        str_replace_all(fixed("~"), "\\~")
}
