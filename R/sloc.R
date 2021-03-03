#' @importFrom fs dir_create path_dir
#' @importFrom fst write.fst
#' @export
compute_sloc <- function(path,
                         engine = sloc_cloc_engine(),
                         split = TRUE,
                         output_filepath = NULL,
                         ...) {
    sloc <- engine(path, ...)

    if(split) {
        sloc <- split_sloc(sloc, path)
    }

    if(!is.null(output_filepath)) {
        dir_create(path_dir(output_filepath), recurse=TRUE)
        write.fst(sloc, output_filepath)
    }

    sloc
}


#' @importFrom readr read_delim
#' @importFrom processx run
#' @importFrom stringr str_replace
#' @importFrom utils str
#' @importFrom dplyr rename
#' @importFrom utils head
#' @export
sloc_cloc_engine <- function(binary = "cloc", arguments = c("--follow-links", "-q", "--csv", "--csv-delimiter=;", "--by-file")) {
    result <- function(path, ...) {
        result <- run(binary,
                      c(arguments, path),
                      ...)

        print(str(result))

        if(result$status == 0) {
            result$stdout <- str_replace(result$stdout, ',"github.com/AlDanial/cloc.*', "")
        }

        if (length(result$stdout) > 0) {
            sloc <- read_delim(result$stdout, delim=';' ,col_types="cciii")
            sloc <- head(sloc, -1)
            rename(sloc, filepath = filename)
        } else {
            NULL
        }
    }
}


#' @importFrom fs path_split
#' @importFrom purrr map_chr
#' @importFrom stringr str_c str_sub str_length
#' @importFrom utils str tail
#' @importFrom dplyr bind_cols
#' @importFrom rlang seq2
#' @importFrom tibble tibble
#' @export
split_sloc <- function(sloc, path) {
    split_paths <- path_split(sloc$filepath)
    ignore <- length(path_split(path)[[1]])
    packages <- map_chr(split_paths, function(x) x[ignore + 1])
    types <- map_chr(split_paths, function(x) x[ignore + 2])
    filenames <- map_chr(split_paths, function(x) tail(x, 1))
    subdirs <- map_chr(split_paths, function(x) {
        elts <- x[seq2(ignore + 3, length(x) - 1)]
        str_c(elts, collapse="/", sep="")
    })

    bind_cols(
        tibble(package = packages,
               type = types,
               subdir = subdirs,
               filename = filenames),
        sloc)
}
