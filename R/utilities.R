
#' @importFrom tools file_path_sans_ext
replace_extension <- function(paths, extension) {
    if (length(paths) == 0) {
        character(0)
    }
    else {
        paths <- file_path_sans_ext(paths)
        paste(paths, extension, sep = "")
    }
}


pad <- function(str, max, char = "", truncate = FALSE) {
    n <- nchar(str)

    if (n == max) str
    else if (n > max) {
        if (truncate) substr(str, 1, max)
        else str
    }
    else {
        spaces <- paste0(replicate(max - n, " "), collapse="")
        paste0(str, spaces, collapse = "", sep = "")
    }
}

#' @export
run_length_encoding <- function(input) {
    .Call(r_experimentr_run_length_encoding, input)
}

#' @export
compose <- function(...) {
    funs <- rev(list(...))

    function(...) {
        res <- funs[[1]](...)
        for (fun in funs[-1]) {
            res <- fun(res)
        }
        res
    }
}
