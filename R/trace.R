
#' @export
#' @importFrom fst write_fst
#' @importFrom fs dir_create path_join path_ext_set
write_tracing_result <- function(result, dir) {
    dir_create(dir, recurse = TRUE)
    data <- result$data
    for(name in names(data)) {
        file <- path_ext_set(path_join(dir, name), ext = "fst")
        write_fst(data[[name]], file)
    }
}

