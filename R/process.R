
#' @export
#' @importFrom processx run
cmd_output <- function(...) {
    result <- run(...)
    result$stdout
}

cmd_output_int <- function(...) {
    compose(as.integer,
            str_trim,
            cmd_stdout)(...)
}

#' @export
#' @importFrom stringr str_trim
id_cmd <- function(...) {
    compose(as.integer,
            str_trim,
            command_output)(command = "id", ...)
}

#' @export
get_uid <- function() {
    id_cmd(args = "-u")
}

#' @export
get_gid <- function() {
    id_cmd(args = "-g")
}

#' @export
get_username <- function() {
    compose(str_trim, system2)("echo", "$USER", stdout = TRUE)
}
