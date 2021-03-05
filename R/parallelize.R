
#' @export
#' @importFrom fs path_join
current_r_exec <- function() {
    path_join(list(c(R.home("bin"), "R")))
}

#' @export
#' @importFrom stringr str_c
r_expr <- function(expr,
                   r_exec = current_r_exec(),
                   args = c("--vanilla", "--slave"),
                   invisible = FALSE) {
    if(invisible) {
        expr <- str_c("invisible(", expr, ")", sep = "");
    }
    expr <- str_c("\"", expr, "\"")

    c(r_exec, args, "-e", expr)
}

#' @export
r_file <- function(file,
                   r_exec = current_r_exec(),
                   args = c("--vanilla", "--slave")) {
    c(r_exec, args, "-f", file)
}

#' @export
#' @importFrom processx run
parallelize <- function(command,
                        ...,
                        error_on_status = TRUE,
                        wd = NULL,
                        echo_cmd = TRUE,
                        echo = FALSE,
                        spinner = FALSE,
                        timeout = Inf,
                        stderr_to_stdout = TRUE,
                        env = NULL,
                        windows_verbatim_args = FALSE,
                        windows_hide_window = FALSE,
                        encoding = "",
                        cleanup_tree = TRUE,
                        engine = gnu_parallel()) {
    inputs <- c(...)
    run(engine$exec,
        c(engine$args, command, inputs),
        error_on_status = error_on_status,
        wd = wd,
        echo_cmd = echo_cmd,
        echo = echo,
        spinner = spinner,
        timeout = timeout,
        stderr_to_stdout = stderr_to_stdout,
        env = env,
        windows_verbatim_args = windows_verbatim_args,
        windows_hide_window = windows_hide_window,
        encoding = encoding,
        cleanup_tree = cleanup_tree)
}

#' @export
file_input <- function(filename, sep = "::::") {
    c(sep, filename)
}

#' @export
vector_input <- function(values, sep = ":::") {
    c(sep, values)
}

#' @export
gnu_parallel <- function(..., exec = "parallel") {
    list(exec = exec, args = as.character(list(...)))
}
