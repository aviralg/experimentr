
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_h3 style_bold
#' @importFrom cli cli_alert_success cli_alert_danger
#' @importFrom bench as_bench_time
#' @importFrom R6 R6Class
CommandLineInterface <- R6Class(

    "CommandLineInterface",

    public = list(
        initialize = function() {
        },

        setup = function() {
        },

        teardown = function() {
        },

        begin = function(task) {
            depth <- private$.increment_depth()
            fun <- if(depth == 1) {
                       cli_h1
                   } else if(depth == 2) {
                       cli_h2
                   } else {
                       cli_h3
                   }

            fun("Executing task {task$name()}")

        },

        end = function(task) {

            stdout <- task$result()$stdout()$read()
            runtime <- task$result()$runtime()$read()
            exitcode <- task$result()$exitcode()$read()
            stdout_size <- str_length(stdout)

            fun <- if(exitcode == 0) cli_alert_success else cli_alert_danger
            cat("\n")
            fun(style_bold("{task$name()} finished with exit code {exitcode} in {as_bench_time(runtime$real)}"))

            private$.decrement_depth()
        }
    ),

    private = list(
        .depth = 0,

        .increment_depth = function() {
            private$.update_depth(1)
        },

        .decrement_depth = function() {
            private$.update_depth(-1)
        },

        .update_depth = function(diff) {
            private$.depth <- private$.depth + diff
            private$.depth
        }
    )
)
