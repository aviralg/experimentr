
#' @export
#' @importFrom R6 R6Class
#' @importFrom withr with_output_sink with_tempdir
#' @importFrom rlang new_environment
#' @importFrom bench bench_time
#' @importFrom tibble tibble
ExpressionTask <- R6Class(

    "ExpressionTask",

    inherit = Task,

    public = list(

        expression = function() {
            private$.expression
        },

        environment = function() {
            private$.environment
        },

        initialize = function(name,
                              description,
                              expression,
                              complete,
                              quote = TRUE,
                              environment = parent.frame()) {

            super$initialize(name, description)

            private$.expression <- if (quote) substitute(expression) else expression

            private$.environment <- environment
        },

        execute = function(executor) {

            ## extend eval env with these 3 variables
            task_env <- new_environment(list(self = self,
                                             root = executor$root(),
                                             params = executor$parameters()),
                                        private$.environment)

            ## extend it further with new env to capture expression effects
            task_env <- new_environment(list(), task_env)

            expression <- substitute(
                tryCatch(
                    list(EXPR, 0),
                    error = function(e) {
                        print(e)
                        list(NULL, 1)
                    }
                ),
                list(EXPR = self$expression())
            )

            time <- unclass(
                bench_time({
                    result <-
                        with_output_sink(
                            self$result()$stdout()$path(),
                            with_tempdir(eval(expression, envir=task_env)),
                            append = FALSE,
                            split = TRUE
                        )
                })
            )

            self$result()$exitcode()$write(result[[2]])

            runtime <- tibble(process = time[1], real = time[2])
            self$result()$runtime()$write(runtime)

            self$result()
        }
    ),

    private = list(
        .expression = NULL,
        .environment = NULL
    )
)
