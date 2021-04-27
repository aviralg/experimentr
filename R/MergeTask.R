
#' @export
#' @importFrom R6 R6Class
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_chr
#' @importFrom bench bench_time
MergeTask <- R6Class(

    "MergeTask",

    inherit = Task,

    public = list(

        initialize = function(name, description, ...) {
            super$initialize(name, description)

            private$.tasks <- list(...)
            names(private$.tasks) <- map_chr(private$.tasks, function(task) task$name())
        },

        setup = function(store) {
            super$setup(store)

            for (task in private$.tasks) {
                task$setup(store)
            }
        },

        teardown = function() {
            super$teardown()

            for (task in private$.tasks) {
                task$teardown()
            }
        },

        execute = function(executor) {

            stdouts <- character(0)
            exitcodes <- integer(0)
            runtimes <- NULL

            time <- bench_time({
                for (task in private$.tasks) {
                    result <- executor$execute(task)

                    stdout <- result$stdout()$read()
                    stdouts <- c(stdouts, stdout)

                    exitcode <- result$exitcode()$read()
                    exitcodes <- c(exitcodes, exitcode)

                    runtime <- result$runtime()$read()
                    runtimes <- bind_rows(runtimes, runtime)

                    if (!result$successful()) {
                        break
                    }
                }
            })

            stdout <- paste(stdouts, collapse = "\n")
            self$result()$stdout()$write(stdout)

            exitcode <- exitcodes[length(exitcodes)]
            self$result()$exitcode()$write(exitcode)

            runtime <- tibble(process = sum(runtimes$process), real = sum(runtimes$real))
            self$result()$runtime()$write(runtime)

            self$result()
        }

    ),

    private = list(
        .tasks = list()
    )
)

#' @export
`%<+>%` <- function(task1, task2) {

    name1 <- task1$name()
    name2 <- task2$name()

    name <- paste0("(", name1, "+", name2, ")")

    description <- name

    MergeTask$new(name, description, task1, task2)
}
