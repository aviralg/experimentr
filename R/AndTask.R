#' @export
#' @importFrom R6 R6Class
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom bench bench_time
AndTask <- R6Class(

    "AndTask",

    inherit = GroupTask,

    public = list(

        initialize = function(name, description, ...) {
            tasks <- list(...)

            super$initialize(name, description, tasks)
        },

        execute = function(executor) {

            results <- list()

            time <- bench_time({
                for(task in self$tasks()) {
                    result <- executor$execute(task)
                    results <- c(results, result)
                    if(!result$successful()) {
                        break
                    }
                }
            })

            stdouts <- map_chr(results, function(result) result$stdout()$read())
            stdout <- paste(stdouts, collapse = "\n")
            self$result()$stdout()$write(stdout)

            exitcode <- results[[length(results)]]$exitcode()
            self$result()$exitcode()$write(exitcode$read())

            runtime <- tibble(process = time[1], real = time[2])
            self$result()$runtime()$write(runtime)
        }
    )
)

#' @export
`%<&>%` <- function(task1, task2) {

    name1 <- task1$name()
    name2 <- task2$name()

    name <- paste0("(", name1, "&", name2, ")")

    description <- name

    AndTask$new(name, description, task1, task2)
}
