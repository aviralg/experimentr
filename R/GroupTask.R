
#' @export
#' @importFrom R6 R6Class
#' @importFrom purrr map_chr
GroupTask <- R6Class(

    "GroupTask",

    inherit = Task,

    public = list(

        independent = function() {
            private$.independent
        },

        task = function(name_or_pos) {
            task <- private$.tasks[[name_or_pos]]

            if (is.null(task)) {
                msg <- sprintf("task %s does not have a task %s", self$name(), name_or_pos)
                stop(msg)
            }

            task
        },

        tasks = function() {
            private$.tasks
        },

        setup = function(store) {
            super$setup(store)

            store_path <- store$path()

            for (task in self$tasks()) {
                output_path <- path(store_path, "tasks", task$name(), "output")
                link <- if (self$independent()) NULL else store$output()$path()
                output <- Directory$new(output_path, link)

                result_path <- path(store_path, "tasks", task$name(), "result")
                result <- Result$new(result_path)

                input_path <- path(store_path, "tasks", task$name(), "input")
                input <- Input$new(input_path, task$inputs())

                task_store <- Store$new(store_path, output, result, input)
                task$setup(task_store)
            }
        },

        teardown = function() {
            super$teardown()

            for (task in self$tasks()) {
                task$teardown()
            }
        },

        initialize = function(name, description, tasks, independent) {
            super$initialize(name, description)

            private$.tasks <- tasks
            names(private$.tasks) <- map_chr(tasks, function(task) task$name())
            private$.independent <- independent
        }
    ),

    private = list(
        .tasks = list(),
        .independent = NULL
    )
)
