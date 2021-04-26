
#' @export
#' @importFrom R6 R6Class
GroupTask <- R6Class(

    "GroupTask",

    inherit = Task,

    public = list(

        task = function(name_or_pos) {
            task <- private$.tasks[[name_or_pos]]

            if(is.null(task)) {
                msg <- sprintf("task %s does not have a task %s", self$name(), name_or_pos)
                stop(msg)
            }

            task
        },

        tasks = function() {
            private$.tasks
        },

        initialize = function(name, description, tasks) {

            super$initialize(name, description)

            private$.tasks <- tasks
            names(private$.tasks) <- map_chr(tasks, function(task) task$name())
        }
    ),

    private = list(
        .tasks = list()
    )
)
