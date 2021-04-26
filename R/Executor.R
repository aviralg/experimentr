#' @export
#' @importFrom stringr str_split fixed
#' @importFrom rlang seq2_along
#' @importFrom R6 R6Class
#' @importFrom fs path_abs
Executor <- R6Class(

    "Executor",

    public = list(

        initialize = function(root, interface) {
            private$.root <- root
            private$.interface <- interface
        },

        task = function(task_name) {
            task_names <- str_split(task_name, fixed("/"))[[1]]

            root <- private$.root

            if(root$name() != task_names[1]) {
                msg <- sprintf("task %s not found", task_names[1])
                stop(msg)
            }

            for(task_index in seq2_along(2, task_names)) {
                task_name <- task_names[task_index]
                root <- root$task(task_name)
            }

            root
        },

        apply = function(input) {
            if(input$describe()) {
                self$describe(task = input$task())
            } else {
                private$.setup(input)
                self$execute(input$task())
                private$.teardown(input)
            }
        },

        execute = function(task) {

            if(typeof(task) == "character") {
                task <- self$task(task)
            }

            private$.interface$begin(task)
            result <- task$execute(self)
            private$.interface$end(task)

            result
        },

        root = function() {
            private$.root
        },

        parameters = function() {
            private$.parameters
        }

    ),

    private = list(
        .root = NULL,
        .parameters = NULL,
        .interface = NULL,

        .setup = function(input) {
            private$.parameters <- Parameters$new()
            private$.parameters$setup(input$parameters())
            private$add_store_(private$.root, path_abs("."))
        },

        .teardown = function(input) {
            private$.parameters$teardown()
            private$.parameters <- NULL
            ## TODO: remove store
        },

        add_store_ = function(task, parent_dir) {
            root_dir <- path(parent_dir, task$name())
            task$set_output(Output$new(root_dir))
            task$set_result(Result$new(root_dir))

            if(inherits(task, "GroupTask")) {
                for(sub_task in task$tasks()) {
                    private$add_store_(sub_task, root_dir)
                }
            }
        }
    )
)
