
#' @export
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect
Task <- R6Class(

    "Task",

    public = list(

        name = function() {
            private$.name
        },

        description = function() {
            private$.description
        },

        store = function() {
            private$.store
        },

        output = function() {
            private$.output
        },

        result = function() {
            private$.result
        },

        set_output = function(output) {
            private$.output <- output
        },

        set_result = function(result) {
            private$.result <- result
        },

        initialize = function(name, description) {

            if (str_detect(name, fixed("/"))) {
                msg <- sprintf("task name %s cannot have a /", name)
                stop(msg)
            }

            stopifnot(is.character(description))

            private$.name <- name
            private$.description <- description
        },

        execute = function(executor) {
            msg <- sprintf("method 'execute' not implemented for task %s",
                           self$name())
            stop(msg)
        },

        setup = function(store) {
            private$.store <- store
            private$.output <- store$output()
            private$.result <- store$result()
        },

        teardown = function() {
            private$.store <- NULL
            private$.output <- NULL
            private$.result <- NULL
        }
    ),

    private = list(
        .name = NULL,
        .description = NULL,
        .store = NULL,
        .output = NULL,
        .result = NULL
    )
)
