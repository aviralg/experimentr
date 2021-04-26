
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

        setup = function(config) {
        },

        execute = function(executor) {
            msg <- sprintf("method 'execute' not implemented for task %s",
                           self$name())
            stop(msg)
        },

        teardown = function(config) {
        }
    ),

    private = list(
        .name = NULL,
        .description = NULL,
        .output = NULL,
        .result = NULL
    )
)
