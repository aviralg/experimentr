
#' @export
#' @importFrom R6 R6Class
ExecutorInput <- R6Class(

    "ExecutorInput",

    public = list(
        task = function() {
            private$.task
        },

        set_task = function(task) {
            private$.task <- task
            self
        },

        parameters = function() {
            private$.parameters
        },

        set_parameters = function(parameters) {
            private$.parameters <- parameters
            self
        },

        envvars = function() {
            private$.envvars
        },

        set_envvars = function(envvars) {
            private$.envvars <- envvars
            self
        },

        verbose = function() {
            private$.verbose
        },

        set_verbose = function(verbose) {
            private$.verbose <- verbose
            self
        },

        describe = function() {
            private$.describe
        },

        set_describe = function(describe) {
            private$.describe <- describe
            self
        },

        initialize = function(task = NULL,
                              parameters = list(),
                              envvars = list(),
                              verbose = 1,
                              describe = FALSE) {
            self$set_task(task)
            self$set_parameters(parameters)
            self$set_envvars(envvars)
            self$set_verbose(verbose)
            self$set_describe(describe)
        }
    ),

    private = list(
        .task = NULL,
        .parameters = list(),
        .envvars = list(),
        .verbose = 1,
        .describe = FALSE
    )
)
