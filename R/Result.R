
#' @export
#' @importFrom R6 R6Class
Result <- R6Class(
    "Result",

    inherit = Directory,

    public = list(

        initialize = function(path) {
            super$initialize(fs::path(path, "result"))
        },

        stdout = function() {
            super$file("stdout.txt")
        },

        exitcode = function() {
            super$file("stdout.int")
        },

        runtime = function() {
            super$file("runtime.csv")
        },

        successful = function() {
            self$exitcode()$read() == 0
        }
    )
)
