
#' @export
#' @importFrom R6 R6Class
Result <- R6Class(
    "Result",

    inherit = Directory,

    public = list(

        initialize = function(path) {
            super$initialize(path)
        },

        stdout = function() {
            super$file("stdout.txt")
        },

        exitcode = function() {
            super$file("exitcode.int")
        },

        runtime = function() {
            super$file("runtime.csv")
        },

        successful = function() {
            self$exitcode()$read() == 0
        }
    )
)
