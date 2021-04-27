
#' @export
#' @importFrom R6 R6Class
#' @importFrom fs path
Store <- R6Class(
    "Store",

    inherit = Directory,

    public = list(

        initialize = function(path, output, result, input, link = NULL) {
            super$initialize(path, link)

            private$.output <- output
            private$.result <- result
            private$.input <- input
        },

        output = function() {
            private$.output
        },

        result = function() {
            private$.result
        },

        input = function() {
            private$.input
        }
    ),

    private = list(
        .output = NULL,
        .result = NULL,
        .input = NULL
    )
)
