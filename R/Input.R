
#' @export
#' @importFrom R6 R6Class
Input <- R6Class(
    "Input",

    inherit = Directory,

    public = list(

        initialize = function(path, inputs) {
            super$initialize(path)

            walk(inputs, ~self$dir(.$name(), .$store()$output()$path()))
        }
    )
)
