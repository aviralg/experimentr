
#' @export
#' @importFrom R6 R6Class
#' @importFrom fs path
Output <- R6Class(
    "Output",

    inherit = Directory,

    public = list(

        initialize = function(path) {
            super$initialize(path(path, "output"))
        }
    )
)
