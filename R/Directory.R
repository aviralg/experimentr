
#' @export
#' @importFrom R6 R6Class
#' @importFrom fs dir_create path
Directory <- R6Class(

    "Directory",

    inherit = FileSystemEntry,

    public = list(

        initialize = function(path) {
            super$initialize(path)
            dir_create(path)
        },

        dir = function(dirname) {
            Directory$new(path(super$path(), dirname))
        },

        file = function(filename) {
            File$new(path(super$path(), filename))
        }
    )
)
