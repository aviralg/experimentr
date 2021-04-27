
#' @importFrom R6 R6Class
#' @importFrom fs path_abs link_exists
FileSystemEntry <- R6Class(

    "FileSystemEntry",

    public = list(

        initialize = function(path) {
            private$.path <- path_abs(path)
        },

        path = function() {
            private$.path
        },

        name = function() {
            path_file(private$.path)
        },

        parent = function() {
            path_dir(private$.path)
        },

        link = function() {
            link_exists(self$path())
        }
    ),

    private = list(
        .path = NULL
    )
)
