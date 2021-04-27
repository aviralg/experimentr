#' @export
#' @importFrom R6 R6Class
#' @importFrom fs dir_create link_create path
Directory <- R6Class(

    "Directory",

    inherit = FileSystemEntry,

    public = list(

        initialize = function(path, link = NULL) {
            super$initialize(path)

            if (is.null(link)) {
                dir_create(path)
            } else {
                dir_create(path_dir(path))
                link_create(link, path)
            }
        },

        dir = function(dirname) {
            Directory$new(path(super$path(), dirname))
        },

        file = function(filename) {
            File$new(path(super$path(), filename))
        }
    )
)
