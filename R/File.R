
#' @export
#' @importFrom R6 R6Class
#' @importFrom fs file_exists
File <- R6Class(

    "File",

    inherit = FileSystemEntry,

    public = list(

        initialize = function(path) {
            super$initialize(path)
        },

        dir = function() {
            Directory$new(super$parent())
        },

        extension = function() {
            path_ext(super$path())
        },

        exists = function() {
            file_exists(super$path())
        },

        read = function(reader = select_reader(self$extension())) {
            ## TODO: cache
            reader(super$path())
        },

        write = function(content, writer = select_writer(self$extension())) {
            writer(content, super$path())
        }
    )
)
