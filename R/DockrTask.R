
#' @export
#' @importFrom R6 R6Class
#' @importFrom gert git_clone git_branch git_remote_info git_pull
#' @importFrom fs dir_ls path_ext_remove path_file
DockrTask <- R6Class(

    "GitRepositoryTask",

    inherit = ExpressionTask,

    public = list(
        url = function() {
            private$.url
        },

        branch = function() {
            private$.branch
        },

        project = function() {
            private$.project
        },

        initialize = function(url, branch, name = NULL, description = NULL) {
            project <- path_ext_remove(path_file(url))

            if (is.null(name)) {
                name <- paste(project, branch, sep = "-")
            }

            if (is.null(description)) {
                description <- sprintf("Clone %s branch %s from %s.", project, branch, url)
            }

            private$.url <- url
            private$.branch <- branch
            private$.project <- project

            expr <- substitute({
                path <- self$output()$path()

                if (length(dir_ls(all = TRUE))) {
                    git_clone(url = URL, path = path, branch = BRANCH, verbose = TRUE)
                }
                else {
                    branch <- tryCatch(git_branch(path))
                    url <- tryCatch(git_remote_info(repo = path)$url)

                    if (branch != BRANCH | url != URL) {
                        msg <- sprintf("directory %s it not the expected git repository with url %s and branch %s",
                                       path, URL, BRANCH)
                        stop(msg)
                    }
                    else {
                        git_pull(repo = path)
                    }
                }
            }, list(URL = url,
                    BRANCH = branch)
            )

            super$initialize(name, description, expr, quote = FALSE)
        }
    ),

    private = list(
        .url = NULL,
        .branch = NULL,
        .project = NULL
    )
)
