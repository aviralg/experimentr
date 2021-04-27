
#' @export
#' @importFrom R6 R6Class
#' @importFrom processx run
#' @importFrom purrr map_chr
ProcessTask <- R6Class(

    "ProcessTask",

    inherit = ExpressionTask,

    public = list(

        command = function() {
            private$.command
        },

        arguments = function() {
            private$.arguments
        },

        working_directory = function() {
            private$.wd
        },

        echo_command = function() {
            private$.echo_cmd
        },

        echo = function() {
            private$.echo
        },

        encoding = function() {
            private$.encoding
        },

        cleanup_tree = function() {
            private$.cleanup_tree
        },

        initialize = function(name,
                              description,
                              command,
                              ...,
                              wd = NULL,
                              echo_cmd = TRUE,
                              echo = TRUE,
                              encoding = "",
                              cleanup_tree = FALSE) {

            private$.command <- substitute(command)
            private$.arguments <- eval(substitute(expression(...)))
            private$.wd <- substitute(wd)
            private$.echo_cmd <- substitute(echo_cmd)
            private$.echo <- substitute(echo)
            private$.encoding <- substitute(encoding)
            private$.cleanup_tree <- substitute(cleanup_tree)

            expr <- substitute({

                args <- map_chr(ARGUMENTS, function(arg) eval(arg))
                cmd <- eval(COMMAND)

                processx::run(cmd,
                              args,
                              wd = WD,
                              echo_cmd = ECHO_CMD,
                              echo = ECHO,
                              encoding = ENCODING,
                              cleanup_tree = CLEANUP_TREE)

            }, list(COMMAND = private$.command,
                    ARGUMENTS = private$.arguments,
                    WD = private$.wd,
                    ECHO_CMD = private$.echo_cmd,
                    ECHO = private$.echo,
                    ENCODING = private$.encoding,
                    CLEANUP_TREE = private$.cleanup_tree)
            )

            super$initialize(name, description, expr, quote = FALSE)
        }
    ),

    private = list(
        .command = NULL,
        .arguments = NULL,
        .wd = NULL,
        .echo_cmd = NULL,
        .echo = NULL,
        .encoding = NULL,
        .cleanup_tree = NULL
    )
)
