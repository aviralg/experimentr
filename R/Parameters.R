
#' @export
#' @importFrom rlang new_environment
#' @importFrom R6 R6Class
Parameters <- R6Class(

    "Parameters",

    public = list(

        get_expr = function() {
            private$.expr
        },

        get_eval_env = function() {
            private$.eval_env
        },

        lookup = function(name, default) {
            if(!exists(name, envir = private$.config_env)) {
                if(missing(default)) {
                    stop(sprintf("parameter %s not found", name))
                }
                else {
                    default
                }
            }
            else {
                get(name, envir = private$.config_env, inherits=TRUE)
            }
        },

        initialize = function(..., eval_env = parent.frame()) {
            private$.params <- eval(substitute(expression(...)))

            if("" %in% names(private$.params)) {
                stop("parameter cannot be unnamed")
            }

            private$.eval_env <- eval_env
        },

        setup = function(bindings) {
            config_env <- new_environment(list(),
                                          parent = new_environment(bindings,
                                                                   parent = private$.eval_env))

            var_names <- names(private$.params)

            for(i in seq2_along(1, private$.params)) {
                var_name <- var_names[[i]]
                expr <- private$.params[[i]]
                eval(
                    substitute(
                        delayedAssign(VAR_NAME,
                                      EXPR,
                                      eval.env = config_env,
                                      assign.env = config_env),
                        list(VAR_NAME = var_name, EXPR = expr)
                    )
                )
            }

            for(var in var_names) {
                get(var, envir = config_env)
            }

            parent.env(parent.env(config_env)) <- emptyenv()

            private$.config_env <- config_env
        },

        teardown = function() {
            private$.config_env <- NULL
        }
    ),

    private = list(
        .expr = NULL,
        .params = NULL,
        .eval_env = NULL,
        .config_env = NULL
    )
)
