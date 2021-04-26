
#' @export
#' @importFrom R6 R6Class
#' @importFrom docopt docopt
#' @importFrom stringr str_split fixed str_sub str_starts
#' @importFrom purrr map_chr
CommandLineExecutorInput <- R6Class(

    "CommandLineExecutorInput",

    inherit = ExecutorInput,

    public = list(

        usage = function() {
            paste("",
                  "experiment",
                  "",
                  "Usage:",
                  "experiment [--verbose=<LEVEL>] <TASK> [-p <PARAMETER>...] [-e <ENVVAR>...]",
                  "experiment <TASK> --describe",
                  "experiment --describe",
                  "experiment --help",
                  "",
                  "Options:",
                  "-h, --help             Show this message or describe task.",
                  "-d, --describe         Describe task structure.",
                  "-v, --verbose=<LEVEL>  Set verbosity level [default: 1].",
                  "-p, --parameters       Configuration parameters.",
                  "-e, --envvars          Environment variables.",
                  "",
                  sep = "\n")
        },

        initialize = function(args = commandArgs(TRUE)) {
            result <- docopt(self$usage(), args)

            parameters <- str_split(result$PARAMETER, fixed("="))
            par_names <- map_chr(parameters, ~.[[1]])
            parameters <- map(parameters, function(par) {
                par <- par[[2]]
                if (str_starts(par, fixed("!"))) {
                    par <- eval(parse(text = str_sub(par, 2)), envir = globalenv())
                }
                par
            })
            names(parameters) <- par_names

            envvars <- str_split(result$ENVVAR, fixed("="))
            env_names <- map_chr(envvars, ~.[[1]])
            envvars <- map_chr(envvars, ~.[[2]])
            names(envvars) <- env_names

            super$initialize(result$TASK,
                             parameters,
                             envvars,
                             as.integer(result$verbose),
                             result$describe)
        }
    )
)
