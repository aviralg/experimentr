
is_exported <- function(ns, name) {
    tryCatch({
        getExportedValue(ns, name);
        TRUE
    },
    error = function(e) {
        FALSE
    })
}

get_ns_path <- function(ns) {
    path <- NA_character_

    namespace <- get0(".__NAMESPACE__.", ns, inherits = FALSE, ifnotfound = NULL)

    if(!is.null(namespace)) {
        path <- get0("path", namespace, inherits = FALSE, ifnotfound = NA_character_)
    }
    path
}

#' @importFrom tibble tibble
get_package_function_info <- function(name, ns) {
    fun <- get0(name, ns)
    args <- formals(fun)
    defaults <- as.character(args)
    defaults[defaults == ""] <- NA_character_
    argnames <- as.character(names(args))
    exported <- is_exported(ns, name)

    tibble(funname = name,
           exported = exported,
           argument = argnames,
           default = defaults)
}

#' @importFrom purrr keep map_dfr
#' @importFrom tibble add_column
get_one_package_info <- function(package) {
    ns <- getNamespace(package)
    path <- get_ns_path(ns)

    names <- ls(ns, all.names = TRUE)

    is_function <- function(name) is.function(get0(name, ns, inherits = FALSE))
    names <- purrr::keep(names, is_function)

    res <- map_dfr(names, get_package_function_info, ns)

    add_column(res, package = package, path = path, .before = 1)
}


#' @importFrom purrr map_dfr
#' @importFrom fst write_fst
#' @importFrom fs dir_create path_dir
#' @importFrom progress progress_bar
#' @export
get_package_info <- function(packages, progress = FALSE, output_filepath = NULL) {

    if(progress) {
        pb <- progress_bar$new(format = "Processing :what [:bar] :current/:total (:percent) eta: :eta",
                               total = length(packages),
                               width = 80)
    }

    helper  <- function(package, ...) {
        if(progress) pb$tick(tokens = list(what = pad(package, 15)))
        get_one_package_info(package, ...)
    }

    result <- map_dfr(packages, helper)

    if(!is.null(output_filepath)) {
        dir_create(path_dir(output_filepath), recurse = TRUE)
        write_fst(result, output_filepath)
    }

    result
}
