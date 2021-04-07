
#' @export
#' @importFrom utils installed.packages
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows slice pull arrange desc count filter
#' @importFrom stringr str_trim str_remove str_split
#' @importFrom fs path_dir dir_create
#' @importFrom readr write_lines
select_packages <- function(rank = 1:500,
                            packages = installed.packages()[,1],
                            fields = c("Depends", "Imports"),
                            corpusfile = NULL,
                            clientfile = NULL,
                            ignore = c("base", "compiler", "datasets", "grDevices",
                                       "graphics", "grid", "methods", "parallel",
                                       "profile", "splines", "stats", "stats4",
                                       "tcltk", "tools", "translations", "utils")) {

    ## filter only from list of supplied packages and fields
    package_table <- installed.packages()[packages, fields]

    dependencies <- package_table[,fields[1]]

    for(field in fields[-1]) {
        dependencies <- paste(dependencies, ", ", package_table[, field])
    }

    dependencies <-
        dependencies %>%
        str_split(",") %>%
        map(function(s) {
            str_trim(str_remove(s, "\\([a-zA-Z0-9.>=<[:space:]-]*\\)"))
        })

    df <-
        lapply(1:length(dependencies),
           function(index) {
               tibble(client = packages[index],
                      corpus = dependencies[[index]])

           }) %>%
    bind_rows() %>%
    filter(!(corpus %in% c("", "R", "NA", ignore)))

    corpuses <-
        df %>%
        count(corpus, name = "count") %>%
        arrange(desc(count)) %>%
        slice(rank) %>%
        pull(corpus)

    clients <-
        df %>%
        filter(corpus %in% corpuses) %>%
        pull(client) %>%
        unique() %>%
        setdiff(corpuses)

    if(!is.null(corpusfile)) {
        dir_create(path_dir(corpusfile))
        write_lines(corpuses, corpusfile)
    }

    if(!is.null(clientfile)) {
        dir_create(path_dir(clientfile))
        write_lines(clients, clientfile)
    }

    list(corpus = corpuses, client = clients)
}

#' @export
#' @importFrom utils installed.packages
#' @importFrom stringr str_split str_trim str_remove
#' @importFrom rlang seq2_along
#' @importFrom dplyr filter
#' @importFrom purrr map_dfr
package_table <- function(fields = c("Depends", "Imports"),
                          ignore = c("base", "compiler", "datasets", "grDevices",
                                     "graphics", "grid", "methods", "parallel",
                                     "profile", "splines", "stats", "stats4",
                                     "tcltk", "tools", "translations", "utils")) {

    ## filter only from list of supplied packages and fields
    tbl <- installed.packages()

    packages <- unname(tbl[,1])

    tbl <- tbl[, fields]

    dependencies <- tbl[,fields[1]]

    for(field in fields[-1]) {
        dependencies <- paste(dependencies, ", ", tbl[, field])
    }

    dependencies <-
        dependencies %>%
        str_split(",") %>%
        map(function(s) {
            str_trim(str_remove(s, "\\([a-zA-Z0-9.>=<[:space:]-]*\\)"))
        })

    df <-
        seq2_along(1, dependencies) %>%
        map_dfr(~tibble(package = dependencies[[.]], client = packages[.])) %>%
        filter(!(package %in% c("", "R", "NA", ignore))) %>%
        filter(!(client %in% c("", "R", "NA", ignore)))

    df
}
