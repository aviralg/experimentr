
#' @export
#' @importFrom fst write_fst
#' @importFrom fs dir_create path_join path_ext_set
write_tracing_result <- function(result, dir) {
    dir_create(dir, recurse = TRUE)
    data <- result$data
    for(name in names(data)) {
        file <- path_ext_set(path_join(c(dir, name)), ext = "fst")
        write_fst(data[[name]], file)
    }
}


#' @export
#' @importFrom utils installed.packages
#' @importFrom dplyr filter case_when
#' @importFrom fst read_fst
#' @importFrom readr write_lines write_file read_file
#' @importFrom fs path_join path_ext_remove
#' @importFrom purrr map2_chr pmap_chr map2
#' @importFrom stringr str_glue_data str_starts
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
tracing_index <- function(index_file,
                          indir,
                          outdir,
                          outfile_index_file,
                          logdir_index_file,
                          packages = installed.packages()[,1],
                          types = c("test", "testthat", "example", "vignette"),
                          test_wrapper = "trace <- trace_expr('{code}'); experimentr::write_trace(trace, '{outdir}')",
                          testthat_wrapper = "trace <- trace_expr(testthat::test_file('{file}', package='{package}')); experimentr::write_trace(trace, '{outdir}')",
                          example_wrapper = "trace <- trace_expr('{code}'); experimentr::write_trace(trace, '{outdir}')",
                          vignette_wrapper = "trace <- trace_expr('{code}');  experimentr::write_trace(trace, '{outdir}')") {

    path_join2 <- function(x, y) path_join(c(x, y))

    df <- read_fst(index_file) %>%
        filter(type %in% types) %>%
        filter(package %in% packages) %>%
        filter(type != "testthat" | str_starts(filename, "test-"))

    file_suffix <- pmap_chr(df,
                            function(type, package, subdir, filename) {
                                path_join(c(type, package, subdir, filename))
                            })

    file_kernel = path_ext_remove(file_suffix)

    file <- map2_chr(indir, file_suffix, path_join2)

    outdirs <- map2_chr(outdir, file_kernel, path_join2)

    outfiles <- map2_chr(outdirs, "program.R", path_join2)

    logdirs <- outdirs

    df <-
        df %>%
        mutate(file = file, outdir = outdirs, outfile = outfiles, logdir = logdirs) %>%
        mutate(code = map_chr(file, read_file)) %>%
        mutate(expr = case_when(type == "testthat" ~ str_glue_data(., testthat_wrapper),
                                type == "test" ~ str_glue_data(., test_wrapper),
                                type == "example" ~ str_glue_data(., example_wrapper),
                                type == "vignette" ~ str_glue_data(., vignette_wrapper)))

    ## create directories to store wrapped programs
    dir_create(outdirs)

    ## save programs
    write_file_checked <- function(expr, file) {
        tryCatch(write_file(expr, file),
                 error = function(e) {
                     print(e)
                     stop(sprintf("Error writing to file %s content %s", file, expr))
                 })
    }
    map2(df$expr, df$outfile, write_file_checked)

    write_lines(df$outfile, outfile_index_file)
    write_lines(df$logdir, logdir_index_file)

    select(df, expr, outfile, logdir)
}

#' @importFrom fst write_fst
#' @importFrom fs path_join path_ext_set
write_tables <- function(tables, outdir) {

    if(is.null(tables)) {
        return()
    }

    table_names <- names(tables)

    for(table_name in table_names) {
        table <- tables[[table_name]]
        filepath <- path_ext_set(path_join(c(outdir, table_name)), "fst")
        write_fst(table, filepath)
    }
}

#' @export
#' @importFrom stringr str_glue
#' @importFrom fs path_join path_ext_set dir_create
#' @importFrom readr write_file
write_trace <- function(trace,
                        outdir,
                        output = TRUE,
                        statistics = TRUE,
                        value = FALSE,
                        error = TRUE) {
    if(output) {
        output_dir <- path_join(c(outdir, "output"))
        dir_create(output_dir)
        write_tables(trace$output, output_dir)
    }

    if(statistics) {
        statistics_dir <- path_join(c(outdir, "statistics"))
        dir_create(statistics_dir)
        write_tables(trace$statistics, statistics_dir)
    }

    result_dir <- path_join(c(outdir, "result"))

    if(value && is.null(trace$result$error)) {
        dir_create(result_dir)

        filepath <- path_ext_set(path_join(c(result_dir, "value")), "RDS")

        saveRDS(trace$result$value, filepath)
    }

    if(error && !is.null(trace$result$error)) {
        dir_create(result_dir)

        filepath <- path_ext_set(path_join(c(result_dir, "error")), "txt")

        contents <- str_glue("MESSAGE: {message}",
                             "CALL:    {call}",
                             "SOURCE:  {source}",
                             message = trace$result$error$message,
                             call = deparse(trace$result$error$call),
                             source = trace$result$error$source,
                             .sep = "\n")

        write_file(contents, filepath)
    }

    invisible(trace)
}

#' @export
#' @importFrom fst read_fst
#' @importFrom readr read_csv read_file
#' @importFrom fs path_ext is_dir
read_any <- function(path, lazy = TRUE, ...) {

    if(is_dir(path)) {
        dir_as_env(path, lazy = lazy, ...)
    }
    else {
        ext <- path_ext(path)

        if(ext == "fst") {
            read_fst(path)
        }
        else if(ext == "csv") {
            read_csv(path)
        }
        else {
            read_file(path)
        }
    }
}

#' @export
#' @importFrom purrr walk
#' @importFrom fs path_ext_remove dir_ls path_file is_dir
dir_as_env <- function(path, lazy = TRUE, ...) {

    if(!is_dir(path)) {
        stop(sprintf("argument %s is not a directory", path), call. = TRUE)
    }

    paths <- dir_ls(path, ...)

    env <- new.env(parent = emptyenv())

    walk(paths, function(path) {
        path <- path
        filename <- path_ext_remove(path_file(path))
        if(lazy) {
            delayedAssign(filename, read_any(path, lazy = lazy, ...), assign.env = env)
        }
        else {
            assign(filename, value = read_any(path, lazy = lazy, ...), envir = env)
        }
    })

    env
}
