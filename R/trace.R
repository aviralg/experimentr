
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
#' @importFrom readr write_lines
#' @importFrom fs path_join path_ext_remove
#' @importFrom purrr map2_chr pmap_chr
#' @importFrom stringr str_glue_data str_starts
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
tracing_index <- function(index_file,
                          indir,
                          outdir,
                          expr_index_file,
                          outdir_index_file,
                          logdir_index_file,
                          packages = installed.packages()[,1],
                          types = c("test", "testthat", "example", "vignette"),
                          test_wrapper = "trace <- trace_file('{file}'); experimentr::write_trace(trace, '{outdir}')",
                          testthat_wrapper = "trace <- trace_expr(testthat::test_file('{file}', package='{package}')); experimentr::write_trace(trace, '{outdir}')",
                          example_wrapper = "trace <- trace_file('{file}'); experimentr::write_trace(trace, '{outdir}')",
                          vignette_wrapper = "trace <- trace_file('{file}');  experimentr::write_trace(trace, '{outdir}')") {

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

    logdirs <- outdirs

    gen_expr <- function(gen_type, wrapper) {
        df %>%
            filter(type == gen_type) %>%
            str_glue_data(wrapper)
    }

    df <- mutate(df, file = file, outdir = outdirs, logdir = logdirs)

    df <-
        df %>%
        mutate(expr = case_when(type == "testthat" ~ str_glue_data(., testthat_wrapper),
                                type == "test" ~ str_glue_data(., test_wrapper),
                                type == "example" ~ str_glue_data(., example_wrapper),
                                type == "vignette" ~ str_glue_data(., vignette_wrapper)))

    write_lines(df$expr, expr_index_file)
    write_lines(df$outdir, outdir_index_file)
    write_lines(df$logdir, logdir_index_file)

    select(df, expr, outdir, logdir)
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
                        result = FALSE,
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

    if(result && is.null(trace$result$error)) {
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
                             call = as.character(trace$result$error$call),
                             source = trace$result$error$source,
                             .sep = "\n")

        write_file(contents, filepath)
    }
}
