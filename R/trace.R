
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
#' @importFrom dplyr filter
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

    logdirs <- paste0(outdirs, "/")

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

    select(df, exprs, outdirs, logdirs)
}

