
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
#' @importFrom readr write_csv
#' @importFrom fs path_join path_ext_remove
#' @importFrom purrr map2_chr pmap_chr
#' @importFrom stringr str_glue_data
#' @importFrom magrittr %>%
tracing_index <- function(index_file,
                          indir,
                          outdir,
                          expr_index_file,
                          outdir_index_file,
                          logdir_index_file,
                          packages = installed.packages()[,1],
                          types = c("test", "testthat", "example", "vignette"),
                          test_wrapper = "trace <- trace_file('{file}'); experimentr::write_trace(trace, '{outdir}')",
                          testthat_wrapper = "trace <- trace_expr(testthat::test_file({file}, package='{package}')); experimentr::write_trace(trace, '{outdir}')",
                          example_wrapper = "trace <- trace_file('{file}'); experimentr::write_trace(trace, '{outdir}')",
                          vignette_wrapper = "trace <- trace_file('{file}');  experimentr::write_trace(trace, '{outdir}')") {

    path_join2 <- function(x, y) path_join(c(x, y))

    df <- read_fst(index_file) %>%
        filter(type %in% types) %>%
        filter(package %in% packages)

    file_suffix <- pmap_chr(df,
                            function(type, package, subdir, filename) {
                                path_join(c(indir, type, package, subdir, filename))
                            })

    file_kernel = path_ext_remove(file_suffix)

    file <- map2_chr(indir, file_suffix, path_join2)

    outdir <- map2_chr(outdir, file_kernel, path_join2)

    logdir <- paste0(outdir, "/")

    df <- mutate(df, file = file, outdir = outdir, logdir = logdir)

    gen_expr <- function(gen_type, wrapper) {
        df %>%
            filter(type == gen_type) %>%
            str_glue_data(wrapper)
    }

    exprs <- c()


    if("testthat" %in% types) {
        exprs <- c(exprs, gen_expr("testthat", testthat_wrapper))
    }

    if("test" %in% types) {
        exprs <- c(exprs, gen_expr("test", test_wrapper))
    }

    if("example" %in% types) {
        exprs <- c(exprs, gen_expr("example", example_wrapper))
    }

    if("vignette" %in% types) {
        exprs <- c(exprs, gen_expr("vignette", vignette_wrapper))
    }

    write_csv(exprs, expr_index_file)
    write_csv(outdirs, outdir_index_file)
    write_csv(logdirs, logdir_index_file)
}

