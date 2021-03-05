
#' @export
#' @importFrom fst read_fst write_fst
#' @importFrom purrr map_dfr
#' @importFrom fs path_dir file_delete dir_delete dir_ls
merge_tables <- function(files,
                         output_filepath,
                         remove_files = TRUE,
                         remove_empty_dirs = TRUE,
                         reader = read_fst,
                         writer = write_fst) {

    df <- map_dfr(files, reader)

    if(!is.null(output_filepath)) {
        writer(df, output_filepath)
    }

    if(remove_files) {
        file_delete(files)

        if(remove_empty_dirs) {
            dirs <- unique(path_dir(files))
            walk(dirs, function(dir) {
                n <- length(dir_ls(dir))
                if(n == 0) {
                    dir_delete(dir)
                }
            })
        }
    }

    df

}

#' @export
#' @importFrom purrr map_chr
#' @importFrom fs path_join dir_delete file_delete
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @importFrom readr read_table
merge_logs <- function(log_dirs,
                       job_log,
                       output_filepath,
                       remove_logs = FALSE,
                       remove_job_log = FALSE,
                       reader = read_file,
                       writer = write_fst) {
    read_log <- function(dirs, type) {
        map_chr(dirs, function(dir) path_join(list(c(dir, type))))
    }

    stderrs <- map_chr(read_log(log_dirs, "stderr"), reader)
    stdouts <- map_chr(read_log(log_dirs, "stdout"), writer)
    seqs <- map_chr(path_join(list(log_dirs, "seq")), reader)

    log_table <- tibble(seq = seqs, stderr = stderrs, stdout = stdouts)

    job_log <- read_table(job_log)

    result <- left_join(job_log, log_table, by = "seq")

    if(!is.null(output_filepath)) {
        writer(result, output_filepath)
    }

    if(remove_logs) {
        dir_delete(log_dirs)
    }

    if(remove_job_log) {
        file_delete(job_log)
    }

    result
}
