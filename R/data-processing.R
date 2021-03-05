
#' @export
#' @importFrom fst read_fst write_fst
#' @importFrom purrr map_dfr
#' @importFrom fs path_dir file_delete dir_delete dir_ls
merge_tables <- function(files,
                         output_filepath,
                         reader = read_fst,
                         writer = write_fst,
                         remove_files = TRUE,
                         remove_empty_dirs = TRUE) {

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
