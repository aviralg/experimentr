#' @importFrom tools Rd2ex Rd_db
extract_examples_helper <- function(package,
                                    libraries = NULL,
                                    encoding = "UTF-8",
                                    comment_dont_run = TRUE,
                                    comment_dont_test = TRUE,
                                    prepend_library_load = TRUE) {

    create_result <- function(filenames, contents) {
        data.frame(package = if(length(filenames) == 0) character(0) else package,
                   filename = filenames,
                   content = contents)
    }

    db <- tryCatch({
        Rd_db(package, package_directory, libraries)
    }, error = function(e) {
        print(e)
        list()
    })

    if(length(db) == 0) {
        return(create_result(character(0), character(0)))
    }

    example_extractor <- function(example_filename) {
        elt <- db[[example_filename]]
        out <- textConnection("code", "w", local = TRUE)
        Rd2ex(elt,
              out,
              outputEncoding = encoding,
              commentDontrun = comment_dont_run,
              commentDonttest = comment_dont_test)
        close(out)

        code <- paste(code, sep = "\n", collapse = "\n");

        if(nchar(code) == 0) NA_character_

        else if(prepend_library_load) {
            library_load <- paste0("library", "(", package, ")")
            paste(library_load, code, sep = "\n")
        }

        else code
    }

    filenames <- names(db)
    contents <- sapply(filenames, example_extractor)

    na_indices <- is.na(contents)

    create_result(replace_extension(filenames[!na_indices], ".R"),
                  contents[!na_indices])

}

#' @export
#' @importFrom progress progress_bar
extract_examples <- function(packages,
                             libraries = NULL,
                             encoding = "UTF-8",
                             comment_dont_run = TRUE,
                             comment_dont_test = TRUE,
                             prepend_library_load = TRUE,
                             progress = TRUE) {

    if(progress) {
        pb <- progress_bar$new(format = "Processing :what [:bar] :current/:total (:percent) eta: :eta",
                               total = length(packages),
                               width = 80)
    }

    helper  <- function(package, ...) {
        if(progress) pb$tick(tokens = list(what = pad(package, 15)))
        extract_examples_helper(package, ...)
    }

    dfs <- lapply(packages,
                  helper,
                  libraries,
                  encoding,
                  comment_dont_run,
                  comment_dont_test,
                  prepend_library_load)

    do.call(rbind, dfs)
}

#' @export
write_examples <- function(examples, output_dirpath) {

    if(nrow(examples) == 0) {
        examples$filepath <- character(0)
        return(examples)
    }

    example_writer <- function(row) {
        filename <- examples[row, "filename"]
        content <- examples[row, "content"]
        filepath <- file.path(output_dirpath, filename)
        cat(content, file = filepath, append = FALSE)
        filepath
    }

    dir.create(output_dirpath, showWarnings = FALSE)

    examples$filepath <- sapply(1:nrow(examples), example_writer)

    examples
}
