#' @export
#' @importFrom tools Rd2ex Rd_db
extract_examples <- function(package_name,
                             package_directory,
                             libraries = NULL,
                             encoding = "UTF-8",
                             comment_dont_run = TRUE,
                             comment_dont_test = TRUE,
                             prepend_library_load = TRUE) {

    name <- if (missing(package_name)) {
                basename(package_directory)
            } else {
                package_name
            }

    create_result <- function(filenames, contents) {
        data.frame(package = name,
                   filename = filenames,
                   content = contents)
    }

    db <- tryCatch({
        Rd_db(package_name, package_directory, libraries)
    }, error = function(e) {
        print(e)
        return(create_result(character(0), character(0)))
    })

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
            library_load <- paste0("library", "(", name, ")")
            paste(library_load, code, sep = "\n")
        }

        else code
    }

    filenames <- names(db)
    contents <- sapply(filenames, example_extractor)

    na_locations <- is.na(contents)

    create_result(replace_extension(filenames[!na_locations], ".R"),
                  contents[!na_locations])
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


extract_package_examples <- function(pkg, pkg_dir, output_dir) {
    db <- tryCatch({
        tools::Rd_db(basename(pkg_dir), dir=dirname(pkg_dir))
    }, error=function(e) {
        c()
    })

    if (!length(db)) {
        return(character())
    }

    files <- names(db)

    examples <- sapply(files, function(x) {
        f <- file.path(output_dir, paste0(basename(x), ".R"))
        tools::Rd2ex(db[[x]], f, defines=NULL,
                     commentDontrun=TRUE, commentDonttest=TRUE)

        if (!file.exists(f)) {
            message("Rd file `", x, "' does not contain any code to be run")
            NA
        } else {
                                        # prepend the file with library call
            txt <- c(
                paste0("library(", pkg, ")"),
                "",
                "",
                readLines(f)
            )
            writeLines(txt, f)
            f
        }
    })

    na.omit(examples)
}
