#' @importFrom tools Rd2ex Rd_db
#' @importFrom stringi stri_enc_toutf8
extract_examples_helper <- function(package,
                                    libraries = NULL,
                                    encoding = "UTF-8",
                                    comment_dont_run = TRUE,
                                    comment_dont_test = TRUE,
                                    prepend_library_load = TRUE) {

    db <- tryCatch({
        Rd_db(package, libraries)
    }, error = function(e) {
        print(e)
        list()
    })

    if(length(db) == 0) {
        return(create_result(package, character(0), character(0)))
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

        code <- stri_enc_toutf8(paste(code, sep = "\n", collapse = "\n"))

        if(code == "") NA_character_

        else if(prepend_library_load) {
            library_load <- paste0("library", "(", package, ")")
            paste(library_load, code, sep = "\n")
        }

        else code
    }

    filenames <- names(db)
    contents <- sapply(filenames, example_extractor)

    na_indices <- is.na(contents)

    create_result(package,
                  replace_extension(filenames[!na_indices], ".R"),
                  contents[!na_indices])

}

#' @importFrom tools pkgVignettes checkVignettes
#' @importFrom utils vignette
#' @importFrom stringi stri_enc_toutf8
extract_vignettes_helper <- function(package) {

    vignettes <- tryCatch({

        vignettes <- pkgVignettes(package, source=TRUE)

        ## if there are vignettes and there are no vignette sources,
        ## then compile the vignettes to sources. This compilation
        ## will result in .R files in the doc directory of package
        ## and will be picked up by the next step of the program
        if (length(vignettes$docs) != 0 &&
            length(vignettes$sources) == 0) {

            tools::checkVignettes(package, tangle=TRUE, weave=FALSE, workdir="src")
        }

        pkgVignettes(package, source=TRUE)
    },
    error = function(e) {
        print(e)
        list(sources = list())
    })

    ## unlist is needed because one source can generate
    ## more than one R file.
    filepaths <- as.character(unlist(vignettes$sources))

    if(length(filepaths) == 0) {
        return(create_result(package, character(0), character(0)))
    }

    filenames <- basename(filepaths)

    vignette_reader <- function(filepath) {
        code <- stri_enc_toutf8(readChar(filepath, file.info(filepath)$size))

        if(code == "") NA_character_
        else code
    }

    contents <- sapply(filepaths, vignette_reader)
    na_indices <- is.na(contents)

    create_result(package,
                  replace_extension(filenames[!na_indices], ".R"),
                  contents[!na_indices])
}

extract_code_helper <- function(package, type, ...) {

    result <- NULL

    helper <- function(type, fun) {
        res <- fun(package)
        if(nrow(res) != 0) {
            res$type <- type
            result <<- rbind(result, res)
        }
    }

    if("examples" %in% type) {
        helper("example", extract_examples_helper)
    }
    if("tests" %in% type) {
        helper("test", extract_tests_helper)
    }
    if("vignettes" %in% type) {
        helper("vignette", extract_vignettes_helper)
    }
    result
}

#' @export
#' @importFrom progress progress_bar
#' @importFrom fst write.fst
extract_code <- function(packages,
                         type = c("examples", "vignettes", "tests"),
                         progress = TRUE,
                         index_filepath = NULL,
                         data_dirpath = NULL,
                         libraries = NULL,
                         encoding = "UTF-8",
                         comment_dont_run = TRUE,
                         comment_dont_test = TRUE,
                         prepend_library_load = TRUE) {

    if(progress) {
        pb <- progress_bar$new(format = "Processing :what [:bar] :current/:total (:percent) eta: :eta",
                               total = length(packages),
                               width = 80)
    }

    helper  <- function(package, ...) {
        if(progress) pb$tick(tokens = list(what = pad(package, 15)))
        extract_code_helper(package, ...)
    }

    dfs <- lapply(packages,
                  helper,
                  type,
                  libraries,
                  encoding,
                  comment_dont_run,
                  comment_dont_test,
                  prepend_library_load)

    result <- do.call(rbind, dfs)

    if(!is.null(index_filepath)) {
        write.fst(result, index_filepath)
    }

    if(!is.null(data_dirpath)) {
        result <- write_code_result(result, data_dirpath)
    }

    result
}

#' @export
extract_examples <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type = "examples"
    eval(call, parent.frame())
}

#' @export
extract_vignettes <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type = "vignettes"
    eval(call, parent.frame())
}

#' @export
extract_tests <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type = "tests"
    eval(call, parent.frame())
}

#' @export
write_code_result <- function(code, data_dirpath) {

    if(nrow(code) == 0) {
        code$filepath <- character(0)
        return(code)
    }

    example_writer <- function(row) {
        package <- code[row, "package"]
        type <- code[row, "type"]
        filename <- code[row, "filename"]
        content <- code[row, "content"]
        filepath <- file.path(data_dirpath, package, type, filename)
        dir.create(dirname(filepath), showWarnings=FALSE, recursive=TRUE)
        cat(content, file = filepath, append = FALSE)
        filepath
    }

    dir.create(data_dirpath, showWarnings = FALSE)

    code$filepath <- sapply(1:nrow(code), example_writer)

    code
}

create_result <- function(package, filenames, contents) {
    data.frame(package = if(length(filenames) == 0) character(0) else package,
               filename = filenames,
               content = contents)
}
