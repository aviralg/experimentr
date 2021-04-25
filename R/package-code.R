EXAMPLE <- "example"
VIGNETTE <- "vignette"
TESTTHAT <- "testthat"
TEST <- "test"

#' @export
#' @importFrom progress progress_bar
#' @importFrom fst write_fst
#' @importFrom stringr str_c str_trim
#' @importFrom dplyr mutate filter if_else select
extract_code <- function(packages,
                         type = c("example", "vignette", "testthat", "test"),
                         progress = TRUE,
                         index_filepath = NULL,
                         data_dirpath = NULL,
                         comment_dont_run = TRUE,
                         comment_dont_test = TRUE,
                         filter_empty = TRUE,
                         add_library_call = c("example", "vignette", "test")) {

    if (progress) {
        pb <- progress_bar$new(format = "Processing :what [:bar] :current/:total (:percent) eta: :eta",
                               total = length(packages),
                               width = 80)
    }

    helper  <- function(package, ...) {
        if (progress) pb$tick(tokens = list(what = pad(package, 15)))
        extract_code_helper(package, ...)
    }

    dfs <- lapply(packages,
                  helper,
                  type,
                  comment_dont_run,
                  comment_dont_test)

    result <- do.call(rbind, dfs)

    if (filter_empty) {
        result <- mutate(result, content = str_trim(content))
        result <- filter(result, content != "")
    }

    library_column <- str_c("library(", result$package, ")", sep = "")
    modified_content <- str_c(library_column, result$content, sep = "\n")

    prepend_library <- function(result, code_type) {
        mutate(result,
               content = if_else(type == code_type,
                                 modified_content,
                                 content))
    }

    for (type in add_library_call) {
        result <- prepend_library(result, type)
    }

    if (!is.null(index_filepath) && !is.null(result)) {
        write_fst(select(result, -content), index_filepath)
    }

    if (!is.null(data_dirpath)) {
        result <- write_code_result(result, data_dirpath)
    }

    result
}

#' @export
extract_examples <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type <- "example"
    eval(call, parent.frame())
}

#' @export
extract_vignettes <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type <- "vignette"
    eval(call, parent.frame())
}

#' @export
extract_testthats <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type <- "testthat"
    eval(call, parent.frame())
}

#' @export
extract_tests <- function(...) {
    call <- match.call()
    call[[1]] <- as.name("extract_code")
    call$type <- "test"
    eval(call, parent.frame())
}


#' @importFrom dplyr bind_rows
extract_code_helper <- function(package, type, ...) {

    result <- NULL

    helper <- function(fun) {
        bind_rows(result, fun(package))
    }

    if ("example" %in% type) {
        result <- helper(extract_examples_helper)
    }
    if ("testthat" %in% type) {
        result <- helper(extract_testthats_helper)
    }
    if ("test" %in% type) {
        result <- helper(extract_tests_helper)
    }
    if ("vignette" %in% type) {
        result <- helper(extract_vignettes_helper)
    }

    result
}


#' @importFrom tools Rd2ex Rd_db
extract_examples_helper <- function(package,
                                    comment_dont_run = TRUE,
                                    comment_dont_test = TRUE) {

    db <- tryCatch({
        Rd_db(package)
    }, error = function(e) {
        print(e)
        list()
    })

    result <- NULL

    if (length(db) != 0) {

        example_extractor <- function(example_filename) {
            elt <- db[[example_filename]]
            out <- textConnection("code", "w", local = TRUE)
            Rd2ex(elt,
                  out,
                  commentDontrun = comment_dont_run,
                  commentDonttest = comment_dont_test)
            close(out)

            paste(code, sep = "\n", collapse = "\n")
        }

        filenames <- names(db)
        contents <- sapply(filenames, example_extractor)

        result <- create_result(EXAMPLE,
                                package,
                                "",
                                filenames,
                                contents)
    }

    result
}

#' @importFrom tools pkgVignettes checkVignettes
#' @importFrom utils vignette
#' @importFrom readr read_file
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

    result <- NULL

    if (length(filepaths) != 0) {
        filenames <- basename(filepaths)

        contents <- sapply(filepaths, read_file)

        result <- create_result(VIGNETTE,
                                package,
                                "",
                                filenames,
                                contents)
    }

    result
}

## replace_extension(filenames[!na_indices], ".R"),


#' @importFrom fs dir_ls path_file file_exists
#' @importFrom purrr map_chr
#' @importFrom readr read_file
#' @importFrom tibble add_row
extract_testthats_helper <- function(package) {

    test_path <- system.file("tests", "testthat", package = package)

    result <- NULL

    if (test_path != "") {

        filepaths <- c(dir_ls(test_path, glob = "*.R"),
                       dir_ls(test_path, glob = "*.r"))

        filenames <- path_file(filepaths)

        contents <- map_chr(filepaths, read_file)

        ignore <- length(path_split(test_path)[[1]])

        subdirs <- map_chr(filepaths, function(x) {
            elts <- x[seq2(ignore + 1, length(x) - 1)]
            str_c(elts, collapse="/", sep="")
        })

        result <- create_result(TESTTHAT,
                                package,
                                subdirs,
                                filenames,
                                contents)

        add_testthat_file <- function(filename) {
            filepath <- path_join(c(test_path, filename))

            res <- result
            if (file_exists(filepath)) {
                 res <- add_row(result,
                                package = package,
                                type = TESTTHAT,
                                subdir = "",
                                filename = filename,
                                content = read_file(filepath))
            }
            res
        }

        result <- add_testthat_file("testthat.r")
        result <- add_testthat_file("testthat.R")
    }

    result
}

#' @importFrom fs dir_ls path_file
#' @importFrom purrr map_chr
#' @importFrom readr read_file
extract_tests_helper <- function(package) {
    tests_dirpath <- system.file("tests", package = package)
    testthat_dirpath <- system.file("tests", "testthat", package = package)

    result <- NULL

    if (tests_dirpath != "" && testthat_dirpath == "") {
        filepaths <- c(dir_ls(tests_dirpath, glob = "*.R"),
                       dir_ls(tests_dirpath, glob = "*.r"))

        filenames <- path_file(filepaths)

        contents <- map_chr(filepaths, read_file)

        ignore <- length(path_split(tests_dirpath)[[1]])

        subdirs <- map_chr(filepaths, function(x) {
            elts <- x[seq2(ignore + 1, length(x) - 1)]
            str_c(elts, collapse="/", sep="")
        })

        result <- create_result(TEST,
                                package,
                                subdirs,
                                filenames,
                                contents)

    }

    result
}

#' @importFrom fs path_join dir_create path_dir
#' @importFrom purrr map2_chr pmap_chr
#' @importFrom readr write_file
#' @export
write_code_result <- function(code, data_dirpath) {

    filepaths <- pmap_chr(list(data_dirpath, code$type, code$package, code$subdir, code$filename),
                          function(...) path_join(c(...)))

    map2_chr(code$content, filepaths, function(content, filepath) {
        dir_create(path_dir(filepath), recurse=TRUE)
        write_file(content, filepath)
    })

    code
}

#' @importFrom fs path_ext_set
#' @importFrom tibble tibble
create_result <- function(types, packages, subdirs, filenames, contents) {
    tibble(type = types,
           package = packages,
           subdir = subdirs,
           filename = path_ext_set(filenames, "R"),
           content = contents)
}
