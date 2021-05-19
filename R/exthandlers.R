
#' @export
#' @importFrom readr write_lines
int_writer <- function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_lines
int_reader <-  function(...) {
    function(file) {
        as.integer(read_lines(file, ...))
    }
}

#' @export
#' @importFrom readr write_lines
dbl_writer <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_lines
dbl_reader <-  function(...) {
    function(file) {
        as.double(read_lines(file, ...))
    }
}

#' @export
#' @importFrom readr write_lines
lgl_writer <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_lines
lgl_reader <-  function(...) {
    function(file) {
        as.logical(read_lines(file, ...))
    }
}

#' @export
#' @importFrom readr write_lines
chr_writer <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_lines
chr_reader <-  function(...) {
    function(file) {
        as.character(read_lines(file, ...))
    }
}

#' @export
#' @importFrom readr write_lines
cpx_writer <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_lines
cpx_reader <-  function(...) {
    function(file) {
        as.complex(read_lines(file, ...))
    }
}

#' @export
#' @importFrom readr write_file
raw_writer <-  function(...) {
    function(content, file) {
        write_file(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_file_raw
raw_reader <-  function(...) {
    function(file) {
        read_file_raw(file, ...)
    }
}

#' @export
rds_writer <-  function(...) {
    function(content, file) {
        base::saveRDS(object = content, file = file, ...)
    }
}

#' @export
rds_reader <-  function(...) {
    function(file) {
        base::readRDS(file, ...)
    }
}

#' @export
#' @importFrom fst write_fst
fst_writer <-  function(...) {
    function(content, file) {
        fst::write_fst(content, file, ...)
    }
}

#' @export
#' @importFrom fst read_fst
fst_reader <-  function(...) {
    function(file) {
        fst::read_fst(file, ...)
    }
}

#' @export
#' @importFrom readr write_csv
csv_writer <-  function(...) {
    function(content, file) {
        readr::write_csv(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_csv cols
csv_reader <-  function(..., col_types = cols()) {
    function(file) {
        readr::read_csv(file, ..., col_types = col_types)
    }
}

#' @export
#' @importFrom readr write_file
txt_writer <-  function(...) {
    function(content, file) {
        readr::write_file(content, file, ...)
    }
}

#' @export
#' @importFrom readr read_file
txt_reader <-  function(...) {
    function(file) {
        read_file(file, ...)
    }
}

initialize_ext_handlers <- function() {
    set_reader_and_writer("int", int_reader(), int_writer())
    set_reader_and_writer("dbl", dbl_reader(), dbl_writer())
    set_reader_and_writer("lgl", lgl_reader(), lgl_writer())
    set_reader_and_writer("chr", chr_reader(), chr_writer())
    set_reader_and_writer("cpx", cpx_reader(), cpx_writer())
    set_reader_and_writer("raw", raw_reader(), raw_writer())
    set_reader_and_writer("rds", rds_reader(), rds_writer())
    set_reader_and_writer("fst", fst_reader(), fst_writer())
    set_reader_and_writer("csv", csv_reader(), csv_writer())
    set_reader_and_writer("txt", txt_reader(), txt_writer())
}

get_ext_handlers <- function(ext) {
    result <- .ext_handlers[[ext]]

    if (is.null(result)) {
        result <- new.env(hash = TRUE, parent = emptyenv())
        assign(ext, result, envir = .ext_handlers)
    }

    result
}

#' @export
get_reader <- function(ext) {

    handlers <- get_ext_handlers(ext)
    reader <- handlers$reader

    if (is.null(reader)) {
        msg <- sprintf("reader not available for extension %s", ext)
        stop(msg)
    }

    reader
}

#' @export
set_reader <- function(ext, reader) {

    handlers <- get_ext_handlers(ext)
    handlers$reader <- reader

    invisible(NULL)
}

#' @export
get_writer <- function(ext) {

    handlers <- get_ext_handlers(ext)
    writer <- handlers$writer

    if (is.null(writer)) {
        msg <- sprintf("writer not available for extension %s", ext)
        stop(msg)
    }

    writer
}

#' @export
set_writer <- function(ext, writer) {

    handlers <- get_ext_handlers(ext)
    handlers$writer <- writer

    invisible(NULL)
}

#' @export
set_reader_and_writer <- function(ext, reader, writer) {

    set_reader(ext, reader)
    set_writer(ext, writer)

    invisible(NULL)
}
