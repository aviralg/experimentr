
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

#' @export
select_writer <- function(ext, ...) {

    writer_gen <- list(int = int_writer,
                       dbl = dbl_writer,
                       lgl = lgl_writer,
                       chr = chr_writer,
                       cpx = cpx_writer,
                       raw = raw_writer,
                       rds = rds_writer,
                       fst = fst_writer,
                       csv = csv_writer,
                       txt = txt_writer)[[ext]]

    if (is.null(writer_gen)) {
        msg <- sprintf("no writer available for extension %s", ext)
        stop(msg)
    }

    writer_gen(...)
}

#' @export
select_reader <- function(ext, ...) {

    reader_gen <- list(int = int_reader,
                       dbl = dbl_reader,
                       lgl = lgl_reader,
                       chr = chr_reader,
                       cpx = cpx_reader,
                       raw = raw_reader,
                       rds = rds_reader,
                       fst = fst_reader,
                       csv = csv_reader,
                       txt = txt_reader)[[ext]]

    if (is.null(reader_gen)) {
        msg <- sprintf("no reader available for extension %s", ext)
        stop(msg)
    }

    reader_gen(...)
}
