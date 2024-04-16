#' Record API responses as mock files
#'
#' `capture_requests()` is a context that collects the responses from requests
#' you make and stores them as mock files. This enables you to perform a series
#' of requests against a live server once and then build your test suite using
#' those mocks, running your tests in [with_mock_api()].
#'
#' `start_capturing()` and `stop_capturing()` allow you to turn on/off request
#' recording for more convenient use in an interactive session.
#'
#' Recorded responses are written out as plain-text files. By storing fixtures
#' as plain-text files, you can
#' more easily confirm that your mocks look correct, and you can more easily
#' maintain them without having to re-record them. If the API changes subtly,
#' such as when adding an additional attribute to an object, you can just touch
#' up the mocks.
#'
#' If the response has status `200 OK` and the `Content-Type`
#' maps to a supported file extension---currently `.json`,
#' `.html`, `.xml`, `.txt`, `.csv`, and `.tsv`---just the response body will be
#' written out, using the appropriate extension. `204 No Content` status
#' responses will be stored as an empty file with extension `.204`. Otherwise,
#' the response will be written as a `.R` file containing syntax that, when
#' executed, recreates the `httr2_response` object.
#'
#' Files are saved to the first directory in [.mockPaths()], which if not
#' otherwise specified is either "tests/testthat" if it exists
#' (as it should if you are in the root directory of your package),
#' else the current working directory.
#' If you have trouble when recording responses, or are unsure where the files
#' are being written, set `options(httptest2.verbose = TRUE)` to print a message
#' for every file that is written containing the absolute path of the file.
#'
#' @param expr Code to run inside the context
#' @param simplify logical: if `TRUE` (default), plain-text responses with status 200
#' will be written as just the text of the response body. In all other cases,
#' and when `simplify` is `FALSE`, the `httr2_response` object will be written out to
#' a .R file using [base::dput()].
#' @return `capture_requests()` returns the result of `expr`. `start_capturing()`
#' invisibly returns the destination directory.
#' `stop_capturing()` returns nothing; it is called for its side effects.
#' @examplesIf FALSE
#' # Setup so that our examples clean up after themselves
#' tmp <- tempfile()
#' .mockPaths(tmp)
#' on.exit(unlink(tmp, recursive = TRUE))
#'
#' library(httr2)
#' capture_requests({
#'   request("http://httpbin.org/get") %>% req_perform()
#'   request("http://httpbin.org/response-headers") %>%
#'     req_headers(`Content-Type` = "application/json") %>%
#'     req_perform()
#' })
#' # Or:
#' start_capturing()
#' request("http://httpbin.org/get") %>% req_perform()
#' request("http://httpbin.org/response-headers") %>%
#'   req_headers(`Content-Type` = "application/json") %>%
#'   req_perform()
#' stop_capturing()
#' @export
#' @seealso [build_mock_url()] for how requests are translated to file paths.
#' And see `vignette("redacting", package = "httptest2")`
#' for details on how to prune sensitive content from responses when recording.
capture_requests <- function(expr, simplify = TRUE) {
  start_capturing(simplify)
  on.exit(stop_capturing())
  eval.parent(expr)
}

#' @rdname capture_requests
#' @export
start_capturing <- function(simplify = TRUE) {
  # Use "substitute" so that `simplify` is inserted. Code remains quoted.
  req_tracer <- substitute(
    {
      # Get the value returned from the function, and sanitize it
      redactor <- httptest2::get_current_redactor()
      if (exists("mock_resp") && !is.null(mock_resp)) {
        # We're mocking and returning early
        resp <- mock_resp
      }
      if (!exists("resp") || inherits(resp, "error")) {
        # req_perform() catches request errors as an "error" object
        # See httr2:::is_error()
        # Also possible that resp doesn't exist if the function errors eariler
        warning(
          "Request errored; no captured response file saved",
          call. = FALSE
        )
      } else {
        httptest2::save_response(
          redactor(resp),
          file = httptest2::build_mock_url(redactor(req)),
          simplify = simplify
        )
      }
    },
    list(simplify = simplify)
  )
  trace_httr2("req_perform", exit = req_tracer)
  invisible(.mockPaths())
}

#' Write out a captured response
#'
#' @param response An `httr2_response` object
#' @param file String file path to write to
#' @param simplify logical: if `TRUE` (default), JSON responses with status 200
#' and a supported `Content-Type`
#' will be written as just the text of the response body. In all other cases,
#' and when `simplify` is `FALSE`, the `httr2_response` object will be written
#' out to a .R file using [base::dput()].
#' @return The character file name that was written out
#' @export
#' @keywords internal
#' @importFrom jsonlite prettify
#' @importFrom httr2 resp_content_type resp_status resp_body_string
save_response <- function(response, file, simplify = TRUE) {
  # Track separately the actual full path we're going to write to
  dst_file <- file.path(.mockPaths()[1], file)
  mkdir_p(dst_file)

  # Get the Content-Type
  ct <- resp_content_type(response)
  status <- resp_status(response)
  if (simplify && status == 200 && ct %in% names(CONTENT_TYPE_TO_EXT)) {
    if (length(response$body)) {
      cont <- resp_body_string(response)
      if (ct == "application/json") {
        # Prettify
        cont <- prettify(cont)
      }
    } else {
      cont <- character()
    }
    dst_file <- paste(dst_file, CONTENT_TYPE_TO_EXT[[ct]], sep = ".")
    cat_wb(cont, file = dst_file)
  } else if (simplify && status == 204) {
    # "touch" a file with extension .204
    dst_file <- paste0(dst_file, ".204")
    cat_wb("", file = dst_file)
  } else {
    # Dump an object that can be sourced

    # Change the file extension to .R
    dst_file <- paste0(dst_file, ".R")
    file <- paste0(file, ".R")

    # If content is text, rawToChar it and dput it as charToRaw(that)
    # so that it loads correctly but is also readable
    text_types <- c(
      "application/json",
      "application/x-www-form-urlencoded", "application/xml",
      "text/csv", "text/html", "text/plain",
      "text/tab-separated-values", "text/xml"
    )
    if (is.raw(response$body) && ct %in% text_types && length(response$body)) {
      cont <- resp_body_string(response)
      response$body <- substitute(charToRaw(cont))
    } else if (inherits(response$body, "httr2_path")) {
      # Copy real file and substitute the response$content "path".
      downloaded_file <- paste0(dst_file, "-FILE")
      file.copy(response$body, downloaded_file)
      file <- paste0(file, "-FILE")
      response$body <- substitute(structure(find_mock_file(file),
        class = "httr2_path"
      ))
    }
    # Needed for httr2 1.0.0
    response$cache <- quote(new.env(parent = emptyenv()))
    response$request <- NULL

    f <- file(dst_file, "wb", encoding = "UTF-8")
    on.exit(close(f))
    dput(response, file = f)
  }
  verbose_message("Writing ", normalizePath(dst_file))
  return(dst_file)
}

#' @rdname capture_requests
#' @export
stop_capturing <- function() {
  untrace_httr2("req_perform")
  invisible()
}

mkdir_p <- function(filename) {
  # Recursively create the directories so that we can write this file.
  # If they already exist, do nothing.
  # Like mkdir -p path

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
}

cat_wb <- function(x, file, ...) {
  # For cleaning up CRLF issues on Windows, write to a file connection in binary mode
  f <- file(file, "wb", encoding = "UTF-8")
  on.exit(close(f))
  cat(enc2utf8(x), file = f, ...)
}

verbose_message <- function(...) {
  if (isTRUE(getOption("httptest2.verbose", FALSE))) {
    message(...)
  }
}
