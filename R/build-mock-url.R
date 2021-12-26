#' Convert a request to a mock file path
#'
#' Requests are translated to mock file paths according to several rules that
#' incorporate the request method, URL, query parameters, and body.
#'
#' First, the request protocol, such as "https://", is removed from the URL.
#' Second, if the request URL contains a query string, it will be popped off,
#' hashed by [digest::digest()], and the first six characters appended to the
#' file being read. Third, request bodies are similarly hashed and
#' appended. Finally, if a request method other than GET is used it will be
#' appended to the end of the end of the file name.
#'
#' Mock file paths also have a file extension appended, based on the
#' `Content-Type` of the response, though this function, which is only concerned
#' with the request, does not add the extension. In an
#' HTTP API, a "directory" itself is a resource,
#' so the extension allows distinguishing directories and files in the file
#' system. That is, a mocked `GET http://example.com/api/` may read a
#' "example.com/api.json" file, while
#' `GET http://example.com/api/object1/` reads "example.com/api/object1.json".
#'
#' Other examples:
#' * `GET http://example.com/api/object1/?a=1` may read
#' "example.com/api/object1-b64371.xml".
#' * `POST http://example.com/api/object1/?a=1` may read
#' "example.com/api/object1-b64371-POST.json".
#'
#' This function is exported so that other packages can construct similar mock
#' behaviors or override specific requests at a higher level than
#' `with_mock_api` mocks.
#'
#' Note that if you are trying to guess the mock file paths corresponding to a
#' test for which you intend to create a mock file manually,
#' instead of trying to build the URL, you should run the test
#' with `with_mock_api` as the error message will contain the mock file path.
#'
#' @param req A `request` object
#' @return A file path and name, without an extension. The file, or a file with
#' some extension appended, may or may not
#' exist: existence is not a concern of this function.
#' @importFrom digest digest
#' @seealso [with_mock_api()] [capture_requests()]
#' @export
build_mock_url <- function(req) {
  method <- get_request_method(req)
  body <- get_string_request_body(req)

  # Remove protocol
  url <- sub("^.*?://", "", req$url)
  # Handle query params
  parts <- unlist(strsplit(url, "?", fixed = TRUE))
  # Remove trailing slash
  f <- sub("\\/$", "", parts[1])
  # Sanitize the path to be portable for all R platforms
  f <- gsub(":", "-", f)
  if (length(parts) > 1) {
    # There's a query string. Append the digest as a suffix.
    f <- paste0(f, "-", hash(parts[2]))
  }

  # Handle body and append its hash if present
  if (!is.null(body)) {
    f <- paste0(f, "-", hash(body))
  }

  if (method != "GET") {
    # Append method to the file name for non GET requests
    f <- paste0(f, "-", method)
  }
  return(f)
}

get_string_request_body <- function(req) {
  # Returns a string if the request has a body, NULL otherwise
  body_apply <- utils::getFromNamespace("req_body_apply", "httr2")
  req <- body_apply(req)

  b <- request_postfields(req)
  if (is.null(b)) {
    if (length(req$fields)) {
      b <- lapply(req$fields, function(x) {
        if (inherits(x, "form_file")) {
          paste0('curl::form_file("', x$path, '")')
        } else {
          # assume form_data
          paste0('curl::form_data("', rawToChar(x$value), '")')
        }
      })
      b <- paste0(
        "list(",
        paste(names(b), b, sep = " = ", collapse = ", "),
        ")"
      )
    } else if (inherits(req$body$data, "httr_path")) {
      # File upload
      b <- paste0('req_body_file("', req$body$data, '")')
    }
  }
  b
}

request_postfields <- function(req) {
  b <- req[["options"]][["postfields"]]
  if (length(b) > 0) {
    # Check length this way because b may be NULL or length 0 raw vector
    return(rawToChar(b))
  } else {
    return(NULL)
  }
}

get_request_method <- function(req) {
  # At the time that we process the request, some defaults may not have been
  # applied, and the request method may be NULL
  # TODO(httr2): report upstream?
  req$method %||% ifelse(is.null(req$body), "GET", "POST")
}

hash <- function(string, n = 6) substr(digest(string), 1, n)

`%||%` <- function(a, b) if (is.null(a)) b else a