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
#' Note that if you are trying to guess the mock file paths corresponding to a
#' test for which you intend to create a mock file manually,
#' instead of trying to build the URL, you should run the test
#' with `with_mock_api()` as the error message will contain the mock file path.
#'
#' @param req A `httr2_request` object
#' @return A file path and name, without an extension. The file, or a file with
#' some extension appended, may or may not
#' exist: existence is not a concern of this function.
#' @importFrom digest digest
#' @seealso [with_mock_api()] [capture_requests()]
#' @export
#' @keywords internal
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
          # hash the file contents
          paste("File:", digest(x$path, serialize = FALSE, file = TRUE))
        } else {
          # assume form_data
          rawToChar(x$value)
        }
      })
      b <- paste(c(
        "Multipart form:",
        paste(names(b), b, sep = " = ")
      ), collapse = "\n  ")
      # add a newline at the end too
      b <- paste0(b, "\n")
    } else if (inherits(req$body$data, c("httr2_path", "httr_path"))) {
      # As of httr2 0.1.1, the class is called httr_path, but future-proof in
      # case of future standardization
      # https://github.com/r-lib/httr2/issues/99
      b <- paste("File:", digest(req$body$data, serialize = FALSE, file = TRUE))
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
  utils::getFromNamespace("req_method_get", "httr2")(req)
}

hash <- function(string, n = 6) substr(digest(string), 1, n)
