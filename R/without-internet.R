#' Make all HTTP requests raise an error
#'
#' `without_internet()` simulates the situation when any network request will
#' fail, as in when you are without an internet connection. Any HTTP request
#' through `httr2` will raise an error.
#'
#' `block_requests()` and `stop_mocking()` allow you to turn on/off request
#' blocking for more convenient use in an interactive session.
#'
#' The error message raised has a well-defined shape, made of three
#' elements, separated by space: (1) the request
#' method (e.g. "GET"); (2) the request URL; and
#' (3) the request body, if present. The verb-expectation functions,
#' such as [expect_GET()] and [expect_POST()], look for this shape.
#' @param expr Code to run inside the mock context
#' @return The result of `expr`
#' @examples
#' library(httr2)
#' library(testthat, warn.conflicts = FALSE)
#' without_internet({
#'   expect_error(
#'     request("http://httpbin.org/get") %>% req_perform(),
#'     "GET http://httpbin.org/get"
#'   )
#'   expect_error(
#'     request("http://httpbin.org/put") %>%
#'       req_method("PUT") %>%
#'       req_body_json(list(a = 1)) %>%
#'       req_perform(),
#'     'PUT http://httpbin.org/put {"a":1}',
#'     fixed = TRUE
#'   )
#' })
#' @export
without_internet <- function(expr) with_mocked_responses(stop_request, expr)

#' @rdname without_internet
#' @export
block_requests <- function() options(httr2_mock = stop_request)

#' @importFrom rlang abort cnd_header cnd_body cnd_footer
stop_request <- function(req) {
  out <- paste(get_request_method(req), req$url)
  body <- get_string_request_body(req)
  if (!is.null(body)) {
    # Max print option for debugging large payloads
    body <- substr(body, 1, getOption("httptest2.max.print", nchar(body)))
    out <- paste(out, body)
  }

  # Raise an error with a special class attached so we can distinguish it
  abort(out, mockfile = req$mockfile, class = "httptest2_request")
}

#' @export
cnd_header.httptest2_request <- function(cnd, ...) {
  "An unexpected request was made:"
}

#' @export
cnd_body.httptest2_request <- function(cnd, ...) {
  cnd$message
}

#' @export
cnd_footer.httptest2_request <- function(cnd, ...) {
  if (is.null(cnd$mockfile)) {
    character(0)
  } else {
    # Poked in here by mock_request for ease of debugging
    paste0("Expected mock file: ", cnd$mockfile, ".*")
  }
}
