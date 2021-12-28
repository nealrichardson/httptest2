#' Test that an HTTP request is made with a header
#'
#' This expectation checks that a HTTP header (and potentially header value)
#' is present in a request. It works by inspecting the request object and
#' raising warnings that are caught by [testthat::expect_warning()].
#'
#' `expect_request_header` works both in the mock HTTP contexts and on "live"
#' HTTP requests.
#'
#' @param expr Code to evaluate
#' @param ... Arguments passed to `expect_warning()`
#' @param ignore.case logical: if `FALSE`, the pattern matching is _case
#' sensitive_ and if `TRUE`, case is ignored during matching. Default is `TRUE`;
#' note that this is the opposite of `expect_warning` but is appropriate here
#' because HTTP header names are case insensitive.
#' @return The value of `expr` if there are no expectation failures
#' @importFrom testthat expect_null expect_match
#' @examplesIf !httptest2:::currently_offline()
#'
#' library(httr2)
#' expect_request_header(
#'   request("http://httpbin.org") %>%
#'     req_headers(Accept = "image/png") %>%
#'     req_perform(),
#'   accept = "image/png"
#' )
#' @export
expect_request_header <- function(expr, ..., ignore.case = TRUE) {
  # HTTR2: enumerate regex options since ... is for headers now
  expected <- list(...)
  # HTTR2: validate `expected`
  names(expected) <- tolower(names(expected))

  # Slightly different behavior based on whether we already are mocking
  current_mocker <- getOption("httr2_mock")
  header_mocker <- function(req) {
    actual <- req$headers
    names(actual) <- tolower(names(actual))
    for (n in names(expected)) {
      if (is.null(expected[[n]])) {
        expect_null(actual[[n]])
      } else {
        expect_match(
          actual[[n]],
          expected[[n]],
          label = sprintf('Header "%s"', n)
        )
      }
    }

    if (is.null(current_mocker)) {
      # If there is no other mocking, req_perform() will proceed normally
      NULL
    } else {
      current_mocker(req)
    }
  }

  httr2::with_mock(header_mocker, expr)
}

# HTTR2: report this to testthat
# f <- function(expr, ...) {
#   testthat::expect_warning(expr, ..., ignore.case = TRUE)
# }
# f("No warning", NA)

# Warning message:
# 1 components of `...` were not used.
#
# We detected these problematic arguments:
# * `ignore.case`
#
# Did you misspecify an argument?
