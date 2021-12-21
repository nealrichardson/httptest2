#' Test that an HTTP request is made with a header
#'
#' This expectation checks that a HTTP header (and potentially header value)
#' is present in a request. It works by inspecting the request object and
#' raising warnings that are caught by [testthat::expect_warning()].
#'
#' `expect_header` works both in the mock HTTP contexts and on "live" HTTP
#' requests.
#'
#' @param expr Code to evaluate
#' @param ... Arguments passed to `expect_warning()`
#' @param ignore.case logical: if `FALSE`, the pattern matching is _case
#' sensitive_ and if `TRUE`, case is ignored during matching. Default is `TRUE`;
#' note that this is the opposite of `expect_warning` but is appropriate here
#' because HTTP header names are case insensitive.
#' @return `NULL`, according to `expect_warning`.
#' @importFrom testthat expect_warning
#' @examplesIf !httptest2:::currently_offline()
#'
#' library(httr2)
#' expect_header(
#'   request("http://httpbin.org") %>%
#'     req_headers(Accept = "image/png") %>%
#'     req_perform(),
#'   "Accept: image/png"
#' )
#' @export
expect_header <- function(expr, ..., ignore.case = TRUE) {
  current_mocker <- getOption("httr2_mock")
  warn_headers <- function(req) {
    heads <- req$headers
    msgs <- lapply(names(heads), function(h) paste(h, heads[h], sep = ": "))
    warning(msgs, call. = FALSE)
    NULL
  }

  # Slightly different behavior based on whether we already are mocking
  header_mocker <- function(req) {
    warn_headers(req)
    # warn_headers() returns NULL, so if there is no other mocking,
    # req_perform() will proceed normally
    if (!is.null(current_mocker)) {
      current_mocker(req)
    }
  }

  # TODO: can we do something different than warn?
  expect_warning(
    httr2::with_mock(header_mocker, expr),
    ...,
    ignore.case = ignore.case
  )
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
