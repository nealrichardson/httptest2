#' Test that an HTTP request is made with a header
#'
#' This expectation checks that HTTP headers (and potentially header values)
#' are present in a request. It works both in the mock HTTP contexts and on
#' "live" HTTP requests.
#'
#' @param expr Code to evaluate
#' @param ... Named headers to match. Values should either be a string (length-1
#' character), which will be passed to [testthat::expect_match()], or `NULL` to
#' assert that the named header is not present in the request. To assert that a
#' header is merely present in the request, without asserting anything about its
#' contents, provide an empty string (`""`). Header names are always
#' case-insensitive; header values will be matched using the following
#' parameters:
#' @inheritParams base::grepl
#' @return The value of `expr` if there are no expectation failures
#' @importFrom testthat expect_null expect_match
#' @importFrom rlang is_string
#' @examplesIf FALSE
#'
#' library(httr2)
#' expect_request_header(
#'   request("http://httpbin.org") %>%
#'     req_headers(Accept = "image/png") %>%
#'     req_perform(),
#'   accept = "image/png",
#'   `x-fake-header` = NULL
#' )
#' expect_request_header(
#'   request("http://httpbin.org") %>%
#'     req_headers(Accept = "image/png") %>%
#'     req_perform(),
#'   accept = ""
#' )
#' @export
expect_request_header <- function(expr,
                                  ...,
                                  fixed = FALSE,
                                  ignore.case = FALSE,
                                  perl = FALSE,
                                  useBytes = FALSE) {
  expected <- list(...)
  if (length(expected) == 0) {
    stop("No headers provided")
  }
  if (is.null(names(expected)) || !all(nzchar(names(expected)))) {
    stop("Header values must be named")
  }
  names(expected) <- tolower(names(expected))

  # Slightly different behavior based on whether we already are mocking
  current_mocker <- getOption("httr2_mock")
  header_mocker <- function(req) {
    actual <- req$headers
    names(actual) <- tolower(names(actual))
    for (n in names(expected)) {
      if (is.null(expected[[n]])) {
        expect_null(
          actual[[n]],
          label = sprintf('Header "%s"', n)
        )
      } else if (is_string(expected[[n]])) {
        expect_match(
          actual[[n]],
          expected[[n]],
          fixed = fixed,
          ignore.case = ignore.case,
          perl = perl,
          useBytes = useBytes,
          label = sprintf('Header "%s"', n)
        )
      } else {
        stop("Expected headers must be strings (length 1)", call. = FALSE)
      }
    }

    if (is.null(current_mocker)) {
      # If there is no other mocking, req_perform() will proceed normally
      NULL
    } else {
      current_mocker(req)
    }
  }

  with_mock(header_mocker, expr)
}

with_mock <- function(mock, code) {
  if (httr2_1.0.0) {
    utils::getFromNamespace("with_mocked_responses", "httr2")(mock, code)
  } else {
    httr2::with_mock(mock, code)
  }
}
