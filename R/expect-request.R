#' Expectations for mocked HTTP requests
#'
#' The mock contexts in `httptest2` can raise errors or messages when requests
#' are made, and those (error) messages have three
#' elements, separated by space: (1) the request
#' method (e.g. "GET"); (2) the request URL; and
#' (3) the request body, if present.
#' These verb-expectation functions look for this message shape. `expect_PUT`,
#' for instance, looks for a request message that starts with "PUT".
#'
#' @param object Code to execute that may cause an HTTP request
#' @param url character: the URL you expect a request to be made to. Default is
#' an empty string, meaning that you can just assert that a request is made with
#' a certain method without asserting anything further.
#' @param ... character segments of a request payload you expect to be included
#' in the request body, to be joined together by `paste0()`. You may also pass
#' any of the following named logical arguments, which will be passed to
#' [base::grepl()]:
#' * `fixed`: Should matching take the pattern as is or treat it as a regular
#' expression. Default: `TRUE`, and note that this default is the opposite of
#' the default in `grepl`. (The rest of the arguments follow its defaults.)
#' * `ignore.case`: Should matching be done case insensitively? Default:
#' `FALSE`, meaning matches are case sensitive.
#' * `perl`: Should Perl-compatible regular expressions be used? Default: `FALSE`
#' * `useBytes`: Should matching be done byte-by-byte rather than
#' character-by-character? Default: `FALSE`
#' @return A `testthat` 'expectation'.
#' @examples
#' library(httr2)
#' without_internet({
#'   expect_GET(
#'     request("http://httpbin.org/get") %>% req_perform(),
#'     "http://httpbin.org/get"
#'   )
#'   expect_GET(
#'     request("http://httpbin.org/get") %>% req_perform(),
#'     "http://httpbin.org/[a-z]+",
#'     fixed = FALSE # For regular expression matching
#'   )
#'   expect_PUT(
#'     request("http://httpbin.org/put") %>%
#'       req_method("PUT") %>%
#'       req_body_json(list(a = 1)) %>%
#'       req_perform(),
#'     "http://httpbin.org/put",
#'     '{"a":1}'
#'   )
#'   # Don't need to assert the request body, or even the URL
#'   expect_PUT(
#'     request("http://httpbin.org/put") %>%
#'       req_method("PUT") %>%
#'       req_body_json(list(a = 1)) %>%
#'       req_perform()
#'   )
#'   expect_no_request(rnorm(5))
#' })
#' @name expect_verb
#' @aliases expect_GET expect_POST expect_PUT expect_PATCH expect_DELETE expect_no_request
#' @export
expect_GET <- function(object, url = "", ...) {
  expect_request(object, "GET ", url, " ", ...)
}

#' @rdname expect_verb
#' @export
expect_POST <- function(object, url = "", ...) {
  expect_request(object, "POST ", url, " ", ...)
}

#' @rdname expect_verb
#' @export
expect_PATCH <- function(object, url = "", ...) {
  expect_request(object, "PATCH ", url, " ", ...)
}

#' @rdname expect_verb
#' @export
expect_PUT <- function(object, url = "", ...) {
  expect_request(object, "PUT ", url, " ", ...)
}

#' @rdname expect_verb
#' @export
expect_DELETE <- function(object, url = "", ...) {
  expect_request(object, "DELETE ", url, " ", ...)
}

#' @rdname expect_verb
#' @export
expect_no_request <- function(object, ...) {
  # No request means no httptest2_request error raised
  expect_error(object, NA, ..., class = "httptest2_request")
}

#' @importFrom testthat expect_error fail
expect_request <- function(
  object,
  ...,
  fixed = TRUE,
  ignore.case = FALSE,
  perl = FALSE,
  useBytes = FALSE
) {
  # PUT/POST/PATCH with no body may have trailing whitespace, so trim it
  expected <- sub(" +$", "", paste0(...))
  withCallingHandlers(
    expect_error(
      object,
      expected,
      class = "httptest2_request",
      fixed = fixed,
      ignore.case = ignore.case,
      perl = perl,
      useBytes = useBytes
    ),
    error = function(e) {
      # Distinguish between unexpected requests and real errors
      if (inherits(e, "httptest2_request")) {
        fail(paste0(
          "An unexpected request was made:\n  Actual:   ",
          e$message,
          "\n  Expected: ",
          expected
        ))
      } else if (
        inherits(e, "expectation_failure") && grepl("did not throw", e$message)
      ) {
        # This is what expect_error() says (under testthat 3e) when there is no error
        fail("No request was made")
      } else {
        # A regular error. Re-raise it.
        stop(e)
      }
    }
  )
}
