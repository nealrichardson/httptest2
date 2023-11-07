#' Remove sensitive content from HTTP responses
#'
#' When recording requests for use as test fixtures, you don't want to include
#' secrets like authentication tokens and personal ids. These functions provide
#' a means for redacting this kind of content, or anything you want, from
#' responses that [capture_requests()] saves.
#'
#' `redact_cookies()` removes cookies from `httr2_response` objects
#' and is the default redactor in `capture_requests()`.
#' `redact_headers()` lets you target selected request and response headers for
#' redaction.
#' `within_body_text()` lets you manipulate the text of the response body
#' and manages the parsing of the raw (binary) data in the `httr_response` object.
#'
#' Note that if you set a redacting function, it will also be applied to requests when loading mocks. This allows you to sanitize and/or shorten URLs in your mock files.
#' @param response An `httr2_response` or `httr2_request` object to sanitize.
#' @param headers For `redact_headers()`, a character vector of header names to
#' sanitize.
#' @param FUN For `within_body_text()`, a function that takes as its argument a
#' character vector and returns a modified version of that. This function will
#' be applied to the text of the response's body.
#' @return All redacting functions return a well-formed `httr2_response`
#' or `httr2_request` object.
#' @name redact
#' @aliases redact_cookies redact_headers within_body_text
#' @seealso `vignette("redacting", package = "httptest2")` for a detailed discussion of what these functions do and how to customize them. [gsub_response()] is another redactor.
#' @export
redact_cookies <- function(response) {
  redact_headers(response, "Set-Cookie")
}

#' @rdname redact
#' @export
redact_headers <- function(response, headers = c()) {
  header_apply(response, headers, function(x) "REDACTED")
}

header_apply <- function(response, headers, FUN, ...) {
  # Apply some function over a set of named headers
  todo <- tolower(names(response$headers)) %in% tolower(headers)
  response$headers[todo] <- lapply(response$headers[todo], FUN, ...)
  response
}

#' @rdname redact
#' @export
within_body_text <- function(response, FUN) {
  # This could be applied to a httr2_request object;
  # only do work on a httr2_response
  if (inherits(response, "httr2_response") && length(response$body)) {
    old <- resp_body_string(response)
    new <- FUN(old)
    response$body <- charToRaw(new)
  }
  return(response)
}

#' Find and replace within a response object
#'
#' This function passes its arguments to [base::gsub()] in order to find and
#' replace string patterns (regular expressions) within
#' the URL and the response body of `httr2_response` objects.
#'
#' Note that, unlike `gsub()`, the first argument of the function is `response`,
#' not `pattern`, while the equivalent argument in `gsub()`, "`x`", is placed
#' third. This difference is to maintain consistency with the other redactor
#' functions in `httptest2`, which all take `response` as the first argument.
#'
#' This function also can be applied to an `http2_request` object to replace
#' patterns inside the request URL.
#' @param response An `httr2_response` or `http2_request` object to sanitize.
#' @param pattern From [base::gsub()]: "character string containing a regular
#' expression (or character string for `fixed = TRUE`) to be matched in the
#' given character vector." Passed to `gsub()`. See the docs for `gsub()` for
#' further details.
#' @param replacement A replacement for the matched pattern, possibly including
#' regular expression backreferences. Passed to `gsub()`. See the docs for
#' `gsub()` for further details.
#' @param ... Additional logical arguments passed to `gsub()`: `ignore.case`,
#' `perl`, `fixed`, and `useBytes` are the possible options.
#' @return An `httr2_response` object, same as was passed in, with the
#' pattern replaced in the URLs and bodies.
#' @export
gsub_response <- function(response, pattern, replacement, ...) {
  replacer <- function(x) gsub(pattern, replacement, x, ...)
  response$url <- replacer(response$url)
  response <- header_apply(response, "location", replacer)
  response <- within_body_text(response, replacer)
  return(response)
}

#' Wrap a redacting expression as a proper function
#'
#' Redactors take a `httr2_response` or `httr2_request` as their first argument, and some take additional
#' arguments: `redact_headers()`, for example, requires that you specify
#' `headers`. This function allows you to take a simplified expression via a
#' formula, similar to what `purrr` does, so that you can provide the function
#' to `capture_requests()`.
#'
#' For example, `as.redactor(~ redact_headers(., "X-Custom-Header"))` is equivalent
#' to `function (response) redact_headers(response, "X-Custom-Header")`. This
#' allows you to do
#' `set_redactor(~ redact_headers(., "X-Custom-Header"))`.
#' @param fmla Partial expression (a `formula`) to turn into a function
#' @return A `function`.
#' @rdname as-redactor
#' @importFrom stats terms
#' @keywords internal
#' @seealso [capture_requests()]
as.redactor <- function(fmla) {
  env <- parent.frame()
  expr <- attr(terms(fmla), "variables")[[2]]
  # cf. magrittr:::wrap_function: wrap that in a function (.) ...
  return(eval(call("function", as.pairlist(alist(. = )), expr), env, env))
}

chain_redactors <- function(funs) {
  # Given a list of functions, return a function that execs them in sequence
  return(function(response) {
    for (f in funs) {
      if (inherits(f, "formula")) {
        f <- as.redactor(f)
      }
      response <- f(response)
    }
    return(response)
  })
}
