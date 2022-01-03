#' `httptest2`: A Test Environment for HTTP Requests
#'
#' The `httptest2` package lets you test R code that wraps an API without
#' requiring access to the remote service. It provides multiple test contexts
#' that mock the network connection in different ways. [with_mock_api()] lets
#' you provide custom fixtures as responses to requests, stored as plain-text
#' files in your test directory. [without_internet()] converts HTTP requests
#' into errors that print the request method, URL, and body payload, if
#' provided, allowing you to assert that a function call would make a
#' correctly-formed HTTP request or assert that a function does not make a
#' request (because if it did, it would raise an error in this context).
#'
#' The package also includes [capture_requests()], a context that collects the
#' responses from requests you make and stores them as mock files. This enables
#' you to perform a series of requests against a live server once and then build
#' your test suite using those mocks, running your tests in `with_mock_api()`.
#' For a context that combines both recording and mocking, use
#' [with_mock_dir()]: it records the first time it is run, and subsequent
#' runs use the mocks for offline testing.
#'
#' `httptest2` also includes expectations to assert that HTTP requests
#' were---or were not---made. [expect_GET()], [expect_PUT()], [expect_PATCH()],
#' [expect_POST()], and [expect_DELETE()] assert that the specified HTTP request
#' is made within one of the test contexts. They catch the error or message
#' raised by the mocked HTTP service and check that the request URL and optional
#' body match the expectation. [expect_no_request()] is the inverse of those: it
#' asserts that no error or message from a mocked HTTP service is raised.
#' [expect_request_header()] asserts that an HTTP request, mocked or not,
#' contains a request header.
#'
#' For an overview of testing with `httptest2`, see `vignette("httptest2")`.
#'
#' When recording requests, by default `httptest2` looks for and redacts the
#' standard ways that auth credentials are passed in requests. This prevents you
#' from accidentally publishing your personal tokens. The redacting behavior is
#' fully customizable, either by providing a `function (response) {...}` to
#' `set_redactor()`, or by placing a function in your package's
#' `inst/httptest2/redact.R` that will be used automatically any time you record
#' requests with your package loaded. See `vignette("redacting")` for details.
#'
#' `httptest2` also enables you to write package vignettes and other R Markdown
#' documents that communicate with a remote API. By adding as little as
#' [start_vignette()] to the beginning of your vignette, you can safely record
#' API responses from a live session, using your secret credentials. These API
#' responses are scrubbed of sensitive personal information and stored in a
#' subfolder in your `vignettes` directory. Subsequent vignette builds,
#' including on continuous-integration services, CRAN, and your package users'
#' computers, use these recorded responses, allowing the document to regenerate
#' without a network connection or API credentials. To record fresh API
#' responses, delete the subfolder of cached responses and re-run. See
#' `vignette("vignettes")` for more discussion and links to examples.
#'
#' @name httptest2
#' @docType package
#' @importFrom httr2 req_perform request
#' @keywords internal
NULL
