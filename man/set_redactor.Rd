% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redactor.R
\name{set_redactor}
\alias{set_redactor}
\title{Set a response redactor}
\usage{
set_redactor(FUN)
}
\arguments{
\item{FUN}{A function or expression that modifies \code{httr2_response} objects.
Specifically, a valid input is one of:
\itemize{
\item A function taking a single argument, the \code{httr2_response},
and returning a valid \code{httr2_response} object.
\item A formula as shorthand for an anonymous function with \code{.} as the
"response" argument, as in the \code{purrr} package. That is, instead of
\code{function (response) redact_headers(response, "X-Custom-Header")}, you can
use \code{~ redact_headers(., "X-Custom-Header")}
\item A list of redacting functions/formulas, which will be executed
in sequence on the response
\item \code{NULL}, to override the default \code{redact_cookies()}.
}}
}
\value{
Invisibly, the redacting function, validated and perhaps modified.
Formulas and function lists are turned into proper functions. \code{NULL} as input
returns the \code{force()} function.
}
\description{
A redactor is a function that alters the response content being written
out in the \code{\link[=capture_requests]{capture_requests()}} context, allowing you to remove sensitive
values, such as authentication tokens,
as well as any other modification or truncation of the response body. By
default, the \code{\link[=redact_cookies]{redact_cookies()}} function will be used to purge standard
auth methods, but \code{set_redactor()} allows you to provide a different one.
}
\details{
Alternatively, you can put a redacting function in \code{inst/httptest2/redact.R}
in your package, and
any time your package is loaded (as in when running tests or building
vignettes), the function will be used automatically.
}
\examples{
# Shorten UUIDs in response body/URLs to their first 6 digits:
set_redactor(function(resp) gsub_response(resp, "([0-9a-f]{6})[0-9a-f]{26}", "\\\\1"))
# Restore the default
set_redactor(redact_cookies)
}
\seealso{
For further details on how to redact responses, see \code{vignette("redacting", package = "httptest2")}.
}
