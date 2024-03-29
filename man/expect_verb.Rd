% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expect-request.R
\name{expect_verb}
\alias{expect_verb}
\alias{expect_GET}
\alias{expect_POST}
\alias{expect_PUT}
\alias{expect_PATCH}
\alias{expect_DELETE}
\alias{expect_no_request}
\title{Expectations for mocked HTTP requests}
\usage{
expect_GET(object, url = "", ...)

expect_POST(object, url = "", ...)

expect_PATCH(object, url = "", ...)

expect_PUT(object, url = "", ...)

expect_DELETE(object, url = "", ...)

expect_no_request(object, ...)
}
\arguments{
\item{object}{Code to execute that may cause an HTTP request}

\item{url}{character: the URL you expect a request to be made to. Default is
an empty string, meaning that you can just assert that a request is made with
a certain method without asserting anything further.}

\item{...}{character segments of a request payload you expect to be included
in the request body, to be joined together by \code{paste0()}. You may also pass
any of the following named logical arguments, which will be passed to
\code{\link[base:grep]{base::grepl()}}:
\itemize{
\item \code{fixed}: Should matching take the pattern as is or treat it as a regular
expression. Default: \code{TRUE}, and note that this default is the opposite of
the default in \code{grepl}. (The rest of the arguments follow its defaults.)
\item \code{ignore.case}: Should matching be done case insensitively? Default:
\code{FALSE}, meaning matches are case sensitive.
\item \code{perl}: Should Perl-compatible regular expressions be used? Default: \code{FALSE}
\item \code{useBytes}: Should matching be done byte-by-byte rather than
character-by-character? Default: \code{FALSE}
}}
}
\value{
A \code{testthat} 'expectation'.
}
\description{
The mock contexts in \code{httptest2} can raise errors or messages when requests
are made, and those (error) messages have three
elements, separated by space: (1) the request
method (e.g. "GET"); (2) the request URL; and
(3) the request body, if present.
These verb-expectation functions look for this message shape. \code{expect_PUT},
for instance, looks for a request message that starts with "PUT".
}
\examples{
library(httr2)
without_internet({
  expect_GET(
    request("http://httpbin.org/get") \%>\% req_perform(),
    "http://httpbin.org/get"
  )
  expect_GET(
    request("http://httpbin.org/get") \%>\% req_perform(),
    "http://httpbin.org/[a-z]+",
    fixed = FALSE # For regular expression matching
  )
  expect_PUT(
    request("http://httpbin.org/put") \%>\%
      req_method("PUT") \%>\%
      req_body_json(list(a = 1)) \%>\%
      req_perform(),
    "http://httpbin.org/put",
    '{"a":1}'
  )
  # Don't need to assert the request body, or even the URL
  expect_PUT(
    request("http://httpbin.org/put") \%>\%
      req_method("PUT") \%>\%
      req_body_json(list(a = 1)) \%>\%
      req_perform()
  )
  expect_no_request(rnorm(5))
})
}
