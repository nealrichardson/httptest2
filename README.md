# httptest2: Test Helpers for 'httr2'

[![Build Status](https://github.com/nealrichardson/httptest2/workflows/R-CMD-check/badge.svg)](https://github.com/nealrichardson/httptest2/actions)
[![codecov](https://codecov.io/gh/nealrichardson/httptest2/branch/main/graph/badge.svg?token=PxOGymudAS)](https://app.codecov.io/gh/nealrichardson/httptest2)

`httptest2` makes it easy to write tests for code and packages that wrap web APIs.
Testing code that communicates with remote servers can otherwise be painful: things like authentication, server state, and network flakiness can make testing seem too costly to bother with. The `httptest2` package enables you to test all of the logic on the R sides of the API in your package without requiring access to the remote service.

Importantly, it provides **contexts** that mock the network connection and tools for **recording** real requests for future offline use as fixtures, both in tests and in vignettes. The package also includes additional **expectations** to assert that HTTP requests were---or were not---made.

Using these tools, you can test that code is making the intended requests and that it handles the expected responses correctly, all without depending on a connection to a remote API. The ability to save responses and load them offline also enables you to write package vignettes and other dynamic documents that can be distributed without access to a live server.

This package is an adaptation of [`httptest`](https://enpiar.com/r/httptest/) to work with [`httr2`](https://httr2.r-lib.org/). Most features work exactly as they do in `httptest`; see the NEWS.md for the initial release for a summary of what has changed.

## Installing

`httptest2` can be installed from CRAN with

```r
install.packages("httptest2")
```

The pre-release version of the package can be pulled from GitHub using the [remotes](https://github.com/r-lib/remotes) package:

```r
# install.packages("remotes")
remotes::install_github("nealrichardson/httptest2")
```

## Getting started

To start using `httptest2` with your package, run `use_httptest2()` in the root of your package directory. This will

* add `httptest2` to "Suggests" in the DESCRIPTION file
* add `library(httptest2)` to `tests/testthat/setup.R`, which `testthat` loads before running tests

Then, you're ready to start using the tools that `httptest2` provides. For an overview of how to get started, see `vignette("httptest2")`, and check out `vignette("faq")` for some common questions. See also the [package reference](https://enpiar.com/httptest2/reference/) for a list of all of the test contexts and expectations provided in the package.
