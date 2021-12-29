#' Set an alternate directory for mock API fixtures
#'
#' By default, `with_mock_api()` will look for and `capture_requests()` will
#' write mocks to your package's `tests/testthat` directory, or else the current
#' working directory if that path does not exist. If you want to look in or
#' write to other places, call `.mockPaths()` to add directories to the search
#' path.
#'
#' It works like [base::.libPaths()]: any directories you specify will be added
#' to the list and searched first. The default directory will be searched last.
#' Only unique values are kept: if you provide a path that is already found in
#' `.mockPaths()`, the result effectively moves that path to the first position.
#'
#' For finer-grained control, or to completely override the default behavior
#' of searching in the current working directory, you can set the option
#' "httptest2.mock.paths" directly.
#' @param new Either a character vector of path(s) to add, or `NULL` to reset
#' to the default.
#' @return If `new` is omitted, the function returns the current search paths, a
#' a character vector. If `new` is provided, the updated value will be returned
#' invisibly.
#' @examples
#' .mockPaths()
#' .mockPaths("/var/somewhere/else")
#' .mockPaths()
#' .mockPaths(NULL)
#' .mockPaths()
#' @rdname mockPaths
#' @export
.mockPaths <- function(new) {
  current <- getOption("httptest2.mock.paths", default = default_mock_path())
  if (missing(new)) {
    # We're calling the function to get the list of paths
    return(current)
  } else if (is.null(new)) {
    # We're calling the function to reset to the default
    options(httptest2.mock.paths = new)
    invisible(current)
  } else {
    # We're adding one or more paths
    current <- unique(c(new, current))
    options(httptest2.mock.paths = current)
    invisible(current)
  }
}

default_mock_path <- function() {
  if (dir.exists("tests/testthat")) {
    "tests/testthat"
  } else {
    "."
  }
}

with_mock_path <- function(path, expr, replace = FALSE) {
  oldmp <- .mockPaths()
  if (replace) {
    options(httptest2.mock.paths = path)
  } else {
    # Append
    .mockPaths(path)
  }
  on.exit(options(httptest2.mock.paths = oldmp))
  eval.parent(expr)
}
