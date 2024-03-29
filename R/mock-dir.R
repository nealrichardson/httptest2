#' Use or create mock files depending on their existence
#'
#' This context will switch the [.mockPaths()] to `tests/testthat/dir`
#' (and then resets it to what it was before).
#' If the `tests/testthat/dir` folder doesn't exist, [capture_requests()] will
#' be run to create mocks.
#' If it exists, [with_mock_api()] will be run.
#' To re-record mock files, simply delete `tests/testthat/dir` and run the test.
#'
#' @param dir character string, unique folder name that will be used or created
#' under `tests/testthat/`
#' @inheritParams start_capturing
#' @param replace Logical: should `dir` replace the contents of `.mockPaths()`
#' (default) or be added in front of the existing paths? The default behavior
#' here is the opposite of `.mockPaths()` so that the tests inside of
#' `with_mock_dir()` are fully isolated.
#' @seealso `vignette("httptest2")` for usage examples.
#' @export
with_mock_dir <- function(dir, expr, simplify = TRUE, replace = TRUE) {
  if (dir.exists("tests/testthat") && !(substr(dir, 1, 1) %in% c("/", "\\"))) {
    # If we're at the top level directory of the package,
    # assume any relative paths are meant to be inside tests/testthat.
    dir <- file.path("tests", "testthat", dir)
  }
  with_mock_path(dir, replace = replace, {
    if (dir.exists(dir)) {
      # We already have recorded, so use the fixtures
      verbose_message("Using mocks found in ", dir)
      with_mock_api(expr)
    } else {
      # Record!
      verbose_message("Recording responses to ", dir)
      capture_requests(expr, simplify = simplify)
    }
  })
}
