Sys.setlocale("LC_COLLATE", "C") # What CRAN does
set.seed(999)
options(
  warn = 1,
  httptest2.debug.trace = FALSE
)

library(httr2, warn.conflicts = FALSE)

capture_while_mocking <- function(..., path) {
  with_mock_path(path, {
    # We'll write to `path` but read from wherever was set before
    tracer <- quote({
      .mockPaths <- function() getOption("httptest2.mock.paths")[-1]
    })
    with_trace("find_mock_file",
      where = with_mock_api, tracer = tracer,
      expr = capture_requests(...)
    )
  })
}

with_redactor <- function(x, ...) {
  old <- getOption("httptest2.redactor")
  old.pkgs <- getOption("httptest2.redactor.packages")
  set_redactor(x)
  on.exit({
    set_redactor(old)
    options(httptest2.redactor.packages = old.pkgs)
  })
  eval.parent(...)
}

reset_redactors <- function() {
  options(
    httptest2.redactor = NULL,
    httptest2.redactor.packages = NULL
  )
}

install_testpkg <- function(pkg, lib = tempfile()) {
  dir.create(lib)
  tools::Rcmd(c("INSTALL", "testpkg", paste0("--library=", shQuote(lib))),
    stdout = NULL, stderr = NULL
  )
  return(lib)
}

skip_on_R_older_than <- function(version) {
  r <- R.Version()
  if (utils::compareVersion(paste(r$major, r$minor, sep = "."), version) < 0) {
    skip(paste("Requires R >=", version))
  }
}

# Mockable version of testthat::skip_if_offline()
skip_if_disconnected <- function(message = paste("Offline: cannot reach", url),
                                 url = "http://httpbin.org/") {
  skip_on_cran()
  if (currently_offline(url)) {
    skip(message)
  }
  invisible(TRUE)
}
