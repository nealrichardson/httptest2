Sys.setlocale("LC_COLLATE", "C") # What CRAN does
set.seed(999)
options(
  warn = 1,
  httptest.debug = FALSE
)

library(httr2, warn.conflicts = FALSE)

# HTTR2: some shims to help update the tests from httr to httr2
GET <- function(url) {
  request(url) %>%
    req_error(is_error = ~FALSE) %>%
    req_perform()
}

PUT <- function(url) {
  request(url) %>%
    req_method("PUT") %>%
    req_perform()
}

POST <- function(url) {
  request(url) %>%
    req_method("POST") %>%
    req_perform()
}

PATCH <- function(url) {
  request(url) %>%
    req_method("PATCH") %>%
    req_perform()
}

DELETE <- function(url) {
  request(url) %>%
    req_method("DELETE") %>%
    req_perform()
}

capture_while_mocking <- function(..., path) {
  with_mock_path(path, {
    # We'll write to `path` but read from wherever was set before
    tracer <- quote({
      .mockPaths <- function() getOption("httptest.mock.paths")[-1]
    })
    with_trace("find_mock_file",
      where = with_mock_api, tracer = tracer,
      expr = capture_requests(...)
    )
  })
}

with_redactor <- function(x, ...) {
  old <- getOption("httptest.redactor")
  old.pkgs <- getOption("httptest.redactor.packages")
  set_redactor(x)
  on.exit({
    set_redactor(old)
    options(httptest.redactor.packages = old.pkgs)
  })
  eval.parent(...)
}

reset_redactors <- function() {
  options(
    httptest.redactor = NULL,
    httptest.redactor.packages = NULL
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
