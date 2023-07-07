Sys.setlocale("LC_COLLATE", "C") # What CRAN does
set.seed(999)
options(
  warn = 1,
  httptest2.debug.trace = FALSE
)

library(httr2, warn.conflicts = FALSE)

httpbin <- webfakes::local_app_process(
  webfakes::httpbin_app(),
  .local_envir = testthat::teardown_env()
)
