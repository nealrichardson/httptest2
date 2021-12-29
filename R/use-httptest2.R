#' Use 'httptest2' in your tests
#'
#' This function adds `httptest2` to Suggests in the package DESCRIPTION and
#' loads it in `tests/testthat/setup.R`. Call it once when you're setting up
#' a new package test suite.
#'
#' The function is idempotent: if `httptest2` is already added to these files,
#' no additional changes will be made.
#'
#' @param path character path to the package
#' @return Nothing: called for file system side effects.
#' @export
use_httptest2 <- function(path = ".") {
  if (!("DESCRIPTION" %in% dir(path))) {
    stop(path, " is not an R package directory", call. = FALSE)
  }
  add_httptest2_to_desc(file.path(path, "DESCRIPTION"))
  add_httptest2_to_setup(file.path(path, "tests", "testthat", "setup.R"))
  invisible()
}

add_httptest2_to_desc <- function(file) {
  # Read DESCRIPTION, add httptest2 to Suggests if not already there

  # Hack to preserve whitespace: read it twice
  desc_fields <- colnames(read.dcf(file))
  desc <- read.dcf(file, keep.white = desc_fields)
  if (!("Suggests" %in% desc_fields)) {
    # Add a column for Suggests
    desc <- cbind(desc, matrix("", ncol = 1, dimnames = list(NULL, "Suggests")))
  }
  if (!grepl("httptest2", desc[, "Suggests"])) {
    # Add httptest2

    # Parse the list, and try to preserve the whitespace from the original
    suggested_pkgs <- unlist(strsplit(desc[, "Suggests"], ","))
    sep <- sub("^([[:blank:]\n]*).*", "\\1", suggested_pkgs)
    suggested_pkgs <- sort(c(trimws(suggested_pkgs), "httptest2"))
    extra_sep <- tail(unique(sep), 1)
    if (length(extra_sep) == 0 || nchar(extra_sep) == 0) {
      extra_sep <- " "
    }
    sep <- c(sep, extra_sep)
    desc[, "Suggests"] <- paste0(sep, suggested_pkgs, collapse = ",")

    # Msg and write
    message("Adding 'httptest2' to Suggests in DESCRIPTION")
    write.dcf(desc, file = file, keep.white = desc_fields)
  }
}

add_httptest2_to_setup <- function(file) {
  # Create tests/testthat/setup.R if it does not exist

  if (!file.exists(file)) {
    message("Creating ", file)
    message("Adding library(httptest2) to ", file)
    mkdir_p(file)
    cat("library(httptest2)\n", file = file)
    # Msg and write
  } else {
    setup_lines <- readLines(file)
    if (!any(grepl("library(httptest2)", setup_lines, fixed = TRUE))) {
      # Add "library(httptest)" to the top if it's not already there
      setup_lines <- c("library(httptest2)", setup_lines)
      # Msg and write
      message("Adding library(httptest2) to ", file)
      writeLines(setup_lines, file)
    }
  }
}
