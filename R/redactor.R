#' Set a response redactor
#'
#' A redactor is a function that alters the response content being written
#' out in the [capture_requests()] context, allowing you to remove sensitive
#' values, such as authentication tokens,
#' as well as any other modification or truncation of the response body. By
#' default, the [redact_cookies()] function will be used to purge standard
#' auth methods, but `set_redactor()` allows you to provide a different one.
#'
#' Alternatively, you can put a redacting function in `inst/httptest2/redact.R`
#' in your package, and
#' any time your package is loaded (as in when running tests or building
#' vignettes), the function will be used automatically.
#'
#' @param FUN A function or expression that modifies `httr2_response` objects.
#' Specifically, a valid input is one of:
#' * A function taking a single argument, the `httr2_response`,
#' and returning a valid `httr2_response` object.
#' * A formula as shorthand for an anonymous function with `.` as the
#' "response" argument, as in the `purrr` package. That is, instead of
#' `function (response) redact_headers(response, "X-Custom-Header")`, you can
#' use `~ redact_headers(., "X-Custom-Header")`
#' * A list of redacting functions/formulas, which will be executed
#' in sequence on the response
#' * `NULL`, to override the default `redact_cookies()`.
#' @return Invisibly, the redacting function, validated and perhaps modified.
#' Formulas and function lists are turned into proper functions. `NULL` as input
#' returns the `force()` function.
#' @export
#' @seealso For further details on how to redact responses, see `vignette("redacting", package = "httptest2")`.
#' @examples
#' # Shorten UUIDs in response body/URLs to their first 6 digits:
#' set_redactor(function(resp) gsub_response(resp, "([0-9a-f]{6})[0-9a-f]{26}", "\\1"))
#' # Restore the default
#' set_redactor(redact_cookies)
set_redactor <- function(FUN) {
  FUN <- prepare_redactor(FUN)
  options(
    httptest2.redactor = FUN,
    # Because we're directly setting a redactor, remove any record that
    # a previous redactor was set by reading from packages
    httptest2.redactor.packages = NULL
  )
  invisible(FUN)
}

default_redactor <- function(packages = get_attached_packages()) {
  # Look for package-defined redactors
  func <- redactor_from_packages(packages)
  # Record what packages we considered here
  options(httptest2.redactor.packages = packages)
  return(func)
}

redactor_from_packages <- function(packages) {
  funcs <- find_package_functions(packages, "redact.R")
  if (length(funcs)) {
    out <- prepare_redactor(funcs)
  } else {
    # Default
    out <- redact_cookies
  }
  return(out)
}

find_package_functions <- function(packages, file = "redact.R") {
  # Given package names, find any redactors put in inst/httptest/redact.R
  base_pkgs <- c(
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils"
  )
  packages <- setdiff(packages, base_pkgs)
  funcs <- lapply(packages, get_package_function, file)
  # Make sure we have functions
  funcs <- Filter(is.function, funcs)
  return(funcs)
}

get_package_function <- function(package, file = "redact.R") {
  if ("pkgload" %in% loadedNamespaces()) {
    # Someone may have loaded a package with pkgload::load_all(), so we
    # need this shim function to look up system files
    system.file <- get("shim_system.file", asNamespace("pkgload"))
  }
  func_file <- system.file("httptest2", file, package = package)
  # If file does not exist, it returns ""
  if (!nzchar(func_file)) {
    # Try the httptest dir too
    func_file <- system.file("httptest", file, package = package)
  }
  if (nzchar(func_file)) {
    func <- source(func_file)$value
    if (is.function(func)) {
      verbose_message(paste("Using", file, "from", dQuote(package)))
      return(func)
    }
  }
  return(NULL)
}

#' Fetch the active redacting function
#'
#' Called inside [capture_requests()]. If using the default redactor, it checks
#' each time it is called to see if any new packages have been attached, in case
#' there are package redactors in them.
#' @return A redacting function.
#' @export
#' @keywords internal
get_current_redactor <- function() {
  # First, check where we've cached the current one
  out <- getOption("httptest2.redactor")
  if (is.null(out)) {
    # Set the default
    out <- default_redactor()
    options(httptest2.redactor = out)
  } else {
    # See if default is based on packages and needs refreshing
    pkgs <- getOption("httptest2.redactor.packages")
    if (!is.null(pkgs)) {
      # We're using the result of default_redactor(). Let's see if any
      # new packages have been loaded
      current_packages <- get_attached_packages()
      packages_changed <- !identical(current_packages, pkgs)
      # Also, always reevaluate the default redactor if pkgload is involved
      if (packages_changed || "pkgload" %in% loadedNamespaces()) {
        # Re-evaluate
        out <- default_redactor(current_packages)
        options(httptest2.redactor = out)
      }
    }
  }
  return(out)
}

prepare_redactor <- function(redactor) {
  if (is.null(redactor)) {
    # Allow, and make it do nothing
    redactor <- force
  } else if (inherits(redactor, "formula")) {
    redactor <- as.redactor(redactor)
  } else if (is.list(redactor)) {
    if (length(redactor) == 1) {
      redactor <- redactor[[1]]
    } else {
      redactor <- chain_redactors(redactor)
    }
  }

  if (!is.function(redactor)) {
    stop("Redactor must be a function or list of functions", call. = FALSE)
  }
  return(redactor)
}

get_attached_packages <- function() {
  gsub("^package\\:", "", grep("^package\\:", search(), value = TRUE))
}
