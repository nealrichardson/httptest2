#' Wrapper around 'trace' to untrace when finished
#'
#' @param x Name of function to trace. See [base::trace()].
#' @param where where to look for the function to be traced.
#' @param print Logical: print a message every time the traced function is hit?
#' Default is `FALSE`; note that in `trace`, the default is `TRUE`.
#' @param ... Additional arguments to pass to `trace`. At minimum, must include
#' either `tracer` or `exit`.
#' @param expr Code to run inside the context
#' @return The result of `expr`
#' @export
#' @keywords internal
with_trace <- function(x, where = topenv(parent.frame()), print = getOption("httptest2.debug.trace", FALSE), ..., expr) {
  quietly(trace(x, print = print, where = where, ...))
  on.exit(safe_untrace(x, where = where))
  eval.parent(expr)
}

#' @importFrom utils sessionInfo
trace_httr2 <- function(..., print = getOption("httptest2.debug.trace", FALSE)) {
  # Trace it as seen from within httr2
  quietly(trace(..., print = print, where = request))
  # And if httr2 is attached and the function is exported, trace the
  # function as the user sees it
  if ("httr2" %in% names(sessionInfo()$otherPkgs) && ..1 %in% getNamespaceExports("httr2")) {
    try(quietly(trace(..., print = print, where = sys.frame())))
  }
}

quietly <- function(expr) {
  env <- parent.frame()
  if (getOption("httptest2.debug.trace", FALSE)) {
    eval(expr, env)
  } else {
    suppressMessages(eval(expr, env))
  }
}

untrace_httr2 <- function(what) {
  safe_untrace(what, request)
  safe_untrace(what)
}

safe_untrace <- function(what, where = sys.frame()) {
  # If you attempt to untrace a function (1) that isn't exported from
  # whatever namespace it lives in and (2) that isn't currently traced,
  # it errors. This prevents that so that it's always safe to call `untrace`

  # untrace() and get() handle enviroments differently
  if (is.environment(where)) {
    env <- where
  } else {
    env <- environment(where)
  }
  if (inherits(try(get(what, env), silent = TRUE), "functionWithTrace")) {
    quietly(untrace(what, where = where))
  }
}
