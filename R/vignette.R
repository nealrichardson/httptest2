#' Set mocking/capturing state for a vignette
#'
#' Use `start_vignette()` to either use previously recorded responses or capture
#' real responses for future use, depending on the value of the `RECORD`
#' environment variable.
#'
#' In a vignette or other R Markdown or Sweave document, place
#' `start_vignette()` in an R code block at the beginning,
#' before the first API request is made, and put
#' `end_vignette()` in a R code chunk at the end. You may
#' want to make those R code chunks have `echo=FALSE` in order to hide the fact
#' that you're calling them.
#'
#' The behavior changes based on the existence of the `path` directory. The
#' first time you build the vignette, the directory won't exist yet, so it will
#' make real requests and record them inside of `path`. On subsequent runs, the
#' mocks will be used. To record fresh responses from the server, delete the
#' `path` directory, and the responses will be recorded again the next time the
#' vignette runs.
#'
#' @param path Root file path for the mocks for this vignette. A good idea is
#' to use the file name of the vignette itself.
#' @param ... Optional arguments passed to `start_capturing()`
#' @return Nothing; called for its side effect of starting/ending
#' response recording or mocking.
#' @export
#' @seealso [start_capturing()] for how requests are recorded; [use_mock_API()]
#' for how previously recorded requests are loaded; [change_state()] for how to
#' handle recorded requests when the server state is changing;
#' `vignette("vignettes", package="httptest")` for an overview of all
start_vignette <- function (path, ...) {
    ## Cache the original .mockPaths so we can restore it on exit
    ## And don't print messages in a vignette
    options(
        httptest.mock.paths.old=getOption("httptest.mock.paths"),
        httptest.verbose=FALSE
    )
    ## Set the starting mockPath
    .mockPaths(file.path(path, "0"))
    if (dir.exists(path)) {
        ## We already have recorded, so use the fixtures
        use_mock_API()
    } else {
        ## Record!
        start_capturing(...)
    }
}

#' Handle a change of server state
#'
#' Changes to server state can be handled by setting a new [.mockPaths()]. In
#' vignettes, these are handled by creating subdirectories with integer names.
#' This function increments that integer subdirectory and adds it to
#' `.mockPaths()` so that changed server state can be captured and loaded.
#'
#' In a vignette,
#' put a call to `change_state()` before any code block that makes a change on
#' the server, or rather, before any code block that might repeat the same
#' request previously done and expect a different result.
#'
#' @return Invisibly, the return of `.mockPaths()` with the new path added.
#' @export
change_state <- function () {
    current_path <- .mockPaths()[1]
    path_segments <- unlist(strsplit(current_path, .Platform$file.sep))
    current_number <- suppressWarnings(as.numeric(path_segments[length(path_segments)]))
    if (is.na(current_number)) {
        ## This path doesn't come from start_vignette/change_state
        stop(current_path, " is not valid for change_state()")
    }
    path_segments[length(path_segments)] <- current_number + 1
    new_path <- do.call(file.path, as.list(path_segments))
    .mockPaths(new_path)
}

#' @rdname start_vignette
#' @export
end_vignette <- function () {
    stop_capturing()
    stop_mocking()
    ## TODO: compress recorded mocks (in case change_state() was called but
    ## state didn't actually change) if we were recording

    ## Restore original .mockPaths
    options(
        httptest.mock.paths=getOption("httptest.mock.paths.old"),
        httptest.mock.paths.old=NULL
    )
}