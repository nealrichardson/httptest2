with_mock <- NULL

.onLoad <- function(...) {
  if (utils::packageVersion("httr2") > "0.2.3") {
    with_mock <<- function(...) httr2::with_mocked_responses(...)
  } else {
    with_mock <<- function(...) httr2::with_mock(...)
  }
}
