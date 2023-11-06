httr2_1.0.0 <- NULL

.onLoad <- function(...) {
  httr2_1.0.0 <<- utils::packageVersion("httr2") > "0.2.3"
}
