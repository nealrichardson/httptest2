httr2_1.0.0 <- NULL

.onLoad <- function(...) {
  # packageVersion() can be slow, so let's only do this check once
  httr2_1.0.0 <<- utils::packageVersion("httr2") > "0.2.3"
}
