# Some constants

EXT_TO_CONTENT_TYPE <- list(
  "json" = "application/json",
  "xml" = "application/xml",
  "csv" = "text/csv",
  "html" = "text/html",
  "txt" = "text/plain",
  "tsv" = "text/tab-separated-values"
)

CONTENT_TYPE_TO_EXT <- structure(as.list(names(EXT_TO_CONTENT_TYPE)),
  .Names = unlist(EXT_TO_CONTENT_TYPE, use.names = FALSE)
)
