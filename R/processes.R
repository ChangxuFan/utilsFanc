t.stat <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(paste0(Sys.time(), " : ", msg, "\n"))
}
