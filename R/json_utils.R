string.to.list <- function(string.vector) {
  md <- string.vector %>% strsplit("|", fixed = T) %>% unlist()
  #print(md)
  md.names <- sapply(md, function(x) {
    sub("^([^\\:]+)\\:(.+)$", "\\1", x)
  })
  # print(md.names)
  md.values <- lapply(md, function(x) {
    sub("^([^\\:]+)\\:(.+)$", "\\2", x)
  })

  names(md.values) <- md.names
  return(md.values)
}




jsongen <- function(df, outfile=NULL) {
  if (is.character(df))
    df <- read.table(df, as.is = T, header = T)
  df.list <- df %>% split(factor(df$name, levels = df$name %>% unique()))
  names(df.list) <- NULL
  # print(df)
  json <- lapply(df.list, function(x) {
    js <- list()
    js[["type"]] <- x$track_type %>% jsonlite::unbox()
    js[["name"]] <- x$name %>% jsonlite::unbox()
    js[["url"]] <- x$url %>% jsonlite::unbox()
    js[["metadata"]] <- gsub("novalue", "", x$metadata ) %>% string.to.list() %>% lapply(jsonlite::unbox)

    js[["options"]] <- gsub("novalue", "", x$options ) %>% string.to.list() %>% lapply(jsonlite::unbox)
    return(js)
  })
  if (!is.null(outfile))
    json %>% jsonlite::toJSON() %>%jsonlite::prettify() %>%write(outfile)
  return(json)
}
