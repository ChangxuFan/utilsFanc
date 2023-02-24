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

get.track.type <- function(x) {
  lookup <- c(bw = "bigwig", bigwig = "bigwig", #bigWig = "bigwig", 
    bdg = "bedgraph", bedgraph = "bedgraph", # bedGraph = "bedgraph", 
    bed = "bed", narrowpeak = "bed", broadpeak = "bed", 
    bam = "bam", lr = "longrange", cat = "categorical",
    hic = "hic", cool = "cool", qbed= "qbed")
  ext <- sub(".gz$","",basename(x)) %>% tools::file_ext() %>% tolower()
  type <- lookup[ext]
  if (sum(is.na(type)) != 0) {
    warning(paste0("the type of these files cannot be parsed: \n", paste0(x[is.na(type)], collapse = "\n")))
  }
  return(type)
}


jsongen <- function(df, vec = NULL, strip = T, outfile=NULL) {
  if (!is.null(vec)) {
    df <- data.frame(url = vec)
  } else {
    if (is.character(df))
      df <- read.table(df, as.is = T, header = T)
  }

  
  if (is.null(df$name)) {
    df$name <- df$url
    if (strip == T) {
      df$name <- basename(df$name)
    }
  }
  if (is.null(df$metadata)) {
    df$metadata <- "novalue"
  }
  if (is.null(df$options)) {
    df$options <- "novalue"
  }
  if (is.null(df$track_type)) {
    df$track_type <- get.track.type(df$url)
  }
  
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

json.partition <- function(in.json, n.part) {

  in.js <- jsonlite::read_json(in.json)
  n.items <- length(in.js)
  n.per.chunk <- ceiling(n.items/n.part)
  id <- ceiling((1:n.items)/n.per.chunk)
  split.list <- in.js %>% split(f = id) 
  
  lapply(seq_along(split.list), function(i) {
    id <- names(split.list)[i]
    out.json <- sub(".json", paste0("_part", id, ".json"), in.json)
    split.list[[i]] %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::prettify() %>% 
      write(out.json)
  })
  return()
}

align.json <- function(files, query.genome, type = "genomeAlign", out.json) {
  if (length(query.genome) != 1) {
    if (length(files) != length(query.genome)) 
      stop("query.genome must be either 1 or the same length as files")
  } else {
    query.genome <- rep(query.genome, length(files))
  }
  
  json.list <- mapply(function(f, g) {
    j <- list()
    j$name <- basename(f)
    j$type <- type
    j$url <- bash2ftp(f)
    j$querygenome <- g
    return(j)
  }, files, query.genome, SIMPLIFY = F)
  names(json.list) <- NULL
  json <- json.list %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::prettify() %>% 
    write(out.json)
  cat(bash2ftp(out.json), sep = "\n")
  return(json)
}