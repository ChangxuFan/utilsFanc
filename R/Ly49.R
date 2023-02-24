klra.ly49.translate <- function(x = NULL, lookup = "~/genomes/ly49/Klra.Ly49.Lookup.tsv",
                                bAll.Ly49 = F, bAll.Klra = F,
                                bKeep.not.found = F) {
  df0 <- read.table(lookup, sep = "\t", header = T)
  if (is.null(x)) {
    if (bAll.Ly49) {
      return(df0$Ly49)
    } else if (bAll.Klra) {
      return(df0$Klra)
    } else {
      stop("nothing to do")
    }
  }
  if (any(grepl("Ly49", x))) {
    df <- data.frame(Ly49 = x)
    res <- suppressMessages(df %>% left_join(df0))
    if (bKeep.not.found) {
      res$Klra[is.na(res$Klra)] <- res$Ly49[is.na(res$Klra)]
    }
    res <- res$Klra
  } else {
    df <- data.frame(Klra = x)
    res <- suppressMessages(df %>% left_join(df0))
    if (bKeep.not.found) {
      res$Ly49[is.na(res$Ly49)] <- res$Klra[is.na(res$Ly49)]
    }
    res <- res$Ly49
  }
  return(res)
}



fanc2nylenna <- function(x, tsv = "~/genomes/rn7/ly49/fanc2nylenna.tsv", 
                         to = "n", bStringi = F, out.file = NULL) {
  if (file.exists(x[1])) {
    x <- readLines(x)
  }
  if (!bStringi) {
    pat <- "^(rn7\\.)*Ly49"
    prefix <- stringr::str_extract(x, pat)
    prefix[is.na(prefix)] <- ""
    x <- sub(pat, "", x)
  }
  df <- read.table(tsv, header = T)
  if (!identical(sort(colnames(df)), sort(c("nylenna", "fanc")))) {
    stop("colnames of tsv must be nylenna and fanc")
  }
  for (i in c("nylenna", "fanc")) {
    dups <- df[, i] %>% .[duplicated(.)] %>% unique()
    if (length(dups) > 0)
      stop(paste0("duplicates found in column ", i, ":\n",
                paste0(dups, collapse = "\n")))
  }
  if (to == "n") {
    rename.vec <- df$nylenna
    names(rename.vec) <- df$fanc
  } else if (to == "f") {
    rename.vec <- df$fanc
    names(rename.vec) <- df$nylenna
  } else {
    stop("to must be f or n, for fanc and nylenna, respectively")
  }
  if (!bStringi){
    utilsFanc::check.intersect(x, "input", names(rename.vec), 
                               y.name = paste0("column ", ifelse(to == "n", "fanc", "nylenna")))
    out <- rename.vec[x]
    out <- paste0(prefix, out)
  } else {
    rename.vec <- rename.vec[rev(order(nchar(rename.vec)))]
    out <- stringi::stri_replace_all_fixed(str = x, 
                                           pattern = paste0("rn7.Ly49", names(rename.vec)), 
                                           replacement = paste0("rn7.Ly49@@@", rename.vec),
                                           vectorize_all = F)
    out <- gsub("@@@", "", out)
  }
  if (!is.null(out.file)) {
    dir.create(path = dirname(out.file), recursive = T, showWarnings = F)
    write(x = out, file = out.file, sep = "\n")
  }
  return(out)
}
