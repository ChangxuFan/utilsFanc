#
rbind.union.fanc <- function(dfs) {
  dfs.colnames <- lapply(dfs, colnames) %>% Reduce(union, .)
  df.cat <- Reduce(rbind, lapply(dfs, function(x) {
    for (i in dfs.colnames[!dfs.colnames %in% colnames(x)])
      x[,i] <- NA
    return(x)
  }))
  return(df.cat)
}


df.rearrange.cols <- function(df, cols, ...) {
  df2 <- df[, cols, drop = F]
  df <- df[, ! colnames(df) %in% cols]
  df <- add.column.fanc(df1 = df, df2 = df2, ...)
  return(df)
}

add.column.fanc <- function(df1, df2, pos=NULL, before=NULL, after=NULL,
  is.gr = F) {
  # priority: pos, before, and after. If none of them specified: append at the end.
  shift <- 0
  if (is.gr == T) {
    df1 <- df1 %>% `names<-`(NULL) %>% as.data.frame()
    shift <- 3
  }
  if(sum(colnames(df2) %in% colnames(df1)) > 0)
    stop("overlapping names between df2 and df1")

  if (!is.null(pos)) {
    j <- cbind(df1, df2)
    pos <- pos+shift
    cnames <- append(colnames(df1), values = colnames(df2), after = pos-1)
    j <- j[,cnames]
    # return(j)
  } else if (!is.null(before)) {
    j <- cbind(df1, df2)
    pos <- which(colnames(df1) == before) + shift
    cnames <- append(colnames(df1), values = colnames(df2), after = pos-1)
    j <- j[,cnames]
    # return(j)
  } else if (!is.null(after)) {
    j <- cbind(df1, df2)
    pos <- which(colnames(df1) == after) + 1 + shift
    cnames <- append(colnames(df1), values = colnames(df2), after = pos-1)
    j <- j[,cnames]
    # return(j)
  } else {
    j <- cbind(df1, df2)
    # return(j)
  }
  if (is.gr == T) {
        j <- makeGRangesFromDataFrame(j, keep.extra.columns = T)
  }
  return(j)

}

rotate.df.fanc <- function(df, id=NULL) {
  # untested
  cnames <- colnames(df)
  rnames <- rownames(df)
  if (!is.null(id))
    rnames <- df[,id]
  dft <- t(df) %>% as.data.frame()
  colnames(dft) <- rnames
  rownames(dft) <- cnames
  return(dft)
}

# png.test <- function() {
#   img <- readPNG("~/4dn/nk/fanc/encode/frag_length/Sp_NK_Ly49A_neg_rep1/*.png" %>% Sys.glob())
#   return(img %>% str())
# }

smart.break.fanc <- function(sample.name, width) {
  # if longer than width: break into multiple lines. only support scalar values.
  #print(sample.name)
  s <- strsplit(sample.name, "") %>% unlist()
  if (length(s) > width) {
    if (! "_" %in% s[1:width]) {
      s <- append(s, "_", after = width-1)
    }

    breakpoint <- which(s[1:width] == "_")[length(which(s[1:width] == "_"))]
    #print(breakpoint)
    part1 <- s[1:breakpoint] %>% paste0(collapse = "")
    part2 <- s[(breakpoint + 1):length(s)] %>% paste0(collapse = "")
    paste0(part1, "\n", part2 %>% smart.break.fanc (width)) %>%
      return()

  } else {
    return(sample.name)
  }
}


change.name.fanc <- function(df, cols.from, cols.to) {
  cnames <- colnames(df)
  for (i in 1:length(cols.from)) {
    cnames[which(cnames==cols.from[i])] <- cols.to[i]
  }
  colnames(df) <- cnames
  return(df)
}

split.fanc <- function(df, split.by) {
  if (length(split.by) !=1)
    stop("split.by must be of length 1")
  if (! split.by %in% colnames(df))
    stop("split.by must be a column in df")


  df %>% split.data.frame(., factor(.[, split.by], levels = unique(.[, split.by]))) %>% return()
}

gr.merge <- function(gr, func, col) {
  gr.r <- GenomicRanges::reduce(gr, with.revmap = T)
  pointers <- sapply(gr.r$revmap, function(x) {
    #print("miao")
    pointer.sub <- do.call( what = func, args = list(gr@elementMetadata[x, col]))[1]
    #print(pointer)
    return(x[pointer.sub])
  })
  gr.sub <- gr[pointers, ]
  GenomicRanges::ranges(gr.sub) <- GenomicRanges::ranges(gr.r)
  return(gr.sub)
}

gr.get.loci <- function(gr) {
  df <- granges(gr) %>% as.data.frame() 
  return(paste0(df$seqnames, ":", df$start, "-", df$end))
}

gr.center <- function(gr) {
  gr <- gr - (GenomicRanges::width(gr) - 1)/2
  return(gr)
}

write.zip.fanc <- function(df, out.file, bed.shift = F, zip=T, col.names = F, row.names = F, sep = "\t") {
  if (is(df, "GRanges")) {
    df <- df %>% `names<-`(NULL) %>% as.data.frame()
    if (ncol(df) < 6)
      df$forth <- paste0("region", 1:nrow(df))
    df <- df.rearrange.cols(df, "strand", pos = 6)
    df <- df.rearrange.cols(df, "width", pos = 5)
    
  }
  df <- as.data.frame(df)
  if (bed.shift == T) {
    df[, 2] <- df[, 2] - 1
  }
  system(paste0("mkdir -p ", dirname(out.file)))
  write.table(df, out.file, sep = sep, quote = F, row.names = row.names, col.names = col.names)
  if (zip == T) {
    system(paste0("/bar/cfan/scripts/bed_browser_v2.sh ", out.file))
    cat(utilsFanc::bash2ftp(paste0(out.file, ".gz")))
    cat("\n")
  }
  return(out.file)
}

factor2character.fanc <- function(df) {
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else {x})
  return(df)
}

import.bed.fanc <- function (bed, col.names = c("chr", "start", "end", "forth", 
    "fifth", "strand"), return.gr = F, use.rtracklayer.naming = F, no.shift = F) 
{
    if (grepl(".gz$", bed)) {
        bed <- sub(".gz", "", bed)
    }
    df <- read.table(bed, as.is = T, sep = "\t", quote = "", 
        header = F)
    if (use.rtracklayer.naming == T)
      col.names[4] <- "name"
    colnames(df) <- col.names[1:ncol(df)]
    if (no.shift != F) {
        df$start <- df$start + 1
    }
    if (return.gr == T) {
        df <- GenomicRanges::makeGRangesFromDataFrame(df = df, 
            keep.extra.columns = T)
    }
    return(df)
}

