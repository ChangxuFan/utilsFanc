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

add.column.fanc <- function(df1, df2, pos=NULL, before=NULL, after=NULL) {
  # priority: pos, before, and after. If none of them specified: append at the end.
  if(sum(colnames(df2) %in% colnames(df1)) > 0)
    stop("overlapping names between df2 and df1")

  if (!is.null(pos)) {
    j <- cbind(df1, df2)
    cnames <- append(colnames(df1), values = colnames(df2), after = pos-1)
    j <- j[,cnames]
    return(j)
  } else if (!is.null(before)) {
    j <- cbind(df1, df2)
    pos <- which(colnames(df1) == before)
    cnames <- append(colnames(df1), values = colnames(df2), after = pos-1)
    j <- j[,cnames]
    return(j)
  } else if (!is.null(after)) {
    j <- cbind(df1, df2)
    pos <- which(colnames(df1) == after) + 1
    cnames <- append(colnames(df1), values = colnames(df2), after = pos-1)
    j <- j[,cnames]
    return(j)
  } else {
    j <- cbind(df1, df2)
    return(j)
  }

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
