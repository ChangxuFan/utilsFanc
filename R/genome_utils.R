liftover.core <- function(in.dir, in.pattern, chain.file, out.dir, zip = T, run = T, 
                          liftOver = "/opt/apps/kentUCSC/334/liftOver") {
  dir.create(out.dir, recursive = T, showWarnings = F)
  in.vec <- list.files(in.dir) %>% .[grepl(in.pattern, .)]
  lapply(in.vec, function(x) {
    in.file <- file.path(in.dir, x)
    suffix <- chain.file %>% basename() %>% sub(".over.+", "", .) %>% sub(".to.", "_to_", .)
    ext <- tools::file_ext(x)
    root <- tools::file_path_sans_ext(x)
    out.file <- paste0(out.dir, "/", root, "_", suffix, ".", ext)
    cmd <- paste0(liftOver, " ", in.file, " ", chain.file, " ", out.file, " /dev/null")
    print(cmd)
    if (run == T) {
      system(cmd)
      if (zip == T) {
        cmd <- paste0("~/scripts/bed_browser_v2.sh ", out.file)
        print(cmd)
        system(cmd)
      }
    }
      
    
    return()
  }) 
  return()
}

loci.2.df <- function(df = NULL, loci.col.name = NULL, loci.vec, 
                      remove.loci.col = F, return.gr = F,out.bed = NULL) {
  if (is.null(df)) {
    df <- data.frame(loci = loci.vec)
    loci.col.name <- "loci"
  }
  if (sum(c("chr", "start", "end") %in% colnames(df)) > 0) {
    stop("chr, left, end should not be present in the colnames of df")
  }
  df2 <- data.frame(chr = sub(":.+$", "", df[, loci.col.name]),
                            start = sub("(.+):(.+)-(.+)", "\\2", df[, loci.col.name]) %>% as.numeric(),
                            end = sub("(.+):(.+)-(.+)", "\\3", df[, loci.col.name]) %>% as.numeric()) 
  df <- utilsFanc::add.column.fanc(df1 = df, df2 = df2, pos = 1)
  if (remove.loci.col == T)
    df <- df[,colnames(df) != loci.col.name]
  if (!is.null(out.bed)) {
    utilsFanc::write.zip.fanc(df = df[, 1:4], out.file = out.bed, zip = T)
  }
  if (return.gr == T) {
    res <- GenomicRanges::makeGRangesFromDataFrame(df, keep.extra.columns = T)
  } else {
    res <- df
  }
  
  return(res)
}
