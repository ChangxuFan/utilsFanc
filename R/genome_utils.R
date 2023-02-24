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

loci.2.df <- function (df = NULL, loci.col.name = NULL, loci.vec, forth.name = "forth", from.igv = T, 
          remove.loci.col = F, return.gr = F, out.bed = NULL) 
{
  if (is.null(df)) {
    df <- data.frame(loci = loci.vec)
    loci.col.name <- "loci"
  }
  if (from.igv == T) {
    df[, loci.col.name] <- gsub(",", "", df[, loci.col.name])
  }
  if (sum(c("chr", "start", "end") %in% colnames(df)) > 0) {
    stop("chr, left, end should not be present in the colnames of df")
  }
  df2 <- data.frame(chr = sub(":.+$", "", df[, loci.col.name]), 
                    start = sub("(.+):(\\d+)-(\\d+).*", "\\2", df[, loci.col.name]) %>% 
                      as.numeric(), end = sub("(.+):(.+)-(\\d+).*", "\\3", 
                                              df[, loci.col.name]) %>% as.numeric())
  if (any(grepl("@", df[, loci.col.name]))) {
    df2[, forth.name] <- stringr::str_extract(df[, loci.col.name], pattern = "@.+") %>% 
      sub("@", "", .)
  }
  df <- utilsFanc::add.column.fanc(df1 = df, df2 = df2, pos = 1)
  if (remove.loci.col == T) 
    df <- df[, colnames(df) != loci.col.name]
  if (!is.null(out.bed)) {
    utilsFanc::write.zip.fanc(df = df[, 1:4], out.file = out.bed, 
                              zip = T)
  }
  if (return.gr == T) {
    res <- GenomicRanges::makeGRangesFromDataFrame(df, keep.extra.columns = T)
  }
  else {
    res <- df
  }
  return(res)
}

loci.2.gr <- function(loci) {
  return(loci.2.df(loci.vec = loci, return.gr = T, remove.loci.col = T))
}
annotate.fanc <- function(gr, genome, genome.name = NULL, annoDb = NULL, TxDb = NULL,
  anno.cols = NULL) {
  if (genome == "mm10") {
    genome.name <- "BSgenome.Mmusculus.UCSC.mm10"
    annoDb <- "org.Mm.eg.db"
    #annoDb <- NULL
    # TxDb <- TxDb.Mmusculus.UCSC.mm10.knownGene::TxDb.Mmusculus.UCSC.mm10.knownGene
    TxDb <- AnnotationDbi::loadDb(file = "~/genomes/mm10/gencode/TxDb.mm10.gencode.v24.klraps.sqlite")
  }

  if (genome == "hg38") {
    genome.name <- "BSgenome.Hsapiens.UCSC.hg38"
    annoDb <- "org.Hs.eg.db"
    TxDb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
  }
  names(gr) <- NULL
  anno <- gr %>%
    ChIPseeker::annotatePeak(TxDb=TxDb, annoDb = annoDb)
  if (length(anno@anno) != length(gr)) {
    stop ("length(anno@anno) != length(gr)")
  }
  gr <- anno@anno
  rm(anno)
  if (!is.null(anno.cols)) {
    mcols(gr) <- mcols(gr)[, anno.cols, drop = F]
  }
  return(gr)
}

gene.name.formater <- function(genes, format = "mouse") {
  if (format == "mouse") {
    genes <- genes %>% stringr::str_to_title(genes) %>% sub("^Mt-", "mt-", .)
    return(genes)
  } else {
    stop("only mouse is developed")
  }
}

gene.list.from.string <- function(x) {
  res <- x %>% gsub("[^0-9A-Za-z-]+", ";", .) %>% 
    strsplit(split = ";") %>% unlist()
  return(res)
}
ensemblID.2.gene <- function(ensembl_ID, genome) {
  # cow: btaurus
  ensembl <- useEnsembl(biomart = "ENSEMBL_MART_ENSEMBL")
  ensembl <- useDataset(paste0(genome, "_gene_ensembl"), mart = ensembl)
  G_list <- getBM(filters = "ensembl_gene_id", attributes = c("ensembl_gene_id","external_gene_name"),
                  values = ensembl_ID, mart= ensembl)
  ori.df <- data.frame(ensembl_gene_id = ensembl_ID)
  df <- left_join(ori.df, G_list)
  if (nrow(df) != nrow(ori.df)) {
    stop("nrow(df) != nrow(ori.df)")
  }
  return(df$external_gene_name)
}