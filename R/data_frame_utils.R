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
  if (nrow(df2) == 1) {
    df2 <- df2[rep(1, nrow(df1)), , drop = F]
    rownames(df2) <- NULL
  }

  if (nrow(df1) != nrow(df2)) {
    stop("nrow(df1) != nrow(df2)")
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

gr.get.loci <- function(gr, keep.strand = F) {
  if (is.data.frame(gr))
    gr <- makeGRangesFromDataFrame(gr)
  df <- granges(gr) %>% as.data.frame()
  res <- paste0(df$seqnames, ":", df$start, "-", df$end)
  if (keep.strand == T) {
    res <- paste0(res, "@", df$strand)
  }
  return(res)
}

gr.center <- function(gr) {
  gr <- gr - (GenomicRanges::width(gr) - 1)/2
  return(gr)
}

gr.up.down.block <- function(gr, dist, size, up = T, ignore.strand = T) {
  gr <- GenomicRanges::flank(gr, width = dist, start = up, both = F, ignore.strand = ignore.strand) %>%
    GenomicRanges::flank(width = size, start = up, both = F, ignore.strand = ignore.strand)
  return(gr)
}

write.zip.fanc <- function(df, out.file, bed.shift = F, zip=T, col.names = F, row.names = F, sep = "\t",
                          ez = F) {
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
    if (ez) {
      script = "~/scripts/bed_browser_ez.sh"
    } else {
      script = "~/scripts/bed_browser_v2.sh"
    }
    system(paste0(script, " ", out.file))
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
    if (ncol(df) <= length(col.names))
      colnames(df) <- col.names[1:ncol(df)]
    else
      colnames(df)[1:length(col.names)] <- col.names
    if (no.shift == F) {
        df$start <- df$start + 1
    }
    if (return.gr == T) {
        df <- GenomicRanges::makeGRangesFromDataFrame(df = df,
            keep.extra.columns = T)
    }
    return(df)
}

import.bed.tabix <- function(bed, gr, tabix = TABIX) {
  o <- tempfile()
  r.out <- tempfile()
  utilsFanc::write.zip.fanc(gr, out.file = r.out, bed.shift = T, zip = F)

  cmd <- paste0(tabix, " -B ", bed, " ", r.out, " > ", o)
  system(cmd)
  gr <- utilsFanc::import.bed.fanc(o, return.gr = T)
  return(gr)
}

binarize.columns <- function(df, col) {
  items <- df[, col] %>% unique()
  for (i in items) {
    df[, i] <- 0
    df[ df[, col] == i, i] <- 1
  }
  return(df)
}

gr.extend.fanc <- function(gr, padding.left = 0, padding.right = 0, ignore.strand = F,
  return.df = F) {
  if (is.data.frame(gr)) {
    gr <- makeGRangesFromDataFrame(gr, keep.extra.columns = T)
  }
  size.final <- width(gr) + padding.left + padding.right
  gr <- gr %>% flank(padding.left, start = T, ignore.strand = ignore.strand) %>%
    resize(width = size.final, fix = "start", ignore.strand = ignore.strand)
  if (return.df == T)
    gr <- as.data.frame(gr %>% `names<-`(NULL))
  return(gr)
}

se.simplify <- function(se, assays = NULL, samples = NULL,
                        add.rowRanges = T, rowData.columns = NULL) {
  # simplify summerized experiment objects. Basically pooling rowRanges, rowData and the assay matrices
  # mainly developed to enable using se objects in scFanc::xy.plot()
  if (is.null(assays))
    assays <- names(assays(se))
  if (is.null(samples))
    samples <- colnames(se)
  if (is.null(rowData.columns))
    rowData.columns <- colnames(rowData(se))
  if (any(!assays %in% names(assays(se))))
    stop("some assays are not found in se")
  if (any(!samples %in% colnames(se)))
    stop("some samples are not found in se")
  if (any(! rowData.columns %in% colnames(rowData(se))))
    stop("some rowData.columns are not cound in se")

  df <- lapply(assays, function(assay) {
    df <- assays(se)[[assay]][, samples] %>% as.data.frame()
    colnames(df) <- paste0(colnames(df), ".", assay)
    return(df)
  }) %>% Reduce(cbind, .)

  df <- cbind(df, as.data.frame(rowData(se)[, rowData.columns, drop = F]))

  if (add.rowRanges == T) {
    loci <- data.frame(locus = utilsFanc::gr.get.loci(gr = rowRanges(se)))
    df <- cbind(loci, df)
  }
  return(df)
}

pmean <- function(df) {
  res <- df %>% apply(1, mean)
  return(res)
}

pmean.2 <- function(...) {
  df <- list(...) %>% as.data.frame()
  res <- df %>% apply(1, mean)
  return(res)
}

collapse.bed.fanc <- function(in.bed, bedtools.path, outfile = NULL) {
  if (is.character(in.bed))
    in.bed <- read.table(in.bed, header = F, quote = "", sep = "\t", as.is = T)
  df <- in.bed
  col.n.ori <- colnames(df)
  n.col <- ncol(df)
  if (n.col < 4)
    stop("collapse.bed.fanc requires at least 4 columns")
  colnames(df) <- paste0("V", 1:n.col)
  df$sep <- Reduce(function(x,y) paste0(x, "@", y), as.list(df[,4:n.col, drop=F]))

  # print(df)
  df.collapsed <- df %>% select(V1, V2, V3, sep) %>% split(., f= factor(.$sep, levels = unique(.$sep))) %>%
    lapply(function(x) {
      res <- find.contigs.fanc.2(x, bedtools.path = bedtools.path)
      res$sep <- x$sep[1]
      return(res)
    }) %>% Reduce(rbind, .) %>% arrange(V1, V2)

  res <- df.collapsed %>% tidyr::separate(sep, col.n.ori[4:length(col.n.ori)], "@")
  colnames(res) <- col.n.ori
  if (!is.null(outfile)) {
    write.table(res, outfile, sep = "\t", col.names = F, row.names = F, quote = F)
    system(paste0("/bar/cfan/scripts/bed_browser_v2.sh ", outfile))
  }

  return(res)
}


find.contigs.fanc.2 <- function(bed, bedtools.path) {
  # only chr, left, right are considered. use collapse.bed.fanc() if you don't want to collapse rows with
  ##different annotation columns (columns starting from the 4th column)
  # PATH <- Sys.getenv("PATH")
  # PATH <- paste0(PATH, ":", bedtools.path)
  # Sys.setenv(PATH = PATH)
  col.n.ori <- colnames(bed)[1:3]
  bed <- bed[,1:3]
  if (sum(bed[, 2] >=bed[,3]) > 0)
    stop("column 3 of bed must be larger than column 2")
  merged <- bedr::bedr.merge.region(bed, check.chr = F, verbose = F, check.sort = F)
  colnames(merged) <- col.n.ori
  rownames(merged) <- NULL
  return(merged)
}

gint.2.lr <- function(gint, value.col = NULL, out.file,
                      value.transformation = NULL) {
  if (is.null(value.col)) {
    value.col <- miao
    gint$miao <- 1
  }
  if (!is.null(value.transformation)) {
    mcols(gint)[, value.col] <- value.transformation(mcols(gint)[, value.col])
  }
  df <- gint %>% BiocGenerics::as.data.frame() %>% mutate(start1 = start1-1, start2 = start2 - 1)
  # head(df) %>% print()
  df1 <- df %>% mutate(forth = paste0(seqnames2, ":", start2, "-", end2,
                                      ",", !!as.name(value.col))) %>%
    dplyr::select(seqnames1, start1, end1, forth) %>% `colnames<-`(c("chr", "start", "end", "forth"))
  df2 <- df %>% mutate(forth = paste0(seqnames1, ":", start1, "-", end1,
                                      ",", !!as.name(value.col))) %>%
    dplyr::select(seqnames2, start2, end2, forth)  %>% `colnames<-`(c("chr", "start", "end", "forth"))

  df <- rbind(df1, df2) %>% arrange(chr, start)
  utilsFanc::write.zip.fanc(df = df, out.file = out.file, bed.shift = F)
  return()
}

extend.df.with.NA <- function(df, key = NULL, key.add) {
  bRownames <- !is.null(rownames(df))
  bMat <- is.matrix(df)
  if (bMat)
    df <- as.data.frame(df)
  if (!is.null(key)) {
    if (any(duplicated(df[, key])))
      stop("any(duplicated(df[, key])")
    rownames(df) <- df[, key]
  } else {
    if (is.null(rownames(df)))
      rownames(df) <- as.character(1:nrow(df))
  }

  key.add <- key.add %>% .[! key.add %in% rownames(df)]
  df.add <- lapply(colnames(df), function(col) {
    if (!is.null(key)) {
      if (col == key) {
        return(key.add)
      }
    }
    return(rep(NA, length(key.add)))
  })
  names(df.add) <- colnames(df)
  df.add <- as.data.frame(df.add)
  rownames(df.add) <- key.add
  df <- rbind(df, df.add)
  if (bMat) {
    df <- as.matrix(df)
  }
  if (bRownames == F)
    rownames(df) <- NULL
  return(df)
}





gr.slide <- function(df, name.col = "forth", bin.size,
                     n.bins = NULL, n.bins.up = NULL, n.bins.down = NULL,
                     out.dir) {
  if (is.character(df)) {
    df <- import.bed.fanc(bed = df, return.gr = F, no.shift = F)
  } else {
    df <- as.data.frame(df)
    df <- change.name.fanc(df = df, cols.from = name.col, cols.to = "forth")
  }
  df <- df.rearrange.cols(df = df, cols = "forth", pos = 4)
  df <- df[, 1:4]
  colnames(df) <- c("chr", "start", "end", "forth")
  if (!is.null(n.bins)) {
    n.bins.up <- n.bins
    n.bins.down <- n.bins
  }
  shift.vec <- 0
  if (!is.null(n.bins.up)) {
    shift.vec <- c(-1 * (n.bins.up:1), shift.vec)
  }
  if (!is.null(n.bins.down)) {
    shift.vec <- c(shift.vec, 1:n.bins.down)
  }

  shift.vec <- shift.vec * bin.size
  shift.names <- so.formatter(shift.vec, show.plus = T)
  df %>% split(., 1:nrow(.)) %>%
    lapply(function(locus) {
      df <- data.frame(chr = locus$chr, start = locus$start + shift.vec,
                       end = locus$end + shift.vec,
                       forth = locus$forth %>% paste0("..", shift.names))
      out.file <- paste0(out.dir, "/", locus$forth, ".bed")
      write.zip.fanc(df = df, out.file = out.file, bed.shift = T, zip = F)
    })
  return()
}

df.fix.direction <- function(df, col.small = "start", col.large = "end") {
  df <- df %>% mutate(..SMALL = !!as.name(col.small), ..LARGE = !!as.name(col.large)) %>%
    mutate(..DIST = ..LARGE - ..SMALL) %>%
    mutate(!!col.small := if_else(..DIST > 0, ..SMALL, ..LARGE)) %>%
    mutate(!!col.large := if_else(..DIST > 0, ..LARGE, ..SMALL)) %>%
    mutate(..DIST = NULL, ..SMALL = NULL, ..LARGE = NULL)
  return(df)
}

gr.fit2bin <- function(x, bin.size, expand = T, style = "GRanges") {
  if (is.character(x)) {
    x <- loci.2.gr(x) %>% as.data.frame()
    input.type <- "loci"
  } else if (is.data.frame(x)) {
    input.type <- "df"
  } else {
    input.type <- "gr"
    x <- as.data.frame(x)
  }
  if (expand == T) {
    x$start <- floor(pmax((x$start - 1), 0)/bin.size) * bin.size
    x$end <- ceiling(x$end/bin.size) * bin.size
  } else {
    stop("the shrinking one is not well debugged")
    x$start <- ceiling(pmax((x$start - 1), 0)/bin.size) * bin.size
    x$end <- floor(x$end/bin.size) * bin.size
  }

  x <- df.fix.direction(df = x, col.small = "start", col.large = "end")
  if (style == "GRanges") {
    x$start <- x$start + 1
  }
  res <- switch(input.type,
                loci = gr.get.loci(x),
                df = x,
                gr = makeGRangesFromDataFrame(x, keep.extra.columns = T))
  return(res)
}

gr.fast.annot <- function(obj, genome, anno.cols = "SYMBOL", use.klraps = T) {
  if (genome == "mm10") {
    if (use.klraps) {
      TxDb <- AnnotationDbi::loadDb(file = "~/genomes/mm10/gencode/TxDb.mm10.gencode.v24.klraps.sqlite")
      annoDb = "org.Mm.eg.db"
    } else {
      annoDb <- "org.Mm.eg.db"
      TxDb <- TxDb.Mmusculus.UCSC.mm10.knownGene::TxDb.Mmusculus.UCSC.mm10.knownGene
    }

  } else if (genome == "hg38") {
    annoDb <- "org.Hs.eg.db"
    TxDb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene

  } else {
    stop("only mm10 and hg38 genomes has been developed.")
  }

  if (any(c("RangedSummarizedExperiment") %in%
          class(obj))) {
    se.flag <- T
    gr <- rowRanges(obj)
  } else {
    se.flag <- F
    gr <- obj
  }
  gr$tempt.id <- 1:length(gr)
  anno <- ChIPseeker::annotatePeak(peak = gr, TxDb = TxDb, annoDb = annoDb)
  anno <- anno@anno %>% mcols() %>% as.data.frame()
  if (nrow(anno) != length(gr)) {
    stop("nrow(anno) != length(gr)")
  }
  if (!identical(sort(anno$tempt.id ), sort(gr$tempt.id)))
    stop("!identical(sort(anno$tempt.id ), sort(gr$tempt.id))")
  if (!is.null(anno.cols)) {
    anno <- anno %>% .[,colnames(.) %in% c("tempt.id", anno.cols), drop = F]
  }
  mcols(gr) <- left_join(as.data.frame(mcols(gr)), anno)
  mcols(gr)$tempt.id <- NULL
  if (se.flag == T) {
    rowRanges(obj) <- gr
  } else {
    obj <- gr
  }
  return(obj)
}

gr.drop.seqlevel <- function(gr) {
  df <- gr %>% `names<-`(NULL) %>% as.data.frame()
  df$seqnames <- as.character(df$seqnames)
  gr <- GenomicRanges::makeGRangesFromDataFrame(df, keep.extra.columns = T)
  return(gr)
}

gr2df <- function(gr, keep.width = T, keep.strand = T) {
  df <- gr %>% `names<-`(NULL) %>% as.data.frame()
  colnames(df)[1] <- "chr"
  df$chr <- as.character(df$chr)
  if (!keep.width) {
    df$width <- NULL
  }
  if (!keep.strand) {
    df$strand <- NULL
  }
  return(df)
}

gr.expand.smooth <- function(gr, smooth.win.1side, drop.seqlevel = F, drop.zero = F) {
  if (drop.seqlevel) {
    gr <- gr.drop.seqlevel(gr)
  }
  gr.exp <- gr + smooth.win.1side
  gr.exp <- coverage(gr.exp, weight = "score") %>% as("GRanges")
  if (drop.zero) {
    gr.exp <- gr.exp[gr.exp$score != 0]
  }
  return(gr.exp)
}

gr.expand.smooth

bed.2.cat <- function(beds, out.json = NULL) {
  if (is.null(out.json)) {
    if (length(beds) == 1) {
      out.json <- beds %>% sub(".gz$", "",.) %>% paste0(".cat.json")
    }
  }
  jgs <- lapply(beds, function(bed) {
    bed <- bed %>% sub(".gz$", "", .)
    df <- read.table(bed, header = F, sep = "\t", quote = "")
    cats <- df$V4 %>% unique() %>% sample(., length(.), replace = F)
    colors <- gg_color_hue(length(cats))
    cat.list <- lapply(seq_along(cats), function(i) {
      return(list(name = cats[i], color = colors[i]))
    })
    names(cat.list) <- cats
    cat.options <- list(category = cat.list)
    other.options <- list(height = 20, alwaysDrawLabel= T, maxRows= 100, hiddenPixels= 0)
    jg <- list(type = "categorical",
               name = basename(bed),
               url = utilsFanc::bash2ftp(paste0(bed, ".gz")),
               options = c(other.options, cat.options))
    return(jg)
  })
  json <- jgs %>% jsonlite::toJSON(auto_unbox = T) %>% jsonlite::prettify()
  write(json, out.json)
  cat(utilsFanc::bash2ftp(out.json))
  cat("\n")
  return()
}

gr.get.seq.max <- function(gr) {
  df <- suppressMessages({
    gr %>% `names<-`(NULL) %>% as.data.frame() %>%
      dplyr::select(seqnames, end) %>%
      dplyr::group_by(seqnames) %>%
      dplyr::summarise(max = max(end))
  })
  res <- df$max
  names(res) <- df$seqnames
  return(res)
}

gr.bp <- function(gr) {
  gr.frame <- tile(gr, width = 1) %>% unlist()
  # if (!any(is.na(seqlengths(gr))) && (!is.null(genome) || !is.null(chrom.sizes))) {
  #   print("seqlengths attribute present in gr, ignoring chrom.sizes and genome parameters")
  #   seq.lengths <- seqlengths(gr)
  # } else {
  #   if (!is.null(genome)) {
  #     chrom.sizes <- paste0("~/genomes/", genome, "/", genome, ".chrom.sizes")
  #   }
  #   if (is.null(chrom.sizes)) {
  #     seq.lengths <- utilsFanc::gr.get.seq.max(gr)
  #   } else {
  #     tmp <- read.table(chrom.sizes, header = F, sep = "\t")
  #     seq.lengths <- tmp$V2
  #     names(seq.lengths) <- tmp$V1
  #     avail.chroms <- seqlengths(gr) %>% names()
  #     utilsFanc::check.intersect(avail.chroms, x.name = "avail.chroms",
  #                                names(seq.lengths), y.name = "names(seq.lengths)")
  #     seq.lengths <- seq.lengths[avail.chroms]
  #   }
  #   seqlengths(gr) <- seq.lengths
  # }
  # gr.frame <- lapply(names(seq.lengths), function(chr) {
  #   df <- data.frame(chr = chr, start = 1:seq.lengths[chr])
  #   df$end <- df$start
  #   return(makeGRangesFromDataFrame(df, keep.extra.columns = T))
  # }) %>% Reduce(c, .)
  # browser()
  # gr$tempt_colll <- 1
  j.gr <- plyranges::join_overlap_left(x = gr.frame, y = gr)

  # if (! keep.empty.frame) {
  #   j.gr <- j.gr[!is.na(j.gr$tempt_colll)]
  # }
  # j.gr$tempt_colll <- NULL
  seqinfo(j.gr) <- seqinfo(gr)
  return(j.gr)
}

gr.expand.smooth.bp <- function(gr, ext, drop.zero = T) {
  gr <- gr %>% utilsFanc::gr.bp()
  gr <- trim(suppressWarnings(gr + ext))
  cov <- coverage(gr, weight = "score") %>% as("GRanges")
  # coverage will also trim off start < 1
  if (drop.zero) {
    cov <- cov[cov$score != 0]
  }
  seqinfo(cov) <- seqinfo(gr)
  return(cov)
}

gr.add.seqlengths <- function(gr, genome, rm = F) {
  chrom.sizes <- read.table(paste0("~/genomes/", genome, "/", genome, ".chrom.sizes"))
  seql <- chrom.sizes$V2
  names(seql) <- chrom.sizes$V1

  seq_avail <- seqlevels(gr)
  not_found <- seq_avail %>% .[!.%in% names(seql)]
  if (length(not_found) > 0) {
    msg <- paste0("some seqnames in gr is not found in chrom.sizes: \n",
      paste0(not_found, collapse = "\n"))
    if (rm) {
        seq_avail <- seq_avail[! seq_avail %in% not_found]
        seqlevels(gr, pruning.mode = "coarse") <- seq_avail
        warning(msg)
      } else {
        stop(msg)
      }
  }
  seql <- seql[seq_avail]
  seqlengths(gr) <- seql
  return(gr)
}


df.melt.by.suffix <- function(df, suffices, id.vars, variable.name) {
  dups <- which(duplicated(df[, id.vars]))
  if (length(dups) > 0) {
    stop(paste0("The following lines have duplicated id.vars: \n",
                paste0(dups, collapse = ", ")))
  }
  pattern <- paste0(paste0("\\.", suffices, "$"), collapse = "|")
  measure.vars <- colnames(df) %>% .[!. %in% id.vars] %>%
    .[grepl(pattern, .)]
  vars <- measure.vars %>% sub(pattern, "", .) %>% unique()

  vars.full <- outer(vars, paste0(".", suffices), paste0)
  not.found <- vars.full %>% .[!. %in% measure.vars]

  if (length(not.found) > 0) {
    stop(paste0("these columns are missing: ",
                paste0(not.found, collapse = ", ")))
  }

  df <- lapply(suffices, function(suffix) {
    cols <- paste0(vars, ".", suffix)
    df <- df[, c(id.vars, cols)]
    colnames(df) <- c(id.vars, vars)
    df <- reshape2::melt(df, id.vars = id.vars, measure.vars = vars,
                         variable.name = variable.name, value.name = suffix)
    df[, variable.name] <- as.character(df[, variable.name])
    return(df)
  }) %>% Reduce(left_join, .)
  return(df)
}

loci.dist.mat.core <- function(loci.x, loci.y) {
  x <- loci.2.gr(loci.x)
  y <- loci.2.gr(loci.y)
  dist <- GenomicRanges::distance(x, y, ignore.strand = T)
  return(dist)
}

loci.dist.mat <- function(loci.x, loci.y) {
  check.dups(loci.x, "loci.x")
  check.dups(loci.y, "loci.y")
  dist <- outer(loci.x, loci.y, loci.dist.mat.core)
  rownames(dist) <- loci.x
  colnames(dist) <- loci.y
  return(dist)
}

gr.dist.mat <- function(gr, name.col = NULL) {
  if (is.null(name.col)) {
    gr$name <- gr.get.loci(gr)
  } else {
    gr$name <- mcols(gr)[, name.col, drop = T]
  }

  check.dups(x = gr$name, x.name = "gr$name")
  map <- gr$name
  names(map) <- gr.get.loci(gr)

  dist <- loci.dist.mat(loci.x = names(map), loci.y = names(map))
  colnames(dist) <- map[colnames(dist)]
  rownames(dist) <- map[rownames(dist)]
  return(dist)
}

gene.dist.mat <- function(genes, gtf, return.too.close = F, cutoff = 100000) {
  if (is.character(gtf)) {
    gtf <- rtracklayer::import(gtf)
  }
  check.intersect(genes, "genes",  gtf$gene_name, "gtf$gene_name")
  gr <- gtf[gtf$gene_name %in% genes] %>% gr2df() %>%
    dplyr::group_by(gene_name) %>%
    dplyr::summarise(start = min(start), end = max(end), chr = chr[1]) %>%
    dplyr::ungroup() %>% as.data.frame() %>%
    makeGRangesFromDataFrame(keep.extra.columns = T)

  gr <- gr[sort.by(x = gr$gene_name, y = genes, return.order = T)]
  dist.mat <- gr.dist.mat(gr, name.col = "gene_name")
  if (!return.too.close) {
    return(dist.mat)
  }
  diag(dist.mat) <- NA
  dist.df <- reshape2::melt(dist.mat, varnames = c("x", "y"), value.name = "dist") %>% na.omit() %>%
    dplyr::filter(dist <= cutoff)
  dist.df <- dist.df %>% factor2character.fanc()
  dist.df <- dist.df[1:(0.5*nrow(dist.df)),]
  return(dist.df)
}


