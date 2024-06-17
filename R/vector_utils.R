sort.by <- function(x, y, missing.method = "error", return.order = F) {
  if (missing.method == "error") {
    check.intersect(x, "x", y, "y")
  } else if (missing.method == "remove") {
    x <- x[x %in% y]
  } else if (missing.method == "add") {
    new <- x[!x %in% y]
    y <- c(y, new)
  }
  m <- match(x, y)
  if (any(is.na(m)))
    stop("any(is.na(m))")
  if (return.order == T)
    return(order(m))
  else
    return(x[order(m)])
}

mixed.sort.by <- function(x, y = NULL, sep = "\\.\\.", sep.out = "..",
                          missing.method = "error") {
  df <- data.frame(a = sub(paste0(sep, ".+$"), "", x),
                   b = sub(paste0("^.+",sep), "", x))
  if (is.null(y))
    y <- df$a %>% gtools::mixedsort()
  order <- utilsFanc::sort.by(df$a, y, missing.method = missing.method, return.order = T)
  df <- df[order, ]
  df <- df %>% group_by(a) %>% mutate(b = gtools::mixedsort(b))
  return(paste0(df$a, sep.out, df$b))
}

so.formatter <- function(x, digits = 2, show.plus = F) {
  prefix <- rep("", length(x))
  prefix[x < 0] <- "-"
  if (show.plus == T)
    prefix[x > 0] <- "+"
  x <- abs(x)

  res <- dplyr::case_when(
    x < 1e3 ~ as.character(round(x, digits = digits)),
    x < 1e6 ~ paste0(round(x/1e3, digits = digits), "K"),
    x < 1e9 ~ paste0(round(x/1e6, digits = digits), "M"),
    x < 1e12 ~ paste0(round(x/1e9, digits = digits), "G"),
    x >= 1e12 ~ paste0(round(x/1e12, digits = digits), "T")
  )
  res <- paste0(prefix, res)
  return(res)
}

log2pm <- function(vec, base = 2, force = F) {
  # add the min non-zero value of a vector to this entire vector
  # prevent Inf result of log funtions.
  # better than log(x + 1) because 1 could be a large number
  # when you think about p values
  if (any(vec < 0)) {
    stop("must not contain negative values")
  }

  if (min(vec) > 0 && force == F) {
    vec <- vec
  } else {
    add.value <- min(vec[vec > 0])
    vec <- vec + add.value
  }
  vec <- log(vec, base = base)
  return(vec)
}
check.intersect <- function(x, x.name, y, y.name, n.examples = 5, warning.only = F) {
  not.in <- x %>% .[!x %in% y]
  n <- length(not.in)
  if ( n > 0) {
    message <- paste0(n, x.name, " are not found in ", y.name, ":\n",
                      paste0(not.in[1:min(n, n.examples)], collapse = "\n"))
    if (warning.only) {
      warning(message)
    } else {
      stop(message)
    }

  }
  return()
}


check.dups <- function(x, x.name, n.examples = 5) {
  dups <- x %>% .[duplicated(.)]
  if (length(dups) > 0) {
    stop(paste0("Some of ", x.name, " is duplicated: \n",
                paste0(dups[1:min(length(dups), n.examples)], collapse = "\n")))
  }
  return()
}

check.file.exists <- function(x, x.name, n.examples = 5) {
  ne <- x %>% .[!file.exists(x)]
  if (length(ne) > 0) {
    stop(paste0(ifelse(length(x) > 1, "Some of ", ""), x.name, " does not exist: \n",
                paste0(ne[1:min(length(ne), n.examples)], collapse = "\n")))
  }
  return()
}


intersect.summary <- function(x, y, x.name = "x", y.name = "y", ret.elements = F) {
  if (ret.elements) {
    x.only <- x %>% .[!. %in% y]
    y.only <- y %>% .[!. %in% x]
    shared <- intersect(x, y)
    res <- list(x.only, y.only, shared)
  } else {
    n.x.only <- x %>% .[!. %in% y] %>% length()
    n.y.only <- y %>% .[!. %in% x] %>% length()
    n.shared <- intersect(x, y) %>% length()
    res <- c(n.x.only, n.y.only, n.shared)
  }
  names(res) <- c(paste0(x.name, ".only"), paste0(y.name, ".only"), "shared")
  return(res)
}
centerRollMean <- function (v = NULL, k = NULL) {
  # this function is stolen from ArchR
  # never read how it actually works. v is an input vector to smooth, k is window size.
  o1 <- data.table::frollmean(v, k, align = "right", na.rm = FALSE)
  if (k%%2 == 0) {
    o2 <- c(rep(o1[k], floor(k/2) - 1), o1[-seq_len(k - 1)], rep(o1[length(o1)], floor(k/2)))
  }
  else if (k%%2 == 1) {
    o2 <- c(rep(o1[k], floor(k/2)), o1[-seq_len(k - 1)],
            rep(o1[length(o1)], floor(k/2)))
  }
  else {
    stop("Error!")
  }
  o2
}

loci.in <- function(x, y, return.id.y = F) {
  gx <- loci.2.gr(x)
  gy <- loci.2.gr(y)
  o <- GenomicRanges::findOverlaps(gx, gy)
  o <- as.data.frame(o)
  o <- o[!duplicated(o$queryHits),]

  out <- rep(NA, length(x))
  out[o$queryHits] <- o$subjectHits
  if (return.id.y) {
    return(out)
  } else {
    Out <- rep(F, length(out))
    Out[!is.na(out)] <- T
    return(Out)
  }
}
