insert.name <- function (name, insert, ext, trim.dir = F)
{
    if (trim.dir == T)
        name <- basename(name)
    inserted <- sub(paste0("(.*)(", ext, ")"), paste0("\\1_",
        insert, "\\2"), name)
    return(inserted)
}

insert.name.2 <- function(name, insert, ext, pre = ".", trim.dir=F) {
  if (trim.dir == T) name <- basename(name)
  inserted <- sub(paste0("(.*)","(",pre ,")" ,"(", ext, ")"), paste0("\\1\\2", insert, "\\3"), name)
  return(inserted)
}

insert.name.before.ext <- function(name, insert, delim = ".") {
  if (grepl(".gz$", name)) {
    name <- sub(".gz$", "", name)
    extra <- ".gz"
  } else {
    extra <- ""
  }
  root.name <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  out.name <- paste0(root.name, delim, insert, ".", ext, extra)
  return(out.name)
}


glob.single.files <- function(x) {
  n.res <- sapply(x, function(x) {
    return(length(Sys.glob(x)))
  })
  if (!identical(1, as.numeric(unique(n.res)))) {
    which.0 <- which(n.res == 0)
    if (length(which.0) > 0) {
      msg <- paste0("These names returned no matches: \n",
                    paste0(x[which.0], collapse = "\n"))
    } else {
      msg <- ""
    }

    which.m <- which(n.res > 1)
    if (length(which.m) > 0) {
      msg <- paste0(msg, "\n", paste0(
        "These names returned more than 1 match: \n",
        paste0(x[which.m], collapse = "\n")))
    }
    stop(msg)
  }
  res <- sapply(x, function(x) {
    return(Sys.glob(x))
  })
  names(res) <- NULL
  return(res)
}

