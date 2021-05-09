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
  root.name <- tools::file_path_sans_ext(name)
  ext <- tools::file_ext(name)
  out.name <- paste0(root.name, delim, insert, ".", ext)
  return(out.name)
}
