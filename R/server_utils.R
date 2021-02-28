bash2ftp <- function(filename) {
  filename <- add.pwd(filename)
  ftp <- sub("^~", "https://wangftp.wustl.edu/~cfan", filename) %>%
    sub("/bar/cfan", "https://wangftp.wustl.edu/~cfan", .) %>%
    sub("/scratch/fanc/", "https://wangftp.wustl.edu/~cfan/sth/", .)

  return(ftp)
}

add.pwd <- function(filename) {
	out <- sapply(filename, function(x) {
		if (!grepl("^[~/]",x)) {
			x <- paste0(getwd(), "/", x)
		}
		return(x)
		})
	names(out) <- NULL
	return(out)
}