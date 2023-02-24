bash2ftp <- function(filename) {
  filename <- add.pwd(filename)
  user <- Sys.getenv("USER")
  
  ftp <- sub("^~", paste0("https://wangftp.wustl.edu/~", user), filename) %>%
    sub(paste0("/bar/", user), paste0("https://wangftp.wustl.edu/~", user), .) %>%
    sub(paste0("/scratch/", user, "/"), paste0("https://wangftp.wustl.edu/~", user, "/sth/"), .) %>% 
	sub("/scratch/fanc/", "https://wangftp.wustl.edu/~cfan/sth/",.)
  return(ftp)
}

add.pwd <- function(filename) {
	out <- sapply(filename, function(x) {
		if (!grepl("^[~/]",x)) {
			x <- paste0(system("pwd", intern = T), "/", x)
		}
		return(x)
		})
	names(out) <- NULL
	return(out)
}
