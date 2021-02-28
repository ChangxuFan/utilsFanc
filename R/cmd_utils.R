cmd.exec.fanc <- function(cmd, stdout.file=NULL, intern, run =T) {
  if (!is.null(stdout.file)) {
    system("mkdir -p " %>% paste0(dirname(stdout.file)))
    cmd <- paste0(cmd, " 1>",stdout.file, " 2>&1")
  }
  cat (cmd)
  cat ("\n")
  if (run == T) {
    res <- system(cmd, intern = intern)
    if (intern == T)
      return(res)
    else {
      if (res !=0 )
        stop("the following command returned non-zero exit code: " %>% paste0(cmd))
      return(res)
    }
  }

  return(cmd)  
}

copy.params <- function(x) {
  params <- x %>% gsub(" ", "", .) %>%  gsub("\\t", "",.)  %>% gsub("\\n", "", .) %>% 
    strsplit(., split = ",") %>% unlist()
  out <- params %>% gsub(" ", "", .) %>% sub("(.+)=(.+)", "\\1 = \\1" ,.) %>% paste0(collapse = ", ")
  return(cat(out))
} 