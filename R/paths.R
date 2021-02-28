path.sync.fanc <- function(path.file = "/bar/cfan/PATHs/login.path") {
  
  path <- readChar(path.file, nchars = file.size(path.file))
  Sys.setenv(PATH = path)
}
