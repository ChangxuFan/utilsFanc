reinstall <- function(package = "utilsFanc") {
  pkg.path <- paste0("/bar/cfan/R_packages/", package)
  install.packages(pkg.path, repos = NULL)
  devtools::reload(pkg.path)
}

attach.fanc <- function(pkg) {
  if (pkg == "igraph" && Sys.getenv("RSTUDIO") == "1") {
    library(pkg, lib.loc = "/opt/apps/R/3.5.1/lib/R/library", character.only = T)
    return()
  }
  library(pkg,character.only = T)
}

attach.fanc.2 <- function(pkg) {
  if (pkg == "igraph") {
    if (system("hostname", intern = T) == "stout") {
      library(pkg,character.only = T)
      return()
    }
    library(pkg, lib.loc = paste0("/opt/apps/R/", version$major, ".", version$minor,"/lib/R/library"), character.only = T)
    return()
  }
  library(pkg,character.only = T)
}