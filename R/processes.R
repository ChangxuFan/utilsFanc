t.stat <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(paste0(Sys.time(), " : ", msg, "\n"))
}

safelapply <- function (..., threads = 1, preschedule = FALSE, stop = T) {
    if (tolower(.Platform$OS.type) == "windows") {
        threads <- 1
    }
    if (threads > 1) {
        o <- mclapply(..., mc.cores = threads, mc.preschedule = preschedule)
        errorMsg <- list()
        for (i in seq_along(o)) {
            if (inherits(o[[i]], "try-error")) {
                capOut <- utils::capture.output(o[[i]])
                capOut <- capOut[!grepl("attr\\(\\,|try-error",
                  capOut)]
                capOut <- head(capOut, 10)
                capOut <- unlist(lapply(capOut, function(x) substr(x,
                  1, 250)))
                capOut <- paste0("\t", capOut)
                errorMsg[[length(errorMsg) + 1]] <- paste0(c(paste0("Error Found Iteration ",
                  i, " : "), capOut), "\n")
            }
        }
        if (length(errorMsg) != 0) {
            errorMsg <- unlist(errorMsg)
            errorMsg <- head(errorMsg, 50)
            errorMsg[1] <- paste0("\n", errorMsg[1])
            if (stop == T)
                stop(errorMsg)
            else
                Warning(errorMsg)
        }
    }
    else {
        o <- lapply(...)
    }
    o
}

rm.except <- function(obj.vec) {
    stop("doesn't work. wrong environment")
    objs <- ls()
    objs <- objs %>% .[!. %in% obj.vec]
    rm(list = objs)
    return()
}

