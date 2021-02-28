plot.name.construct <- function(outlist, root.name.internal = NULL, sub.name = NULL) {
  plot.file=outlist$plot.file
  plot.dir=outlist$plot.dir
  root.name.external=outlist$root.name.external
  # root.name.internal=outlist$root.name.internal
  # sub.name=outlist$sub.name
  
  if (!is.null(plot.file))
    return(plot.file)
  if (! is.null(plot.dir) && !is.null(sub.name) ) {
    if (is.null(root.name.external) && is.null(root.name.internal))
      root.name <-""
    else
      root.name <- ifelse(!is.null(root.name.external), root.name.external, root.name.internal)
    system(paste0("mkdir -p ", plot.dir))
    return(paste0(plot.dir, "/", root.name, sub.name))
  }
  stop("more info needed to generate output file!")
}
