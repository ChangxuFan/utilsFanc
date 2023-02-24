captilize.first.letter <- function(x) {
  y <- substr(x, 2, nchar(x))
  y <- paste0(toupper(substr(x, 1, 1)),y)
  return(y)
}
