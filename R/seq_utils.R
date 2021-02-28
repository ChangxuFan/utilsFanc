fasta.wrap.fanc <- function(in.fa, out.fa=NULL) {
  if (is.null(out.fa))
    out.fa <- in.fa
  fa <- seqinr::read.fasta(in.fa)
  headers <- lapply(fa, attr, "Annot") %>% unlist() %>% sub("^>", "", .)
  seqinr::write.fasta(fa, headers, out.fa)
  return(out.fa)
}

