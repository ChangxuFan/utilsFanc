# mouse2human <- function(x){
#
#   # require("biomaRt")
#   human = biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#   mouse = biomaRt::useMart("ensembl", dataset = "mmusculus_gene_ensembl")
#
#   genesV2 = biomaRt::getLDS(attributes = c("mgi_symbol"), filters = "mgi_symbol", values = x ,
#                    mart = mouse, attributesL = c("hgnc_symbol"), martL = human, uniqueRows=T)
#   humanx <- unique(genesV2[, 2])
#
#   # Print the first 6 genes found to the screen
#   print(head(humanx))
#   return(humanx)
# }
#
# human2mouse <- function(x){
#
#   # require("biomaRt")
#   human = biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
#   mouse = biomaRt::useMart("ensembl", dataset = "mmusculus_gene_ensembl")
#
#   genesV2 = biomaRt::getLDS(attributes = c("hgnc_symbol"), filters = "hgnc_symbol", values = x ,
#                    mart = human, attributesL = c("mgi_symbol"), martL = mouse, uniqueRows=T)
#
#   humanx <- unique(genesV2[, 2])
#
#   # Print the first 6 genes found to the screen
#   print(head(humanx))
#   return(humanx)
# }

human.mouse.convert <- function(mouse.vec = NULL, human.vec =NULL,
                                mouse.db = "~/R_packages/utilsFanc/objects/biomaRt_mouse.Rds",
                                human.db = "~/R_packages/utilsFanc/objects/biomaRt_human.Rds") {
  if (is.null(mouse.vec) && is.null(human.vec))
    stop ("at least one of mouse.vec or human.vec should be provided")
  if (!is.null(mouse.vec) && !is.null(human.vec))
    stop("please only provide one of mouse.vec and human.vec")
  mouse.db <- readRDS(mouse.db)
  human.db <- readRDS(human.db)

  if (!is.null(mouse.vec)) {
    vec <- mouse.vec
    source <- mouse.db
    source.att <- "mgi_symbol"
    target <- human.db
    target.att <- "hgnc_symbol"
  } else {
    vec <- human.vec
    source <- human.db
    source.att <- "hgnc_symbol"
    target <- mouse.db
    target.att <- "mgi_symbol"
  }
  genesV2 = biomaRt::getLDS(attributes = source.att, filters = source.att, values = vec ,
                            mart = source, attributesL = target.att, martL = target, uniqueRows=T)
  return(unique(genesV2[, 2]))
}
