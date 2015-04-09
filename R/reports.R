# Reports

#' A report of the first neighborhood of an individiual
#' @param name a single or several names
#' @param den an affiliation edge list in the \link{den} format
#' @param the maximum social distance - or highest edge weight - that a person can be from the individuals in name
#' @return a ggplot ego.plot
#' @export

ego.network.two.mode   <- function(name, den, n = Inf){
  ind         <- den$NAME %in% name  
  stopifnot("Wrong name" = any(ind))
  affil        <- as.character(den$AFFILIATION[ind])
  rel.affil    <- den[den$AFFILIATION %in% affil,]
  
  if(n < Inf){
  net.elite    <- elite.network(rel.affil)
  ind.e        <- V(net.elite)$name %in% name
  dist.to.ind  <- net.elite[ind.e,]
  ind.e        <- V(net.elite)$name[dist.to.ind < n]
  rel.affil    <- rel.affil[rel.affil$NAME %in% ind.e,]
  }
  net.two      <- two.mode(rel.affil)  
  type         <- V(net.two)$type
  type[V(net.two)$name %in% name] <- "Ind"
  type <- as.factor(type)
  levels(type)    <- c("Individual", "Ego", "Affiliation")
  
graph.plot.twomode(net.two, text = TRUE, vertex.fill = type) + scale_fill_manual(values = c("black", "slateblue", "white"))
}

#' Ego network
#' 
#' An ego network of individuals
#' @param name
#' @param den
#' @param n
#' @return a ggplot2 plot
#' @export

ego.network      <- function(name, den, n = Inf){
  ind            <- den$NAME %in% name  
  stopifnot("Wrong name" = any(ind))
  affil          <- as.character(den$AFFILIATION[ind])
  rel.affil      <- den[den$AFFILIATION %in% affil,]
  net.elite      <- elite.network(rel.affil)
  ind.e          <- V(net.elite)$name %in% name
  dist.to.ind    <- net.elite[ind.e,]
  delete.them    <- which(dist.to.ind >= n & ind.e == FALSE)
  net.elite      <- delete.vertices(net.elite, delete.them)
  
  ego.name       <- V(net.elite)$name
  ego.name[(V(net.elite)$name %in% name) == FALSE] <- ""
  V(net.elite)$ego.name   <- ego.name
  V(net.elite)$ego.degree <- net.elite[V(net.elite)$name %in% name,]
  V(net.elite)$ego.degree
  net.elite
}

