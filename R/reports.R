# Reports

#' A report of the first neighborhood of an individiual
#' @return A list of lists - a list of plots, a dataframe with affiliations, a network object
report.two.mode <- function(name, n = Inf){
  data(den)  
  ind       <- den$NAME %in% name  
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

#' Twomode graphs
#' 
#' @param graph
#' @param layout
#' @param vertex.fill
#' @param vertex.size
#' @param edge.color
#' @param edge.alpha
#' @param ...
#' @return a ggplot2 plot
#' @export


graph.plot.twomode <- function(graph, layout = "default",
                               vertex.fill = "type", vertex.size = "degree",
                               edge.color = "edge.betweenness", edge.alpha = "edge.betweenness", ...){
  
  if (is.bipartite(graph)==FALSE) stop("Graph is not a two-mode network")
  
  scale.adjustments <- list()
  
  if (identical(vertex.fill, "type")){
    vertex.fill            <- as.factor(V(graph)$type)
    levels(vertex.fill)    <- c("Individual", "Affiliation")
    scale.adjustments$fill <- scale_fill_manual(values = c("black", "white"), name = "Type")
  }
  
  if (identical(edge.color, "edge.betweenness")){
    edge.color              <- edge.betweenness(graph)
    scale.adjustments$color <- scale_color_continuous(high = "darkblue", low = "papayawhip", name = "Edge betweeness")
  }
  
  if (identical(edge.alpha, "edge.betweenness")){
    edge.alpha              <- edge.betweenness(graph)
    scale.adjustments$alpha <- scale_alpha_continuous(range = c(0.3, 1))
  }
    
  if (identical(vertex.size, "degree")){
  vertex.size <- vector(length=vcount(graph))
  vertex.size[V(graph)$type == FALSE] <- degree(bipartite.projection(graph)$proj1)
  vertex.size[V(graph)$type == TRUE]  <- degree(bipartite.projection(graph)$proj2)
  }
    
  
  graph.plot(graph, vertex.fill = vertex.fill, vertex.size = vertex.size,
             edge.alpha = edge.alpha, edge.color = edge.color, ...) + scale.adjustments
}
