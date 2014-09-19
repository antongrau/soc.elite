#####################################################################################
## Two-mode network

#' Create a two-mode network from an affiliation list
#' 
#' @param rel an affiliation list
#' @param weighted If TRUE the graph is weighted
#' @param directed If TRUE the graph is directed - does that even make sense?
#' @param ... further arguments are passed on to \link{graph.incidence}
#' @export

two.mode <- function(rel, weighted=TRUE, directed=FALSE, ... ){
  
  netmat           <- droplevels(data.frame(rel$NAME, rel$AFFILIATION))
  colnames(netmat) <- c("navn", "org")
  
  ### Nu laves netværksobjekterne
  tabnet          <- table(netmat)
  tabnet          <- as.matrix(tabnet)
  
  graph.incidence(tabnet, multiple = TRUE, weighted = weighted, directed = directed, ...)
}

###############################################
# Find largest component
# Removes all including and below the cut.off

#' Largest component
#' 
#' Finds the largest component in a network
#' @param graph a \link{igraph} network object
#' @param cut.off is the minimum number of weighted affiliation memberships a component member has to have.
#' @return a graph object
#' @export

largest.component <- function(graph, cut.off = 1){
  
  original.names <- V(graph)$name
  
  ind           <- vector()
  if(inherits(graph, "elite.network")){
    ind           <- c(ind, which(V(graph)$weighted.memberships <= cut.off))
    graph           <- graph - ind
  }
  
  cl            <- clusters(graph)
  ind           <- which(cl$membership != which.max(cl$csize))
  
  ind           <- unique(ind)
  graph.com       <- graph - ind
  graph.com$removed.vertex  <- original.names %in% V(graph.com)$name
  graph.com
}

##########################################################################################
# Tag network

#' Tag network
#' 
#' Creates and plots a tag network from an affilation list
#' @param rel.tag an affiliation list
#' @param plot If TRUE the network is plotted
#' @param link.col is the column name in the affiliation list that is used as the edges
#' @return a graph object or a plot
#' @export

tag.network         <- function(rel.tag, plot = TRUE, link.col = "ORG_NAVN"){
  
  tag.edge1           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG1)
  tag.edge2           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG2)
  tag.edge3           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG3)
  tag.edge4           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG4)
  tag.edge5           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG5)
  tag.edge6           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG6)
  tag.edge7           <- cbind(link.col = subset(rel.tag, select = link.col), TAG = rel.tag$TAG7)
  
  tag.edges           <- as.data.frame(rbind(tag.edge1, tag.edge2, tag.edge3, tag.edge4, tag.edge5, tag.edge6, tag.edge7))
  colnames(tag.edges) <- c("ORG_NAVN", "TAG")
  exclude             <- which(is.na(tag.edges$TAG) | tag.edges$TAG == "" | tag.edges$TAG == " ")
  if(length(exclude) > 0) tag.edges <- tag.edges[-exclude,]
  tabnet              <- table(tag.edges)
  tabnet              <- as.matrix(tabnet)
  adj                 <- t(tabnet)%*%tabnet
  
  net.tag             <- graph.adjacency(adj, mode = "undirected", weighted = TRUE)
  
  if(identical(plot, TRUE)){
    wc                  <- as.factor(fastgreedy.community(net.tag)$membership)
    graph.plot(net.tag, text = TRUE, vertex.size = degree(net.tag), edge.alpha = E(net.tag)$weight, vertex.fill = wc) + scale_size_continuous(range = c(2, 10))
  }else{
    net.tag
  }
}

#########################################################################
### Create adjacency

#' Adjacency matrix for individuals
#' 
#' Create an adjacency matrix from an affilation list
#' @param rel an affiliation list
#' @return a sparse adjacency matrix of individual * individual
#' @export

adj.ind <- function(rel){
  netmat <- data.frame(rel$NAME, rel$AFFILIATION)
  colnames(netmat) <- c("navn", "org")
  
  ### Nu laves netværksobjekterne
  tabnet          <- table(netmat)
  tabnet          <- Matrix(tabnet)
  adj             <- tabnet%*%t(tabnet) # Individ*individ
  return(adj)
}

#' Adjacency matrix for affiliations
#' 
#' Create an adjacency matrix from an affilation list
#' @param rel an affiliation list
#' @return a sparse adjacency matrix of affiliation * affiliation
#' @seealso \link{adj.ind}, \link{two.mode}
#' @export


adj.org <- function(rel){
  netmat <- data.frame(rel$NAME, rel$AFFILIATION)
  colnames(netmat) <- c("navn", "org")
  
  ### Nu laves netværksobjekterne
  tabnet          <- table(netmat)
  tabnet          <- as.matrix(tabnet)
  adj             <- t(tabnet)%*%tabnet # Org*Org
  return(adj)
}
