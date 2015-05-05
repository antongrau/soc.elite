#####################################################################################
## Two-mode network

#' Create a two-mode network from an affiliation list
#' 
#' @param rel an affiliation list
#' @param weighted If TRUE the graph is weighted
#' @param directed If TRUE the graph is directed - does that even make sense?
#' @param ... further arguments are passed on to \link{graph.incidence}
#' @export

two.mode <- function(rel, weighted = TRUE, directed = FALSE, ... ){
  
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

#' Has.tags
#' 
#' Selects all affiliations that have one or several tags
#' @param x an affiliation edge list
#' @param tags is a character vector of tags
#' @param res if "affiliations" \code{has.tags} returns a character vector with unique affiliation names. If res is "names", a character vector of unique names is returned. If res is "relations", a subset of x is returned.
#' @param silent if TRUE the table with the number of matched positions per tag is not shown.
#' @export
#' @examples
#' data(den)
#' has.tags(den, tags = c("Youth", "Children"))
#' has.tags(den, tags = c("Youth", "Children"), res = "names")
#' has.tags(den, tags = c("Youth", "Children"), res = "relations")

has.tags       <- function(x, tags, res = "affiliations", silent = FALSE){
  
  org.navn     <- as.character(x$AFFILIATION)
  sector.match <- sapply(tags, grepl, x = x$TAGS)
  tag.orgs     <- unique(org.navn[which(rowSums(sector.match) >= 1)])
  
  if (identical(silent, FALSE)){
  how.many.per.tag <- cbind("Matched positions" = colSums(sector.match))
  print(how.many.per.tag)
  cat("\n", "\n")
  }
  
if(res == "affiliations"){
  return(as.character(tag.orgs))
}

if(res == "names"){
  return(as.character(unique(x$NAME[x$AFFILIATION %in% tag.orgs])))
}

if(res == "relations"){
  droplevels(x[x$AFFILIATION %in% tag.orgs,])
}
}

#' show.all.tags
#' 
#' Displays a matrix with all the tags in den.
#' 
#' @param den a affiliation edgelist
#' @export
#' @examples
#' data(den)
#' show.all.tags(den)

show.all.tags   <- function(den){
  tags                <- as.character(den$TAGS)
  tags.positions      <- table(unlist(strsplit(tags, ", ")))
  tags.affiliations   <- tags[duplicated(den$AFFILIATION) == FALSE]
  tags.affiliations   <- table(unlist(strsplit(tags.affiliations, ", ")))
  cbind(Positions = tags.positions, Affiliations = tags.affiliations)
}




##########################################################################################
# Tag network

#' Tag network
#' 
#' Creates and plots a tag network from an affilation list
#' @param den an affiliation list
#' @param plot If TRUE the network is plotted
#' @return a graph object or a plot
#' @export

tag.network         <- function(den, plot = TRUE){
  
  den.unique          <- den[duplicated(den$AFFILIATION)==FALSE,]
  split.den           <- split(x = den.unique, f = den.unique$AFFILIATION)
  
  affil.to.tag.net     <- function(x){
    tag.vector         <- unlist(strsplit(as.character(x$TAGS), ", "))
    affiliation.vector <- rep(as.character(x$AFFILIATION), times = length(tag.vector))
    cbind(AFFILIATION = affiliation.vector , TAG = tag.vector)
  }
  
  split.tag.edges     <- lapply(split.den, affil.to.tag.net)
  tag.edges           <- do.call(rbind, split.tag.edges)
  tag.incidence       <- Matrix(table(tag.edges[,1], tag.edges[,2]))
  net.tag             <- graph.incidence(tag.incidence)
    
  if(identical(plot, TRUE)){
    net.tag             <- bipartite.projection(net.tag)$proj2
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
  netmat <- droplevels(data.frame(rel$NAME, rel$AFFILIATION))
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
  netmat <- droplevels(data.frame(rel$NAME, rel$AFFILIATION))
  colnames(netmat) <- c("navn", "org")
  
  ### Nu laves netværksobjekterne
  tabnet          <- table(netmat)
  tabnet          <- as.matrix(tabnet)
  adj             <- t(tabnet)%*%tabnet # Org*Org
  return(adj)
}

#' Create a den object from the elite database
#' 
#' Something something
#' @export

eliteDB.connections <- function(){
  # Måske skal der laves noget datarens - tjek fx for tomme navne og NA.
  # Det ville nok også være sundt med nogle automatiske tests
  # Det ser ud til at ikke alle affiliation ids kan findes i connections 
  elite.db.connections            <- fromJSON("http://elitedb.ogtal.dk/exporter.php?type=connections")
  elite.db.persons                <- fromJSON("http://elitedb.ogtal.dk/exporter.php?type=persons")
  elite.db.affil                  <- fromJSON("http://elitedb.ogtal.dk/exporter.php?type=affiliations")
  connections                     <- elite.db.connections[order(elite.db.connections$affiliation_id),]
  persons                         <- elite.db.persons[order(elite.db.persons$id),]
  affiliations                    <- elite.db.affil[order(elite.db.affil$id),]
  affiliations$id.x               <- affiliations$id
  
  persons$person_id               <- persons$id
  
  # Data rens
  persons                         <- persons[is.na(persons$fullname)==FALSE,]
  
  # Navne dupletter
  dup.navn                       <- persons$fullname[duplicated(persons$fullname)]
  dup.id                         <- persons$person_id[duplicated(persons$fullname)]
  persons$fullname_dup           <- persons$fullname
  persons$fullname_dup[duplicated(persons$fullname)]           <- paste(dup.navn, dup.id)
  
  # Merge
  
  connections                     <- merge(connections, persons, by = "person_id", all.x = T, sort = TRUE)
  connections                     <- merge(connections, affiliations, by.x = "affiliationname", by.y = "name", all.x = T, sort = TRUE)
  
  # Merge
  gender                          <- find.gender(navne = connections$fullname)
  levels(gender)                  <- c("Women", "Undefined", "Men")
  
  
  connections.den                 <- data.frame(NAME        = connections$fullname,
                                                AFFILIATION = connections$affiliationname,
                                                ROLE        = connections$rolename,
                                                GENDER      = gender,
                                                DESCRIPTION = connections$description.x,
                                                SOURCE      = connections$affiliationsector,
                                                BIQ_LINK    = connections$biq,
                                                CVR         = connections$cvr,
                                                TAGS        = connections$tagnames,
                                                MODIFIED    = connections$modified_date.y,
                                                CREATED     = connections$created_date
                                                )
  connections.den
}

