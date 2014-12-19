###############################################################################
######## Analysis
#' Find the core in an elite network
#' 
#' Uses the k-core decomposition, see \link{graph.adjacency}, to identify the elite
#' @param sp a shortest paths matrix
#' @param reach the maximum distance considered as a relation in the decompostition
#' @return a numerical vector with the coreness score for each vertex
#' @export
#' @examples
#' data(den)
#' health.affil  <- has.tags(den, c("Health"))
#' den.health    <- den[den$AFFILIATION %in% health.affil,]
#' adj.health    <- adj.ind(den.health)
#' net.health    <- graph.adjacency(adj.health)
#' net.health    <- graph.adjacency(adj.health, mode = "undirected", weighted = TRUE)
#' sp.health     <- shortest.paths(net.health)
#' core.health   <- find.core(sp.health)
#' table(core.health)

find.core <- function(sp, reach = 2.1){
  sp.1     <- (sp <= reach) * 1
  net.sp   <- graph.adjacency(sp.1, mode="undirected", diag=FALSE, weighted=TRUE)
  core     <- graph.coreness(net.sp)
  core
}

#' Find the core in an elite network - using a graph as input
#' 
#' Uses the k-core decomposition, see \link{graph.adjacency}, to identify the elite
#' @param net an \link{igraph} network object
#' @param reach the maximum distance considered as a relation in the decompostition
#' @return a numerical vector with the coreness score for each vertex
#' @export
find.core.net <- function(net, reach = 2.1){
  sp       <- shortest.paths(net)
  sp       <- (sp <= reach) * 1
  net.sp   <- graph.adjacency(sp, mode="undirected", diag=FALSE, weighted=TRUE)
  core     <- graph.coreness(net.sp)
  core
}

#' Elite network
#'
#' Construct a weighted elite network
#' @param rel.all an affiliation edge list
#' @param sigma the number of members in an affiliation above which all affiliations are weighted down
#' @return a elite network object
#' @export

elite.network     <- function(rel.all = rel.all, sigma = 14){
  
  ## Vægt baseret på størrelse af org
  netmat              <- droplevels(data.frame(rel.all$NAME, rel.all$AFFILIATION))
  colnames(netmat)    <- c("navn", "org")
  tabnet              <- Matrix(table(netmat), sparse=TRUE)
  org.medlemmer       <- colSums(tabnet)
  medlemskaber        <- rowSums(tabnet)
  
  # Occassions weight
  col.max             <- apply(tabnet, 2, max)
  tabweight           <- t(t(tabnet) * (1 / col.max))
  dimnames(tabweight) <- dimnames(tabnet)
  
  # Org size weight
  org.weight           <- sqrt((sigma/org.medlemmer))
  org.weight[org.weight > 1]      <- 1
  names(org.weight)    <- colnames(tabnet)
  
  # Tildel en vægt til rel.all
  tb                   <- t(tabweight) * org.weight
  tb                   <- t(tb)
  cs                   <- colSums(tb)
  
  # Adjacency matrix for individer
  tb                   <- Matrix(tb, sparse=TRUE)
  adj.all              <- sqrt(tb) %*% sqrt(t(tb))
  
  antal.medlemskaber   <- diag(adj.all)
  
  ## Indlejrede
  org.1a               <- nested$Nested.org
  org.2a               <- nested$Nested.in
  org.navne            <- colnames(tb)
  org.1                <- org.1a[org.1a %in% org.navne & org.2a %in% org.navne]
  org.2                <- org.2a[org.1a %in% org.navne & org.2a %in% org.navne]
  
  for (i in 1:length(org.1)){
    ret.org.1         <- which(colnames(tb) == org.1[i])
    ret.org.2         <- which(colnames(tb) == org.2[i])
    if (length(ret.org.1) > 0 & length(ret.org.2) >0){
      ret.org.1.navn    <- org.navne[ret.org.1]
      ret.org.2.navn    <- org.navne[ret.org.2]
      
      ret.rel.1         <- tb[, ret.org.1]
      ret.rel.2         <- tb[, ret.org.2]
      ret.mem.1         <- which(ret.rel.1 > 0)
      ret.mem.2         <- which(ret.rel.2 > 0)
      ret.mem           <- intersect(ret.mem.1, ret.mem.2)
      
      ret.vaegt.1        <- org.weight[ret.org.1]
      ret.vaegt.2        <- org.weight[ret.org.2]
      
      adj.all[ret.mem, ret.mem] <- adj.all[ret.mem, ret.mem] - ret.vaegt.2
    }
  }
  
  ## Netværksobjektet skabes
  net.all             <- graph.adjacency(adj.all, weighted = TRUE, diag = FALSE, mode = "undirected")
  
  E(net.all)$weight.nolog        <- E(net.all)$weight
  over                           <- E(net.all)$weight > 1
  E(net.all)$weight[over]        <- log(E(net.all)$weight[over]) + 1
  E(net.all)$weight              <- 1/E(net.all)$weight
  
  V(net.all)$weighted.memberships <- antal.medlemskaber
  V(net.all)$memberships          <- medlemskaber
  net.all$org.weight              <- org.weight
  net.all$org.members             <- org.medlemmer
  
  class(net.all) <- c("igraph", "elite.network")
  net.all
}

#' Elite network for affiliations
#'
#' Construct a weighted elite network of affiliations
#' @param rel.all an affiliation edge list
#' @param sigma the number of members in an affiliation above which all affiliations are weighted down
#' @return a elite network object
#' @export

elite.network.org     <- function(rel.all = rel.all, sigma = 14){
  
  ## Vægt baseret på størrelse af org
  netmat              <- droplevels(data.frame(rel.all$NAME, rel.all$AFFILIATION))
  colnames(netmat)    <- c("navn", "org")
  tabnet              <- Matrix(table(netmat), sparse = TRUE)
  
  # Occassions weight
  col.max             <- apply(tabnet, 2, max)
  tabnet              <- t(t(tabnet) * (1 / col.max))
  dimnames(tabnet)    <- dimnames(tabnet)
  
  adj.org             <- t(tabnet) %*% tabnet
  org.medlemmer       <- diag(adj.org)
  
  # Org size weight
  org.weight           <- sqrt((sigma/org.medlemmer))
  org.weight[org.weight > 1]      <- 1
  names(org.weight)    <- colnames(tabnet)
  
  adj.org             <- adj.org * org.weight
  net.org             <- graph.adjacency(adj.org, weighted = TRUE, diag = FALSE, mode = "directed")
  V(net.org)$members  <- org.medlemmer
  V(net.org)$weighted.members <- diag(adj.org)
  
  over                <- E(net.org)$weight > 1
  #E(net.org)$weight[over] <- log(E(net.org)$weight[over]) + 1
  E(net.org)$weight   <- 1/E(net.org)$weight
  
  net.org
}

##########################################################
## Secondary actors

#' Secondary actors
#' 
#' Identify secondary actors within a group. A secondary actor is an individual with a neighborhood that is perfectly nested within another individuals neighborhood.
#' Here it is identified by comparing memberships between all agents within a group. If any individual has the exact same memberships as another individual he is considered a secondary actor.
#' @param x a named core numerical vector with coreness values, see \link{graph.coreness}
#' @param rel.all an affiliation edge list
#' @return something
#' @export
secondary.actors <- function(x, rel.all){
  
  mem        <- names(x)[x == max(x)]
  rel.x      <- droplevels(rel.all[rel.all$NAME %in% mem,])
  
  affil      <- table(rel.x$NAME, rel.x$AFFILIATION)
  affil      <- affil > 0
  mem.list   <- apply(affil, 1, which)
  lengths    <- sapply(mem.list, length)
  overlap    <- function(x, y) length(intersect(x,y)) == length(x)
  secondary  <- vector(length = length(mem.list))
  
  for (i in 1:length(mem.list)){
    ov         <- which(sapply(mem.list, overlap, x = mem.list[[i]]))
    if(length(ov) > 1)  secondary[i] <- paste(unique(c(mem[i], mem[ov])), collapse="|")
  }
  secondary
}

#' Network by variable
#' 
#' Splits a network by a variable and returns matrix of descriptive values
#' @param graph is a \link{igraph} network
#' @param variabel is a factor of the same length and order as the vertices in graph
#' @return a matrix with descriptives
#' @export

network.by.variable <- function(graph, variabel){
  variabel <- as.factor(variabel)
  dele <- levels(variabel)
  output <- matrix(nrow=20, ncol=length(dele)) # Output matrix
  for ( i in 1:length(dele)){
    del <- dele[i]
    del.ind <- which(variabel==del)
    del.not <- which(variabel!=del)
    graph.del             <- graph - del.not
    
    # Antal Vertices
    Number.of.vertices  <- length(del.ind)
    # Antal edges
    Number.of.edges     <- sum(degree(graph)[del.ind])
    # Average degree
    Average.degree      <- round(Number.of.edges/Number.of.vertices, 1)
    # Part density i 1000
    Part.density        <- round(Number.of.edges/((Number.of.vertices*(vcount(graph)-1)/2))*1000, 1)
    # Clusters in part
    Number.of.clusters.in.del  <- clusters(graph.del)$no
    
    # Average path length total network
    sp                  <- shortest.paths(graph)
    ind.av.sp           <- rowSums(sp)[del.ind]/ncol(sp)
    Average.path.length <- round(sum(ind.av.sp)/length(del.ind),1)
    # Average path length within group
    sp.del                  <- shortest.paths(graph)[del.ind,del.ind]
    ind.av.sp.del           <- rowSums(sp.del)/length(del.ind)
    Average.path.length.del <- round(sum(ind.av.sp.del)/length(del.ind),1)
    
    # Longest path within group
    Longest.path.del    <- max(sp.del)
    
    # Largest number of degrees
    Largest.degree <- max(degree(graph)[del.ind])
    # Largest degree in part
    Largest.degree.del <-max(degree(graph.del))
    # Largest 2 neighborhoods
    Largest.2.neighborhood <- max(neighborhood.size(graph, 2)[del.ind])
    # Largest 3 neighborhoods
    Largest.3.neighborhood <- max(neighborhood.size(graph, 3)[del.ind])
    
    # Average closeness whole network * 10000
    Average.closeness.network    <- round(sum(closeness(graph)[del.ind])/length(del.ind) * 10000, 1)
    # Average closeness part
    Average.closeness.part       <- round(sum(closeness(graph.del))/length(del.ind) * 10000, 1)
    # Average betweenness whole network
    Average.betweenness.network  <- round(sum(betweenness(graph)[del.ind])/length(del.ind))
    # Average betweeness part
    Average.betweenness.part     <- round(sum(betweenness(graph.del))/length(del.ind))
    # Maximum betweeness whole network
    Maximum.betweenness          <- max(betweenness(graph)[del.ind])
    # Maximum closeness whole network * 10000
    Maximum.closeness            <- round(max(closeness(graph)[del.ind]) * 10000, 1)
    # Average eigenvector centrality * 1000
    Average.eigen.network        <- round(sum(evcent(graph)$vector[del.ind])/length(del.ind) * 1000, 1)
    # Maximum eigenvector centrality
    Maximum.eigen                <- round(max(evcent(graph)$vector[del.ind])* 1000, 1)
    
    del.stat <- c(Number.of.vertices, Number.of.edges, Average.degree, Part.density, Number.of.clusters.in.del,
                  Average.path.length, Average.path.length.del, Longest.path.del, Largest.degree, Largest.degree.del,
                  Largest.2.neighborhood, Largest.3.neighborhood,
                  Average.closeness.network, Average.closeness.part, Maximum.closeness,
                  Average.betweenness.network, Average.betweenness.part, Maximum.betweenness,
                  Average.eigen.network, Maximum.eigen)
    
    
    output[,i] <- round(del.stat, 1)
  }
  colnames(output) <- dele
  rownames(output) <- c("Number of vertices", "Number of edges", "Average degree", "Part density (o/oo)", "Number of clusters in part",
                        "Average path length", "Average path length in part", "Longest path in part", "Highest degree", "Highest degree in part",
                        "Largest 2. neighborhood", "Largest 3. neighborhood",
                        "Average closeness", "Average closeness in part", "Maximum closeness",
                        "Average betweeness", "Average betweenness in part", "Maximum betweenness",
                        "Average eigencentrality", "Maximum eigencentrality")
  return(output)
}
############# Endnu en beskrivende funktion

describe.network <- function(graph, variabel, org.data){
  
  #ALLE
  between       <- betweenness(graph)
  neighborhood.size.3 <- neighborhood.size(graph, 3)
  degrees       <- degree(graph)
  core.com      <- clusters(graph) 
  core.com.mem  <-  core.com$membership==which.max(core.com$csize)
  
  nvertex.all                 <- vcount(graph)
  nedges.all                  <- ecount(graph)
  percentage.in.largest.com   <- sum(core.com.mem)/nvertex.all * 100
  Average.degree              <- sum(degrees)/nvertex.all
  Average.betweenness         <- sum(between)/nvertex.all
  Average.3.neighborhood.size <- sum(neighborhood.size.3)/nvertex.all
  
  result.matrix <- as.data.frame(matrix(nrow = 6, ncol=1+nlevels(variabel)))
  res.all       <- c(nvertex.all, nedges.all, percentage.in.largest.com, Average.degree, Average.betweenness, Average.3.neighborhood.size)
  result.matrix[,1] <- res.all
  
  levels.variabel <- levels(variabel)
  
  # Del
  for( i in 1:nlevels(variabel)){
    graph.part               <- graph - which(variabel!=levels.variabel[i])
    part.ind                 <- which(variabel==levels.variabel[i]) 
    between.part             <- between[part.ind]
    neighborhood.size.3.part <- neighborhood.size.3[part.ind]
    degrees.part             <- degrees[part.ind]
    core.com.mem.part        <- core.com.mem[part.ind]
    
    nvertex.part                     <- vcount(graph.part)
    nedges.part                      <- ecount(graph.part)
    percentage.in.largest.com.part   <- sum(core.com.mem.part)/nvertex.part * 100
    Average.degree.part              <- sum(degrees.part)/nvertex.part
    Average.betweenness.part         <- sum(between.part)/nvertex.part
    Average.3.neighborhood.size.part <- sum(neighborhood.size.3.part)/nvertex.part
    
    res.part       <- c(nvertex.part, nedges.part, percentage.in.largest.com.part, Average.degree.part, Average.betweenness.part, Average.3.neighborhood.size.part)
    result.matrix[,i+1] <- res.part  
  }  
  
  colnames(result.matrix) <- c("All", levels.variabel)
  rownames(result.matrix) <- c("Corporations", "Ties", "% in central component", "Average degree", "Average betweeness", "Average 3rd neighborhoodsize")
  
  round(result.matrix, 1)
}



######## Overlapping Social Circles by Alba and Kadushin
# Der er stadig noget bøvl med at trække 1 fra de overlappende hoods

#' Social proximity
#' 
#' Calculates the social proximity of all vertices in a graph as described by Alba and Kadushin
#' @param graph is a \link{igraph} network
#' @param neihborhood a numerical value indicating the order of the neighborhood, see \link{neighborhood}
#' @param mode if "total" the proximity is calculated on the size of the combined neighborhood. If "own" or "other" proximity is calculated on the basis of either of the vertices in a relation.
#' @return a matrix with proximity measures



proximity <- function(graph, neighborhood = 2, mode = "total"){
  n2 <- neighborhood(graph, order=neighborhood)
  
  ###
  individual.hoodoverlap <- function(n2, individual, result=1){
    hood <- n2[[individual]]
    res <- vector(length=length(n2))
    for (j in 1:length(n2)){
      hood2 <- n2[[j]]
      # Andel af egne forbindelser man deler med hood2  
      hood.size          <- length(hood) #-1
      hood2.size         <- length(hood2) #-1
      hood.overlap       <- sum(hood %in% hood2) - sum(hood2 == j)
      hood.total.size    <- hood.size + hood2.size - hood.overlap # NB er det her korrekt!
      
      
      overlap.total      <- hood.overlap/hood.total.size
      overlap.own        <- hood.overlap/hood.size 
      overlap.other      <- hood.overlap/hood2.size
      ind.res <- c(overlap.total, overlap.own, overlap.other, hood.total.size, hood.overlap)
      
      res[j]       <- ind.res[result]
    }
    return(res)
  }
  
  ############# Resultater
  if (identical(mode, "total")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=1)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  if (identical(mode, "own")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=2)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  if (identical(mode, "other")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=3)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  if (identical(mode, "overlap")==TRUE){
    circle.mat <- matrix(nrow=length(n2), ncol=length(n2))
    
    pb <- txtProgressBar(min = 0, max = length(n2), style=3)
    
    for (i in 1:length(n2)){
      circle.mat[,i] <- individual.hoodoverlap(n2, i, result=5)
      setTxtProgressBar(pb, i, label=paste( round(i/length(n2)*100, 0), "% ready!"))
    }
    close(pb)
  }
  
  rownames(circle.mat) <- V(graph)$name
  colnames(circle.mat) <- V(graph)$name
  
  return(circle.mat)
}

####################### Describe vertex

# Den her funktion skal laves om - den skal hente resultater ind fra nogle allerede eksisterende analyser

#' Who is it?
#' 
#' Affiliation and descriptives for an individual
#' @param net a \link{igraph} network object
#' @param name the name of the individual
#' @param relation.matrix a affiliation edge list
#' @param vertex the index number of the individual
#' @export

who  <-  function(net, name=NULL, relation.matrix=rel, vertex=NULL){
  
  
  ## Finding the name and vertex number
  if( identical(name, NULL))   name    <- V(net)$name[vertex]
  if( identical(vertex, NULL)) vertex  <- which(V(net)$name == name)
  
  # Number of degrees
  deg     <- degree(net)[vertex]
  # Betweenness
  between <- round(betweenness(net))
  between.vertex <- between[vertex]
  between.rank   <- which(order(between, decreasing=TRUE)==vertex)
  # 2nd Neighborhood
  n2      <- neighborhood.size(net, 2)[vertex]
  # Closeness
  close          <- closeness(net)
  close.vertex   <- close[vertex]
  # Closeness rank
  close.rank     <- which(order(close, decreasing=TRUE)==vertex)
  
  ###### Memberships
  
  medlemskaber <- as.character(relation.matrix$AFFILIATION[relation.matrix$NAVN == name])
  positioner   <- as.character(relation.matrix$POSITION[relation.matrix$NAVN == name])
  mem <- medlemskaber
  positioner[positioner == ""] <- "Medlem"
  mem <- paste(positioner, ": ", mem)
  mem <-  mem[order(positioner, decreasing=FALSE)]  
  
  cat( "Name: ", name, "\n")  
  cat( "Degrees: ", deg, "\n")
  cat( "2nd Neighborhood: ", n2, "\n")
  cat( "Betweenness: ", between.vertex, "\n")
  cat( "Betweenness rank: ", between.rank, "\n")
  cat( "Closeness: ", close.vertex, "\n")
  cat( "Closeness rank: ", close.rank, "\n")
  cat( "Memberships: ", "\n")
  print(noquote(as.matrix(mem)))
}

#' Hvem er det?
#' 
#' Returns the affiliation memberships of an individual, or all direct contacts
#' @param name the name of the individual
#' @param relation.matrix a affiliation edge list
#' @param memberships if FALSE returns all direct contacts
#' @export

hvem  <-  function(name=NULL, relation.matrix=rel.all, memberships = TRUE){
  
  ## Finding the name and vertex number
  medlemskaber            <- as.character(relation.matrix$AFFILIATION[relation.matrix$NAME == name])
  rels                    <- relation.matrix[relation.matrix$AFFILIATION %in% medlemskaber,]
  out                     <- cbind(rels$NAME, rels$AFFILIATION)
  
  if (memberships == FALSE){ 
    out <- out[order(rels$AFFILIATION),]
    out[is.na(out)] <- ""
    cat( "Name: ", name, "\n")  
    print(noquote(as.matrix(out)))
  }else{
    unique(as.matrix(rels$AFFILIATION))
  }
}

#' Combine descriptions
#' 
#' Combine all descriptions into a single character vector
#' @param x the name of an individual
#' @param rel.all an affiliation edge list
#' @return a character vector

beskrivelser <- function(x, rel.all = rel.all){
  besk       <- rel.all$DESCRIPTION[rel.all$NAME == x]
  org        <- rel.all$AFFILIATION[rel.all$NAME == x]
  paste(org, ":", besk)
}

#' Search descriptions
#' 
#' Find specifik search terms in all descriptions
#' @param rel a affiliation edgelist
#' @param soegeord a character vector of search terms
#' @param ignore.case if TRUE the search is not case sensitive
#' @param ... further arguments are passed on to \link{grep}.
#' @return a affiliation edgelist
#' @export

find.beskrivelse <- function(rel, soegeord, ignore.case=TRUE, ...){
  
  beskrivelse <- as.character(rel$DESCRIPTION)
  if(ignore.case==TRUE) beskrivelse <- tolower(beskrivelse)
  
  grep.soeg  <- paste(soegeord, collapse="|")
  grep.fund <- grep(grep.soeg, beskrivelse, ignore.case=ignore.case, ...)  
  # grep.fund <- grep(grep.soeg, beskrivelse, ignore.case=ignore.case)  
  navne.fund <- levels(as.factor(rel$NAME[grep.fund]))
  navne.ind  <- which(rel$NAME %in% navne.fund)
  
  droplevels(rel[navne.ind,])
}



#' Find the gender by name
#' 
#' Guesses the gender for a list of names by comparing it to the national distribution of first names.
#' @param navne a character vector of full names
#' @param names.gender a matrix with national distributions of first names
#' @return a factor with a gender guess
#' @export

find.gender <- function(navne){
  names.gender <- soc.elite:::names.gender
  n.list <- strsplit(navne, " ")
  fornavne <- vector(length=length(navne))
  for (i in 1:length(fornavne)){
    fornavne[i] <- n.list[[i]][1]
  }
  fornavne <- toupper(fornavne)
  
  ng <- names.gender[names.gender$Navn %in% fornavne,]
  
  koen   <- vector(length=length(navne))
  for (i in 1:length(navne)){
    n <- fornavne[i]
    koen.navn <- as.numeric(ng[match(n, ng$Navn),][5])
    koen[i]     <- koen.navn
  }
  b <- c(0, 0.2, 0.8, 1)
  kategori <- cut(koen, b, include.lowest=TRUE, labels=c("Women", "Binominal", "Men"))
  return(kategori)
}

#' Extract first names
#' 
#' Extract first names from full names
#' @param navne a character vector of full nmaes
#' @return a character vector of first names
#' @export

fornavne      <- function(navne){
  navne           <- as.character(navne)
  n.list          <- strsplit(navne, " ")
  fornavne        <- vector(length=length(navne))
  for (i in 1:length(fornavne)){
    fornavne[i] <- n.list[[i]][1]
  }
  fornavne  
}

#' Extract last names
#' 
#' Extract last names from full names
#' @param navne a character vector of full nmaes
#' @return a character vector of last names
#' @export

efternavne    <- function(navne){
  navne           <- as.character(navne)
  n.list          <- strsplit(navne, " ")
  efternavne      <- vector(length=length(navne))
  for (i in 1:length(efternavne)){
    efternavne[i] <- tail(n.list[[i]], 1)
  }
  efternavne  
}

#' Categories from postal codes
#' 
#' @param x a numeric vector with 4 digit danish postal codes
#' @return a data.frame with various factors
#' @export

inddel.postnummer <- function(x){
  postnumre <- postnumre[duplicated(postnumre$POSTNR)==FALSE,]
  jx <- data.frame(POSTNR = x)
  xm <- join(jx, postnumre, by = "POSTNR")
  xm
}



#' Create an organisation variable from relations matrix 
#' 
#' @param rel is a relations matrix - or a affiliation edge list
#' @param net is an igraph network object
#' @param var is a variable of the same length and order as rel - often it would be "SOURCE" or "TAG"
#' @return a character vector
#' @export

variable.from.rel.org  <- function(rel, net, var){
  
  un.org.navn  <- rel$AFFILIATION[duplicated(rel$AFFILIATION) == FALSE]
  un.var       <- var[duplicated(rel$AFFILIATION) == FALSE]
  order.navn   <- order(un.org.navn)
  
  stopifnot(all.equal(V(net)$name, un.org.navn[order.navn]))
  un.var[order.navn]
}

#' Vertex descriptives
#' 
#' Descriptive statistics for each vertex
#' @param net is an \link{igraph} network object
#' @param reach is the maximum distance between two individuals for the reach statistic
#' @return a matrix with a lot of descriptives
#' @export

vertex.measures <- function(net, reach = 2.1){
  
  sp                <- shortest.paths(net)
  av.path.length    <- rowSums(sp) / nrow(sp)
  l                 <- vcount(net) + 1
  deg               <- rowSums(sp <= 1)
  close.w           <- closeness(net)
  close.rw          <- l - rank(close.w)
  between.w         <- round(betweenness(net), 1)
  between.rw        <- l - rank(between.w)
  n2.uw             <- neighborhood.size(net, 2)
  n2.w              <- rowSums(sp <= reach)
  n2.rw             <- l - rank(n2.w)
  constraint        <- round(constraint(net) * 1000)
  
  out               <- data.frame(deg, n2.uw, n2.w, n2.rw, close.rw, between.w, between.rw, av.path.length, constraint)
  colnames(out)     <- c("Degree", "Unweighted reach", "Reach", "Reach rank", "Closeness rank", "Betweenness", "Betweenness rank", "Average path length", "Burts Constraint")
  out
}

#' Vertex descriptives for directed graphs
#' 
#' Descriptive statistics for each vertex
#' @param net is an directed \link{igraph} network object
#' @param reach is the maximum distance between two individuals for the reach statistic
#' @return a matrix with a lot of descriptives
#' @export


vertex.measures.directed <- function(net, n = 2.5){
  
  sp.in               <- shortest.paths(net, mode = "in")
  sp.out              <- shortest.paths(net, mode = "out")
  av.path.length      <- rowSums(sp.in) / nrow(sp.in)
  l                 <- vcount(net) + 1
  deg.in            <- rowSums(sp.in <= 1)
  deg.out           <- rowSums(sp.out <= 1)
  deg.all           <- (deg.in + deg.out)/2
  close.in          <- closeness(net, mode = "in")
  close.out         <- closeness(net, mode = "out")
  close.rin         <- l - rank(close.in)
  close.rout        <- l - rank(close.out)
  between.w         <- round(betweenness(net), 1)
  between.rw        <- l - rank(between.w)
  n2.in             <- rowSums(sp.in <= n)
  n2.out            <- rowSums(sp.out <= n)
  n2.rin            <- l - rank(n2.in)
  n2.rout           <- l - rank(n2.out)
  page.rank         <- round(page.rank(net)$vector * 1000)
  
  out               <- data.frame(deg.in, deg.out, deg.all, n2.in, n2.out, n2.rin, n2.rout,
                                  close.rin, close.rout, between.w, between.rw, av.path.length, page.rank)
  colnames(out)     <- c("In degree", "Out degree", "All degrees", "Reach in", "Reach out", "Reach in rank", "Reach out rank", 
                         "Closeness in rank", "Closeness out rank", "Betweenness", "Betweenness rank", "Average path length", "Pagerank")
  out
}


