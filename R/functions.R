#### Add edges to soc.ca object
# now it only works for active maps

add.edges.to.soc.ca <- function(object, graph, p=map.active(object, dim=dim), dim=1:2, edge.alpha=E(graph)$weight, edge.color="red", modalities=1:object$n.mod){
  
  coord <- object$coord.mod[modalities,dim]
  ec    <- edge.coord(graph, coord)
  ec    <- data.frame(ec, ea=edge.alpha) 
  
  p + geom_segment(aes(x=start.x, y=start.y, xend=slut.x, yend = slut.y, alpha=ea, linetype=as.character(edge.line)), data=ec, size=edge.size, color=edge.color)  
  #return(colnames(ec))#
}

#################################################################
# ## Interactive kort
# library(soc.ca)
# library(reshape)
# example(soc.mca)
# p  <- map.ind(result)
# 
# interactive.points <- data.plot(result, "ind", dim=c(1,2))
# 
# des.data <- data.frame(names=result$names.ind, result$ctr.ind[,1:5])
# 
# des.data <- melt(des.data, id.vars="names")
# 
# q <- ggplot() + geom_line(data=des.data, aes(x=value, y=variable, showSelected=names)) + coord_flip()
# 
# q <- q + make_text(des.data, max(des.data$value) * 1.1, 3, "names")
# 
# p <- p  + geom_point(data=interactive.points, aes(x=x, y=y, clickSelects=names))
# p <- p  + geom_point(data=interactive.points, aes(x=x, y=y, showSelected=names), color="red")
# 
# gg2animint(list(plot1=p, plot2=q), out.dir = "test.p")
# 
# 
# 


######################################################
### Pretty vector printing

pp             <- function(x, n = 30, head = TRUE){
  as.matrix(head(sort(x, decreasing = head), n))
}

#########################################################
## Levels of power

levels.of.power <- function(x){
  
  var           <- vector(length = length(x))
  var[x == max(x)]                               <- "1. Power Elite"
  var[x < max(x) & x >= 2/3 * max(x)]            <- "2. Higher levels of Power"
  var[x < 2/3 * max(x) & x >= 1/3 * max(x)]      <- "3. Middle levels of Power"
  var[x < 1/3 * max(x)]                          <- "4. Lower levels of Power"
  as.factor(var)
}

#####################################################
# Positional method overlap
position.compare <- function(x, key = FALSE, digits = 0){
  
  position                       <- read.csv("~/My Dropbox/Elite/Data/Data/Top_elite_positional_method.csv",
                                             sep = "|", encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = "")
  position                       <- position[complete.cases(position),]
  
  position.names                 <- position$Name
  if (key == TRUE) position.names   <- position$Name[position$Key.position == 1]
  
  number.having.position         <- sum(x %in% position.names)
  share.having.position          <- sum(x %in% position.names) / length(x) * 100
  positions.represented          <- sum(position.names %in% x) 
  share.positions.represented    <- sum(position.names %in% x) / length(position.names) * 100
  
  out <- c("Individuals holding a position" = number.having.position,
           "% with a position"              = share.having.position,
           "Represented positions"          = positions.represented,
           "% of positions represented"     = share.positions.represented)
  
  round(out, digits=digits)
}

##############################################################################
# Sigma and Reach table
SR.table                 <- function(sigma = 14, reach = 2.1, rel.all = rel.all, net.com = net.com, core = core, elite.names = elite.names){
  
  net.sig                  <- elite.network(rel.all, sigma = sigma)
  net.com.sig              <- largest.component(net.sig, cut.off = 1)
  core.sig                 <- find.core.net(net.com.sig, reach = reach)
  kore.sig                 <- core.sig == max(core.sig)
  
  
  component.size           <- vcount(net.com.sig)
  component.size.i         <- vcount(net.com.sig) / vcount(net.com) * 100
  
  # Edgecount
  edges                    <- ecount(net.com.sig)
  edges.i                  <- ecount(net.com.sig) / ecount(net.com) * 100
  
  # Density
  density                  <- graph.density(net.com.sig)
  density.i                <- graph.density(net.com.sig) / graph.density(net.com) * 100
  
  # Maximum coreness
  coreness                 <- max(core.sig)
  coreness.i               <- max(core.sig) / max(core) * 100
  
  # Secondary agents
  secondary.actors.sig        <- secondary.actors(core.sig, rel.all)
  secondary.actors.n          <- sum(secondary.actors.sig != FALSE)
  
  l                       <- vcount(net.com.sig) + 1
  sec                     <- vector(length = l - 1)
  secondary               <- secondary.actors.sig
  sec[kore.sig]           <- secondary
  sec[sec=="FALSE"]       <- ""
  sec[V(net.com.sig)$name == "Prins Henrik"] <- ""
  
  elite.names.sig         <- V(net.com.sig)$name[kore.sig & sec == ""]
  
  
  
  # Core density
  net.core.sig             <- net.com.sig - which((V(net.com.sig)$name %in% elite.names.sig) == FALSE)
  net.core.sig             <- delete.edges(net.core.sig, which(E(net.core.sig)$weight > 1))
  core.density             <- graph.density(net.core.sig)
  
  
  # Core size 
  core.size                <- length(elite.names.sig)
  core.size.i              <- core.size / length(elite.names) * 100
  
  # Share of secondary actors
  secondary.actors.share   <- secondary.actors.n / core.size * 100
  
  # Maximum coreness i forhold til core size
  cs.share.cv              <- coreness / core.size
  
  # Overlap with reference
  core.names               <- elite.names
  core.names.sig           <- elite.names.sig
  intersect.sig            <- intersect(y = core.names.sig, x = core.names)
  
  # Share of union with core.names
  
  # Antallet af originale der ikke kommer med
  excluded.12              <- length(intersect.sig) - length(core.names)
  included.12.share        <- length(intersect.sig) / length(core.names)  * 100
  
  # Overlap mellem 12 og de andre
  overlap.12                  <- length(intersect.sig) - length(core.names.sig)
  overlap.12.share            <- length(intersect.sig) / length(core.names.sig) * 100
  
  # Compare positional approach
  position.comparisions         <- position.compare(core.names.sig)
  position.comparisions.key     <- position.compare(core.names.sig, key = TRUE)
  
  # Levels of power - 1/3 af core value.
  levels.of.power.sig         <- levels.of.power(core.sig)
  table.of.power              <- table(levels.of.power.sig)
  vector.of.power             <- as.vector(table.of.power)
  names(vector.of.power)      <- levels(levels.of.power.sig)
  
  power.tab                   <- prop.table(table.of.power)
  rownames(power.tab)         <- paste("(%)", rownames(power.tab))
  power.vector                <- as.vector(power.tab)
  names(power.vector)         <- names(power.tab)
  
  
  position.tab                <- sapply(position.comparisions, cbind)
  names(position.tab)         <- paste("ALL:", names(position.comparisions))
  position.key.tab            <- sapply(position.comparisions.key, cbind)
  names(position.key.tab)     <- paste("KEY:", names(position.comparisions.key))
  
  out                         <- c("Size of component" = component.size, "Size of component (index)" = component.size.i,
                                   "Number of edges" = edges, "Number of edges (index)" = edges.i,
                                   "Density" = density, "Density (index)" = density.i,
                                   "Core density" = core.density,
                                   "Coreness value" = coreness, "Coreness value (index)" = coreness.i,
                                   "Core size" = core.size, "core.size (index)" = core.size.i,
                                   "Core value per core size" = cs.share.cv * 100,
                                   "Excluded elite individuals" = excluded.12, "Included elite individuals (%)" = included.12.share,
                                   "Secondary actors (%)" = secondary.actors.share, 
                                   vector.of.power, power.vector,
                                   position.tab, position.key.tab)
  out
}


#### Data funktioner
# inddel.postnummer <- function(postnummer, postnumre.mat, del="Region"){
#   post.nummer <- postnummer
#   output   <- vector(length=length(post.nummer))
#   
#   if (identical(del, "Region")){
#     regioner <- levels(postnumre.mat$ADRESSERINGSNAVN)
#     post.region <- postnumre.mat$ADRESSERINGSNAVN
#     
#   }
#   if (identical(del, "Kommune")){
#     regioner <- levels(postnumre.mat$ADRESSERINGSNAVN_1)
#     post.region <- postnumre.mat$ADRESSERINGSNAVN_1
#   }
#   
#   for (i in 1:length(regioner)) {
#     inddeling <- postnumre.mat$POSTNR[post.region==regioner[i]]
#     output[post.nummer %in% inddeling] <- regioner[i]
#   }
#   output <- as.factor(output)
#   levels(output)[levels(output)=="FALSE"] <- "Missing"
#   return(as.factor(output))
#   # del, kan tage funktionerne: "Region" og "Kommune"
#   # Bemærk at hvis der er overlap så tager funktionen den seneste. Dvs. at fx. ballerup bliver spist af herlev.
# }

#' Tag relation
#' 
#' Finds relations for each affiliation
#' @param rel.all
tag.relations            <- function(rel.all, org.names, by.individual = FALSE){
  rel.select             <- rel.all[rel.all$AFFILIATION %in% org.names,]
  if(by.individual == TRUE){
    rel.select             <- rel.all[rel.all$NAME %in% rel.select$NAME,]
  }
  rel.select
}



