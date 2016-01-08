#' graph.plot - Plots a weighted graph, with weighted edges as a default
#' 
#' @param graph a \link{igraph} network object
#' @param layout a two-column numerical matrix with coordinates for each vertex of graph
#' @param vertex.color a single value or a vector of the same length and order as the vertices in graph. See \link{colors} for valid single values.
#' @param vertex.fill a single value or a vector of the same length and order as the vertices in graph. See \link{colors} for valid single values.
#' @param vertex.shape a single value or a vector of the same length and order as the vertices in graph.
#' @param vertex.size a single value or a vector of the same length and order as the vertices in graph.
#' @param vertex.alpha a single value between 0 and 1 or a vector of the same length and order as the vertices in graph. 
#' @param edges if TRUE edges are drawn.
#' @param edge.color a single value or a vector of the same length and order as the edges in graph. See \link{colors} for valid single values.
#' @param edge.alpha a single value between 0 and 1 or a vector of the same length and order as the edges in graph. 
#' @param edge.size a single value or a vector of the same length and order as the edges in graph.
#' @param edge.line a single value or a vector of the same length and order as the edges in graph. 
#' @param edge.order a vector of the same length and order as the edges in graph. Edges are drawn in increasing order, with the highest overlapping those at a lower order.#' @param text
#' @param text.size if TRUE vertex names are drawn.
#' @param text.color a single value or a vector of the same length and order as the vertices in graph. See \link{colors} for valid single values.
#' @param text.alpha a single value or a vector of the same length and order as the vertices in graph.
#' @param legend sets the position of the legend. "side" places the legend on the right hand side and "bottom" places the legend along the bottom of the plot.
#' @param text.vjust a number setting the amount of vertical adjustment of the position of the text
#' @param midpoints if TRUE edges have a arrow at their middle showing the direction of the edge. This naturally only applies to directed networks.
#' @param midpoint.arrow sets the character of the midpoint arrow, see \link{arrow}
#' @param edge.text if not FALSE, then a vector with the labels for each edge.
#' @param edge.text.size a single value or a vector of the same length and order as the edges in graph.
#' @param edge.text.alpha a single value or a vector of the same length and order as the edges in graph.
#' @return a \link{ggplot2} plot
#' @export
#' @examples
#' data(den)
#' health.affil  <- has.tags(den, c("Health"))
#' den.health    <- den[den$AFFILIATION %in% health.affil,]
#' net.org       <- elite.network.org(den.health)
#' lay.org       <- layout.fruchterman.reingold(net.org)
#' p             <- graph.plot(net.org, layout = lay.org, vertex.size = V(net.org)$members, vertex.fill = degree(net.org),
#'                             edge.color = "darkmagenta", edge.alpha = log(1/E(net.org)$weight))
#' p             <- p + scale_fill_continuous(low = "white", high = "magenta") + scale_size_continuous(range = c(3, 10))
#' p + scale_alpha_continuous(range = c(0,1))
graph.plot  <- function(graph, layout = layout_with_fr(graph, weight = E(graph)$weight^2),
                        vertex.color = "black", vertex.fill = "grey60", vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
                        edges = TRUE, edge.color = "darkblue", edge.alpha = E(graph)$weight, edge.size = 1, edge.line = "solid", edge.order = FALSE,
                        text = FALSE, text.size = 3, text.color = "black", text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
                        midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed"), edge.text = FALSE, edge.text.size = 3, edge.text.alpha = 0.9){
  
  layout                  <- norm_coords(layout, xmin = 1, xmax = 10^10, ymin = 1, ymax = 10^10)
  vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
  
  vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
  v.i                     <- unlist(lapply(vertex.l, length)) == 1
  vertex.attributes       <- vertex.l[v.i]
  vertex.aes              <- vertex.l[v.i==FALSE]
  vertex.aes$x            <- vertex.coords$x
  vertex.aes$y            <- vertex.coords$y
  
  if(identical(edges, TRUE)){
    
    edge.coords             <- edge.coord(graph, layout)
    edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
    e.i                     <- unlist(lapply(edge.l, length)) == 1
    edge.attributes         <- edge.l[e.i]
    edge.attributes$lineend <- "butt"
    edge.aes                <- edge.l[e.i==FALSE]
    edge.aes$x              <- edge.coords$start.x
    edge.aes$y              <- edge.coords$start.y
    edge.aes$xend           <- edge.coords$slut.x
    edge.aes$yend           <- edge.coords$slut.y
    
    if(identical(edge.order, FALSE) == FALSE){
      edge.aes              <- as.list(as.data.frame(edge.aes)[order(edge.order),])
    } 
  }
  
  if(identical(midpoints, TRUE)){
    midpoint.attributes         <- edge.attributes
    midpoint.attributes$arrow   <- midpoint.arrow 
    midpoint.aes                <- edge.aes
    midpoint.aes$x              <- (edge.coords$start.x + edge.coords$slut.x) / 2
    midpoint.aes$y              <- (edge.coords$start.y + edge.coords$slut.y) / 2
    
    
    # Her bevæger vi os 1/l hen af vectoren imod slutpunktet. x1 kan så være midpunktet.
    # l = sqrt((x2 - x1)^2 + (y2 -y1)^2)
    # x3 = x1 + (1/l) * (x2 - x1)
    # y3 = y1 + (1/l) * (y2 - y1)
    
    a                            <- (edge.coords$slut.x - midpoint.aes$x)^2
    b                            <- (edge.coords$slut.y - midpoint.aes$y)^2
    L                            <- sqrt(a + b)
    midpoint.aes$xend            <- midpoint.aes$x + (1/L) * (edge.coords$slut.x - midpoint.aes$x)
    midpoint.aes$yend            <- midpoint.aes$y + (1/L) * (edge.coords$slut.y - midpoint.aes$y)
    midpoint.aes$group           <- paste(midpoint.aes$x, midpoint.aes$y)
    
  }
  
  if(identical(edge.text, FALSE)==FALSE){
    edge.l                     <- list(color = edge.color, alpha = edge.text.alpha, size = edge.text.size)
    e.i                        <- unlist(lapply(edge.l, length)) == 1
    edge.text.attributes       <- edge.l[e.i]
    edge.text.attributes$hjust <- -0.5
    edge.text.attributes$vjust <- -0.5
    edge.text.aes              <- edge.l[e.i==FALSE]
    edge.text.aes$x            <- (edge.coords$start.x + edge.coords$slut.x) / 2
    edge.text.aes$y            <- (edge.coords$start.y + edge.coords$slut.y) / 2
    edge.text.aes$label        <- edge.text
  }
  
  text.l                  <- list(size=text.size, color=text.color, alpha=text.alpha, vjust=text.vjust, lineheight=1)
  t.i                     <- unlist(lapply(text.l, length)) == 1
  text.attributes         <- text.l[t.i]
  text.aes                <- text.l[t.i==FALSE]
  text.aes$x              <- vertex.coords$x
  text.aes$y              <- vertex.coords$y
  text.aes$label          <- rownames(vertex.coords)
  if(length(text)>1){
    text.aes$label          <- text
    text                    <- TRUE
  }
  
  # Plot edges
  p <- ggplot()
  
  if(identical(edges, TRUE)){
    edge.attributes$mapping        <- do.call("aes", edge.aes)
    p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
  }
  
  # Plot midpoints
  if(identical(midpoints, TRUE)){
    midpoint.attributes$mapping     <- do.call("aes", midpoint.aes)
    p <- p + do.call("geom_segment", midpoint.attributes, quote=TRUE)
  }
  
  # Plot edge text
  if(identical(edge.text, FALSE)==FALSE){
    edge.text.attributes$mapping     <- do.call("aes", edge.text.aes)
    p <- p + do.call("geom_text", edge.text.attributes, quote=TRUE)
  }
  
  
  # Plot vertices
  vertex.attributes$mapping     <- do.call("aes", vertex.aes)
  p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
  
  # Plot text
  if(text==TRUE){
    text.attributes$mapping     <- do.call("aes", text.aes)
    p <- p + do.call("geom_text", text.attributes, quote=TRUE)
  }
  
  # Formatting
  p <- p + theme_bw()
  p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
  
  if(legend=="bottom")  p <- p + theme(legend.position='bottom', legend.direction="horizontal", legend.box="horizontal")
  
  p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())  
}

#####################################################
### distribution plots

#' Distribution plot
#' 
#' Creates a sorted line plot
#' @param x a numerical vector
#' @param decreasing if TRUE values are sorted from highest to lowest
#' @param sort if TRUE values are sorted
#' @param navn the plot title
#' @param mean if TRUE annotates the mean on the plot
#' @param linecolor the color of the line
#' @param rug if TRUE adds "rugs" to the plot
#' @param area if TRUE colors the area below the line
#' @param plf if TRUE fits a power law on the distribution, see \link{power.law.fit}
#' @return a ggplot2
#' @export

dist.plot <- function(x, decreasing = TRUE, sort = TRUE, navn = substitute(x), mean = FALSE, linecolor = "darkblue", rug = FALSE, area = TRUE, plf = FALSE, ... ){
  navn    <- paste(navn)
  
  if(identical(sort, TRUE)){
    x       <- sort(x, decreasing = decreasing)
    rank.x  <- order(rank(x), decreasing = decreasing)
    gg.mat  <- data.frame(x, rank = rank.x, x = x)
  }
  
  if(identical(sort, FALSE)){
    rank.x  <- 1:length(x)
    gg.mat  <- data.frame(x, rank = rank.x, x = x)
  }
  
  p       <- ggplot(data = gg.mat, aes(y = x, x = rank)) + geom_line(color = linecolor) + theme_bw()
  p       <- p + ylab(label = navn)
  p       <- p + xlab(label = paste("Rank by", navn))
  
  if (identical(area, TRUE)) {
    p       <- p + geom_segment(data = gg.mat , aes(x = rank, xend=rank, y = x, yend = 0, colour = x)) + geom_line(color=linecolor)
    p       <- p + guides(color=guide_colorbar(title=navn))
  }
  
  if(identical(rug, TRUE))  p <- p + geom_rug(sides = "r", aes(colour = x))
  
  if (mean == TRUE){
    mean.x      <- mean(x, na.rm=TRUE)
    rank.mean   <- rank.x[min(which(x >= mean.x))]
    left.mean   <- max(rank.x)-rank.mean
    p           <- p + annotate("segment", yend=0, y=mean.x, xend=rank.mean, x=rank.mean, color=linecolor, linetype="dashed") + annotate("segment", yend=mean.x, y=mean.x, xend=0, x=rank.mean, color=linecolor, linetype="dashed")
    p           <- p + annotate("text", x=rank.mean, y=mean.x, label=paste("Mean: ", round(mean.x, 2), "\n", "Rank: ", rank.mean, "\n", "Remaining :", left.mean, sep=""), hjust=1, vjust=-0.5, size=4, color=linecolor)
  }
  
  if (identical(plf, TRUE)){
    
    plf.txt.x     <- max(x) * 0.7 
    plf.txt.y     <- max(rank.x) * 0.2
    
    plf           <- power.law.fit(x)
    value.plf     <- plf$xmin
    rank.plf      <- min(which(x >= value.plf))
    left.rank     <- max(rank.x)-rank.plf
    p             <- p + annotate("segment", yend = 0, y = value.plf, xend = rank.plf, x = rank.plf, color = "black", linetype = "solid") + annotate("segment", yend=value.plf, y=value.plf, xend=0, x=rank.plf, color="black", linetype="solid")
    p             <- p + annotate("text", x = plf.txt.y, y = plf.txt.x, label = paste("Power law", "\n","Value: ", round(value.plf, 2), "\n", "Rank: ", rank.plf, "\n", "Remaining :", left.rank, "\n", "Alpha:", round(plf$alpha, 2), sep=""), hjust=1, vjust=-0.2, size=4, color="black")
  }
  
  p             <- p + scale_colour_continuous(low = "papayawhip", high = linecolor, guide = "colorbar")
  p
}

#' Line plots
#' 
#' Produces a plot with several lines
#' @param x is a vector or a dataframe of vectors
#' @param sort if TRUE values are sorted from highest to lowest
#' @param var.names a character vector with the variable names
#' @param label.x the label for the x axis
#' @param label.y the label for the y axis
#' @return a ggplot2 lineplot
#' @export
#' @examples
#' data(den)
#' health.affil  <- has.tags(den, c("Health"))
#' den.health    <- den[den$AFFILIATION %in% health.affil,]
#' net.health    <- elite.network(den.health)
#' measures      <- data.frame(degree = degree(net.health, normalized = TRUE), betweenness = betweenness(net.health, normalized = TRUE))
#' line.plot(measures)


line.plot <- function(x, sort = TRUE, var.names = colnames(x), label.x = NULL, label.y = NULL){
  
  if (identical(sort, TRUE)) x <- as.data.frame(apply(x, 2, sort, decreasing = FALSE))
  
  x$rank   <- seq(from = nrow(x), to = 1)  
  mx      <- melt(x, id.vars = "rank")
  
  p       <- ggplot(mx, aes(x = rank, y = value, group = variable, color = variable)) + geom_line(aes(linetype = variable))
  p       <- p + theme_bw() + scale_color_manual(values = c("darkblue", "purple", "darkorange", "darkgreen", "grey50", "darkred"))
  p       <- p + theme(panel.grid.major = element_line(color = "grey70", linetype = "dotted"), panel.grid.minor = element_line(color = "grey65", linetype = "dotted"))
  p       <- p + guides(guide_legend(title=""), color = guide_legend(title = ""))
  p       <- p + xlab(label = label.y) + ylab(label = label.x)
  p
}

##################################
# Data object for plotting
# Det her kan optimeres ved at lave en lang factor, ændre levels, som nu indeholder både afsender og modtager og derefter dele den i 2 igen.
# Måske kan det også klares med merge()

#' Edge coordinates
#' 
#' Generates coordinates as a data.frame suitable for ggplot2's \link{geom_segment}. This function is not super efficient and should be upgraded.
#' @param graph a \link{igraph} network object
#' @param layout a two-column numerical matrix with coordinates for each vertex of graph
#' @return an edge list as a matrix with coordinates of the start and end vertices for each edge.
#' @export

edge.coord <- function(graph, layout){
  
  graph.names       <- V(graph)$name
  el                <- data.frame(get.edgelist(graph))
  
  el.X1.levels.x    <- levels(el$X1)
  el.X1.levels.y    <- levels(el$X1)
  el.X2.levels.x    <- levels(el$X2)
  el.X2.levels.y    <- levels(el$X2)
  
  for (i in 1:length(graph.names)){
    navn             <- graph.names[i]
    navn.el.pos.1    <- which(el.X1.levels.x == navn)
    navn.el.pos.2    <- which(el.X2.levels.x == navn)
    el.X1.levels.x[navn.el.pos.1]      <- layout[i, 1] 
    el.X1.levels.y[navn.el.pos.1]      <- layout[i, 2] 
    el.X2.levels.x[navn.el.pos.2]      <- layout[i, 1] 
    el.X2.levels.y[navn.el.pos.2]      <- layout[i, 2] 
  }
  
  out                   <- data.frame(start.x = el$X1, start.y = el$X1, slut.x = el$X2, slut.y = el$X2, weight = E(graph)$weight)
  levels(out$start.x)   <- el.X1.levels.x
  levels(out$start.y)   <- el.X1.levels.y
  levels(out$slut.x)    <- el.X2.levels.x
  levels(out$slut.y)    <- el.X2.levels.y
  
  out                   <- apply(out, 2, as.character)
  out                   <- apply(out, 2, as.numeric)
  
  as.data.frame(out)
}


#' Vertex.coord
#' 
#' Convienience funcion for streamlining vertex.coordinates
#' @param graph a \link{igraph} network object
#' @param layout a two-column numerical matrix with coordinates for each vertex of graph
#' @return a data.frame with vertex coordinates
#' @export

vertex.coord <- function(graph, layout=layout.fruchterman.reingold(graph)){
  rownames(layout)  <- V(graph)$name
  layout            <- as.data.frame(layout, rownames(layout))
  colnames(layout)  <- c("x", "y")
  layout
}

#' Tile.plot
#' Plots a matrix as tile with color according to intensity
#' @param adj.mat is the input matrix, with named rows and columns and numerical cells
#' @return a ggplot2 tile plot
#' @export
#' @examples
#' data(pe13)
#' mat                  <- data.frame(degree = pe13$Degree...1, reach = pe13$Reach, memberships = pe13$Memberships)
#' rownames(mat)        <- as.character(pe13$Name)
#' adj.mat              <- t(as.matrix(mat[1:20,]))
#' tile.plot(adj.mat)

tile.plot <- function(adj.mat, max.color.value = 0.8, text.size = 3, brewer.set = "Set1"){
  mti                <- melt(adj.mat, as.is = TRUE)
  colnames(mti)      <- c("rows", "cols", "value")
  mti$name           <- factor(mti$rows, levels = rownames(adj.mat), ordered = TRUE )
  mti$rows           <- factor(mti$rows, levels = rownames(adj.mat), ordered = TRUE )
  mti$cols           <- factor(mti$cols, levels = colnames(adj.mat), ordered = TRUE )
  mti$color          <- mti$value
  mti                <- mti[order(mti$rows),]
  mti$color          <- unlist(lapply(split(mti$color, f = mti$rows), FUN = function(x) x/max(x, na.rm = TRUE)))
  mti$color[mti$color == 0] <- NA
  mti$color[mti$color > max.color.value]  <- max.color.value 
  
  sc                 <- list()
  sc$theme_bw        <- theme_bw()
 # sc$fill            <- scale_fill_continuous(high = "#b2182b", low = "#fddbc7", na.value = "white", guide = "none")
  sc$alpha           <- scale_alpha_continuous(range = c(0.2, 1), guide = "none", na.value = 0)
  sc$fill            <- scale_fill_manual(values = brewer.pal(n = nlevels(mti$rows), brewer.set), na.value = "white", guide = "none") 
  sc$axis.angle      <- theme(axis.text.x = element_text(size = 11, angle = 90, hjust = 1, color = "black"), axis.text.y = element_text(size = 11, color = "black"))
  sc$xlab            <- xlab(NULL)
  sc$ylab            <- ylab(NULL)
  sc$theme           <- theme(axis.ticks = element_blank(), panel.border = element_blank())
  
  p                  <- ggplot(data = mti, aes(x = rows, y = cols, fill = rows, label = value, alpha = color)) + geom_tile(color = "black") + geom_text(alpha = 1, size = text.size)
  p                  <- p + sc
  p
}


# Function does not create a default layout that is different from that produced by graph.plot

#' Twomode graphs
#' 
#' @param graph a twomode graph
#' @param layout if "default" 
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
