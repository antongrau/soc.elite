# ####################################################################################
# # Få pilene til at virke i graph.plot
# 
# # Vi skal standardisere layoutet når det kommer ind i graph.plot
# # Midterpunkterne skal nu identificeres efter de nye standardiserede layouts
# # Det samme gælder vores borders - men de findes måske kun i moneca??
# # Husk at importere grid
# # Vi skal bruge norm_coords
# library(grid)
# data(den)
# health.affil  <- has.tags(den, c("Health"))
# den.health    <- den[den$AFFILIATION %in% health.affil,]
# graph       <- elite.network.org(den.health)
# layout = layout_with_fr(graph, weight = E(graph)$weight^2)
# vertex.color = "black"
# vertex.fill = "grey60"
# vertex.shape = 21
# vertex.size = 3
# vertex.alpha = 1
# edges = TRUE
# edge.color = "darkblue"
# edge.alpha = E(graph)$weight
# edge.size = 1
# edge.line = "solid"
# edge.order = FALSE
# text = FALSE
# text.size = 3
# text.color = "black"
# text.alpha = 1
# legend = "side"
# text.vjust = 1.5
# midpoints = FALSE
# midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed")
# edge.text = FALSE
# edge.text.size = 3
# edge.text.alpha = 0.9
# 
# graph.plot  <- function(graph, layout = layout_with_fr(graph, weight = E(graph)$weight^2),
#                         vertex.color = "black", vertex.fill = "grey60", vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
#                         edges = TRUE, edge.color = "darkblue", edge.alpha = E(graph)$weight, edge.size = 1, edge.line = "solid", edge.order = FALSE,
#                         text = FALSE, text.size = 3, text.color = "black", text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
#                         midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed"), edge.text = FALSE, edge.text.size = 3, edge.text.alpha = 0.9){
#   
#   layout                  <- norm_coords(layout, xmin = 1, xmax = 100, ymin = 1, ymax = 100)
#   vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
#   
#   vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
#   v.i                     <- unlist(lapply(vertex.l, length)) == 1
#   vertex.attributes       <- vertex.l[v.i]
#   vertex.aes              <- vertex.l[v.i==FALSE]
#   vertex.aes$x            <- vertex.coords$x
#   vertex.aes$y            <- vertex.coords$y
#   
#   if(identical(edges, TRUE)){
#     
#     edge.coords             <- edge.coord(graph, layout)
#     edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
#     e.i                     <- unlist(lapply(edge.l, length)) == 1
#     edge.attributes         <- edge.l[e.i]
#     edge.attributes$lineend <- "butt"
#     edge.aes                <- edge.l[e.i==FALSE]
#     edge.aes$x              <- edge.coords$start.x
#     edge.aes$y              <- edge.coords$start.y
#     edge.aes$xend           <- edge.coords$slut.x
#     edge.aes$yend           <- edge.coords$slut.y
#     
#     if(identical(edge.order, FALSE) == FALSE){
#       edge.aes              <- as.list(as.data.frame(edge.aes)[order(edge.order),])
#       # Vi sorterer ikke edge.coords og den bruges længere nede
#     } 
#   }
#   
#   if(identical(midpoints, TRUE)){
#     midpoint.attributes         <- edge.attributes
#     midpoint.attributes$arrow   <- midpoint.arrow 
#     midpoint.aes                <- edge.aes
#     midpoint.aes$x              <- (edge.coords$start.x + edge.coords$slut.x) / 2
#     midpoint.aes$y              <- (edge.coords$start.y + edge.coords$slut.y) / 2
#     
#     
#     # Her bevæger vi os 1/l hen af vectoren imod slutpunktet. x1 kan så være midpunktet.
#     # l = sqrt((x2 - x1)^2 + (y2 -y1)^2)
#     # x3 = x1 + (1/l) * (x2 - x1)
#     # y3 = y1 + (1/l) * (y2 - y1)
#     
#     a                            <- (edge.coords$slut.x - midpoint.aes$x)^2
#     b                            <- (edge.coords$slut.y - midpoint.aes$y)^2
#     L                            <- sqrt(a + b)
#     midpoint.aes$xend            <- midpoint.aes$x + (1/L) * (edge.coords$slut.x - midpoint.aes$x)
#     midpoint.aes$yend            <- midpoint.aes$y + (1/L) * (edge.coords$slut.y - midpoint.aes$y)
#     midpoint.aes$group           <- paste(midpoint.aes$x, midpoint.aes$y)
#     
#   }
#   
#   if(identical(edge.text, FALSE)==FALSE){
#     edge.l                     <- list(color = edge.color, alpha = edge.text.alpha, size = edge.text.size)
#     e.i                        <- unlist(lapply(edge.l, length)) == 1
#     edge.text.attributes       <- edge.l[e.i]
#     edge.text.attributes$hjust <- -0.5
#     edge.text.attributes$vjust <- -0.5
#     edge.text.aes              <- edge.l[e.i==FALSE]
#     edge.text.aes$x            <- (edge.coords$start.x + edge.coords$slut.x) / 2
#     edge.text.aes$y            <- (edge.coords$start.y + edge.coords$slut.y) / 2
#     edge.text.aes$label        <- edge.text
#   }
#   
#   text.l                  <- list(size=text.size, color=text.color, alpha=text.alpha, vjust=text.vjust, lineheight=1)
#   t.i                     <- unlist(lapply(text.l, length)) == 1
#   text.attributes         <- text.l[t.i]
#   text.aes                <- text.l[t.i==FALSE]
#   text.aes$x              <- vertex.coords$x
#   text.aes$y              <- vertex.coords$y
#   text.aes$label          <- rownames(vertex.coords)
#   if(length(text)>1){
#     text.aes$label          <- text
#     text                    <- TRUE
#   }
#   
#   # Plot edges
#   p <- ggplot()
#   
#   if(identical(edges, TRUE)){
#     edge.attributes$mapping        <- do.call("aes", edge.aes)
#     p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
#   }
#   
#   # Plot midpoints
#   if(identical(midpoints, TRUE)){
#     midpoint.attributes$mapping     <- do.call("aes", midpoint.aes)
#     p <- p + do.call("geom_segment", midpoint.attributes, quote=TRUE)
#   }
#   
#   # Plot edge text
#   if(identical(edge.text, FALSE)==FALSE){
#     edge.text.attributes$mapping     <- do.call("aes", edge.text.aes)
#     p <- p + do.call("geom_text", edge.text.attributes, quote=TRUE)
#   }
#   
#   
#   # Plot vertices
#   vertex.attributes$mapping     <- do.call("aes", vertex.aes)
#   p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
#   
#   # Plot text
#   if(text==TRUE){
#     text.attributes$mapping     <- do.call("aes", text.aes)
#     p <- p + do.call("geom_text", text.attributes, quote=TRUE)
#   }
#   
#   # Formatting
#   p <- p + theme_bw()
#   p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
#   
#   if(legend=="bottom")  p <- p + theme(legend.position='bottom', legend.direction="horizontal", legend.box="horizontal")
#   
#   p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
#             axis.text.y=element_blank(),axis.ticks=element_blank(),
#             axis.title.x=element_blank(),
#             axis.title.y=element_blank(),
#             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#             panel.grid.minor=element_blank(),plot.background=element_blank())  
# }