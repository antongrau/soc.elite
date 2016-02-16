# #####################################################################################
# # Geo - elite kodning
# library(ggmap)
# library(geosphere)
# library(soc.elite)
# library(igraph)
# data(den)
# data(pe13)
# 
# 
# lon                <- pe13$lon
# lat                <- pe13$lat
# x                  <- cbind(lon, lat)
# y                  <- cbind(lon, lat)
# 
# afstande           <- distm(x, y)
# rownames(afstande) <- pe13$Name
# colnames(afstande) <- pe13$Name
# 
# median(afstande, na.rm = T)
# 
# afstande.adj <- afstande
# afstande.adj[is.na(afstande.adj)] <- 0
# #afstande.adj[afstande.adj >= 500] <- 0
# 
# adjs          <- list()
# afs           <- seq(from = 100, to = 10000, by = 200)
# for( i in 1:length(afs)){
#   nif                                 <- afstande.adj
#   nif[afstande.adj >= afs[i]]         <- 0
#   adjs[[i]]                           <- nif
# }
# 
# l.graphs      <- lapply(adjs, graph.adjacency, mode = "undirected", weighted = TRUE)
# l.plots       <- lapply(l.graphs, graph.plot, vertex.fill = pe13$region, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# for(i in 1:length(l.plots)) l.plots[[i]] <- l.plots[[i]] + ggtitle(afs[i])
# 
# coms <- lapply(l.graphs, components)
# els  <- sapply(coms, getElement, "no")
# plot(afs, els)
# 
# pdf(file = "~/Desktop/geoafstande.pdf", height = 10, width = 10)
# l.plots
# dev.off()
# 
# afstand.g <- afstande.adj
# afstand.g[afstande.adj >= 2000] <- 0
# 
# graph.afstand <- graph.adjacency(afstand.g, mode = "undirected", weighted = TRUE)
# graph.plot(graph.afstand, vertex.fill = pe13$region, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# 
# sort(degree(graph.afstand))
# plot(table(degree(graph.afstand)))
# 
# afstand.2km   <- afstande.adj
# afstand.10km  <- afstande.adj
# afstand.2km[afstande.adj >= 2000]    <- 0
# afstand.10km[afstande.adj >= 10000]  <- 0
# 
# graph.2km     <- graph.adjacency(afstand.2km, mode = "undirected", weighted = TRUE)
# graph.10km    <- graph.adjacency(afstand.10km, mode = "undirected", weighted = TRUE)
# 
# deg.afstand.2km  <- degree(graph.2km)
# deg.afstand.10km <- degree(graph.10km) 
# 
# as.matrix(table(deg.afstand.2km))
# as.matrix(table(deg.afstand.10km))
# 
# social.geography <- deg.afstand.2km
# social.geography[deg.afstand.2km == 0 & deg.afstand.10km == 0] <- "Isolated at 10km"
# social.geography[deg.afstand.2km == 0 & deg.afstand.10km != 0] <- "Isolated at 2km"
# social.geography[deg.afstand.2km %in% 1:2]                     <- "1-2"
# social.geography[deg.afstand.2km %in% 3:6]                     <- "3-6"
# social.geography[deg.afstand.2km %in% 7:20]                    <- "7-20"
# social.geography[deg.afstand.2km %in% 21:30]                   <- "21-30"
# social.geography[deg.afstand.2km %in% 31:max(deg.afstand.2km)] <- "+30"
# 
# as.matrix(table(social.geography))
# 
# graph.plot(graph.afstand, vertex.fill = social.geography, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# 
# # ####################################################################################
# # # Få pilene til at virke i graph.plot
# # 
# # # Vi skal standardisere layoutet når det kommer ind i graph.plot
# # # Midterpunkterne skal nu identificeres efter de nye standardiserede layouts
# # # Det samme gælder vores borders - men de findes måske kun i moneca??
# # # Husk at importere grid
# # # Vi skal bruge norm_coords
# # library(grid)
# # data(den)
# # health.affil  <- has.tags(den, c("Health"))
# # den.health    <- den[den$AFFILIATION %in% health.affil,]
# # graph       <- elite.network.org(den.health)
# # layout = layout_with_fr(graph, weight = E(graph)$weight^2)
# # vertex.color = "black"
# # vertex.fill = "grey60"
# # vertex.shape = 21
# # vertex.size = 3
# # vertex.alpha = 1
# # edges = TRUE
# # edge.color = "darkblue"
# # edge.alpha = E(graph)$weight
# # edge.size = 1
# # edge.line = "solid"
# # edge.order = FALSE
# # text = FALSE
# # text.size = 3
# # text.color = "black"
# # text.alpha = 1
# # legend = "side"
# # text.vjust = 1.5
# # midpoints = FALSE
# # midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed")
# # edge.text = FALSE
# # edge.text.size = 3
# # edge.text.alpha = 0.9
# # 
# # graph.plot  <- function(graph, layout = layout_with_fr(graph, weight = E(graph)$weight^2),
# #                         vertex.color = "black", vertex.fill = "grey60", vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
# #                         edges = TRUE, edge.color = "darkblue", edge.alpha = E(graph)$weight, edge.size = 1, edge.line = "solid", edge.order = FALSE,
# #                         text = FALSE, text.size = 3, text.color = "black", text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
# #                         midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed"), edge.text = FALSE, edge.text.size = 3, edge.text.alpha = 0.9){
# #   
# #   layout                  <- norm_coords(layout, xmin = 1, xmax = 100, ymin = 1, ymax = 100)
# #   vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
# #   
# #   vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
# #   v.i                     <- unlist(lapply(vertex.l, length)) == 1
# #   vertex.attributes       <- vertex.l[v.i]
# #   vertex.aes              <- vertex.l[v.i==FALSE]
# #   vertex.aes$x            <- vertex.coords$x
# #   vertex.aes$y            <- vertex.coords$y
# #   
# #   if(identical(edges, TRUE)){
# #     
# #     edge.coords             <- edge.coord(graph, layout)
# #     edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
# #     e.i                     <- unlist(lapply(edge.l, length)) == 1
# #     edge.attributes         <- edge.l[e.i]
# #     edge.attributes$lineend <- "butt"
# #     edge.aes                <- edge.l[e.i==FALSE]
# #     edge.aes$x              <- edge.coords$start.x
# #     edge.aes$y              <- edge.coords$start.y
# #     edge.aes$xend           <- edge.coords$slut.x
# #     edge.aes$yend           <- edge.coords$slut.y
# #     
# #     if(identical(edge.order, FALSE) == FALSE){
# #       edge.aes              <- as.list(as.data.frame(edge.aes)[order(edge.order),])
# #       # Vi sorterer ikke edge.coords og den bruges længere nede
# #     } 
# #   }
# #   
# #   if(identical(midpoints, TRUE)){
# #     midpoint.attributes         <- edge.attributes
# #     midpoint.attributes$arrow   <- midpoint.arrow 
# #     midpoint.aes                <- edge.aes
# #     midpoint.aes$x              <- (edge.coords$start.x + edge.coords$slut.x) / 2
# #     midpoint.aes$y              <- (edge.coords$start.y + edge.coords$slut.y) / 2
# #     
# #     
# #     # Her bevæger vi os 1/l hen af vectoren imod slutpunktet. x1 kan så være midpunktet.
# #     # l = sqrt((x2 - x1)^2 + (y2 -y1)^2)
# #     # x3 = x1 + (1/l) * (x2 - x1)
# #     # y3 = y1 + (1/l) * (y2 - y1)
# #     
# #     a                            <- (edge.coords$slut.x - midpoint.aes$x)^2
# #     b                            <- (edge.coords$slut.y - midpoint.aes$y)^2
# #     L                            <- sqrt(a + b)
# #     midpoint.aes$xend            <- midpoint.aes$x + (1/L) * (edge.coords$slut.x - midpoint.aes$x)
# #     midpoint.aes$yend            <- midpoint.aes$y + (1/L) * (edge.coords$slut.y - midpoint.aes$y)
# #     midpoint.aes$group           <- paste(midpoint.aes$x, midpoint.aes$y)
# #     
# #   }
# #   
# #   if(identical(edge.text, FALSE)==FALSE){
# #     edge.l                     <- list(color = edge.color, alpha = edge.text.alpha, size = edge.text.size)
# #     e.i                        <- unlist(lapply(edge.l, length)) == 1
# #     edge.text.attributes       <- edge.l[e.i]
# #     edge.text.attributes$hjust <- -0.5
# #     edge.text.attributes$vjust <- -0.5
# #     edge.text.aes              <- edge.l[e.i==FALSE]
# #     edge.text.aes$x            <- (edge.coords$start.x + edge.coords$slut.x) / 2
# #     edge.text.aes$y            <- (edge.coords$start.y + edge.coords$slut.y) / 2
# #     edge.text.aes$label        <- edge.text
# #   }
# #   
# #   text.l                  <- list(size=text.size, color=text.color, alpha=text.alpha, vjust=text.vjust, lineheight=1)
# #   t.i                     <- unlist(lapply(text.l, length)) == 1
# #   text.attributes         <- text.l[t.i]
# #   text.aes                <- text.l[t.i==FALSE]
# #   text.aes$x              <- vertex.coords$x
# #   text.aes$y              <- vertex.coords$y
# #   text.aes$label          <- rownames(vertex.coords)
# #   if(length(text)>1){
# #     text.aes$label          <- text
# #     text                    <- TRUE
# #   }
# #   
# #   # Plot edges
# #   p <- ggplot()
# #   
# #   if(identical(edges, TRUE)){
# #     edge.attributes$mapping        <- do.call("aes", edge.aes)
# #     p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
# #   }
# #   
# #   # Plot midpoints
# #   if(identical(midpoints, TRUE)){
# #     midpoint.attributes$mapping     <- do.call("aes", midpoint.aes)
# #     p <- p + do.call("geom_segment", midpoint.attributes, quote=TRUE)
# #   }
# #   
# #   # Plot edge text
# #   if(identical(edge.text, FALSE)==FALSE){
# #     edge.text.attributes$mapping     <- do.call("aes", edge.text.aes)
# #     p <- p + do.call("geom_text", edge.text.attributes, quote=TRUE)
# #   }
# #   
# #   
# #   # Plot vertices
# #   vertex.attributes$mapping     <- do.call("aes", vertex.aes)
# #   p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
# #   
# #   # Plot text
# #   if(text==TRUE){
# #     text.attributes$mapping     <- do.call("aes", text.aes)
# #     p <- p + do.call("geom_text", text.attributes, quote=TRUE)
# #   }
# #   
# #   # Formatting
# #   p <- p + theme_bw()
# #   p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
# #   
# #   if(legend=="bottom")  p <- p + theme(legend.position='bottom', legend.direction="horizontal", legend.box="horizontal")
# #   
# #   p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
# #             axis.text.y=element_blank(),axis.ticks=element_blank(),
# #             axis.title.x=element_blank(),
# #             axis.title.y=element_blank(),
# #             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
# #             panel.grid.minor=element_blank(),plot.background=element_blank())  
# # }
# 
# 
# ###############################################
# # REACH ---
# 
# # R er hvor langt væk en alter må være fra ego
# # Først sletter vi alle edges med en vægt over R
# # Derefter tager vi et neighborhood med make_ego_graph med en "order" på R/min(E(graph)$weight)
# # Nu tager vi så alle shortests paths for ego graphen og sletter alle over R.
# 
# # Vi kan udregne order mere dynamisk tror jeg
# # Vi kan også klare det sidste step mere smart.
# library(soc.elite)
# library(igraph)
# data(den)
# data(pe13)
# graph <- net.elite
# graph <- elite.network(den[1:9000,])
# R <- 2.1
# reach <- function(graph, R = 2.1){
# g          <- graph
# g          <- delete.edges(g, which(E(g)$weight > 2.1))
# order      <- ceiling(R/min(E(g)$weight))
# order      <- 3
# 
# out        <- numeric(vcount(g))
# for(i in 1:vcount(g)){
# ego.name   <- V(g)$name[i]
# ego        <- make_ego_graph(g, order = order, nodes = i)[[1]]
# 
# out[i]     <- sum(distances(ego, v = ego.name) <= R)
# }
# out
# }
# 
# egos <- ego(graph = g, order = order)
# names(egos) <- V(g)$name
# sapply(egos, function(x, g) sum(distances(g, v = names(x), to = x) <= 2.1), g)
# sp         <- distances(g)
# ecount(g)
# ecount(graph)
# elite.network()