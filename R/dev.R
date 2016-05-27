# # DEN summary -----
# 
# den <- eliteDB.connections(pass )
# b <- den
# load("~/My Dropbox/Elite og Tal/Projekter/Magtelite-projektet 2016/Magtelite R/den.db.Rda")
# den <- den.db
# changes       <- function(den){
#   
#   # Number of affiliations
#   n.affil      <- table(den.db$SOURCE[duplicated(den.db$AFFILIATION) == FALSE])
#   # Number of positions
#   n.positions  <- table(den.db$SOURCE)
#   # Number of individuals
#   n.ind        <- table(den.db$SOURCE[duplicated(den.db$NAME) == FALSE])
#   # Number of archived positions
#   n.archived   <- table(den.db$SOURCE[is.na(den.db$ARCHIVED)])
#   # Number of active positions
#   n.active     <- n.positions - n.archived
#   # Last change
#   recent       <- lapply(split(den.db$MODIFIED, f = den.db$SOURCE), function(x) max(x, na.rm = TRUE))
#   recent       <- do.call("c", recent)
#        
#   # Newest affiliation
#   
#   # Percentage of individuals with CVR numbers
#   
#   data.frame("Affiliations"  = as.vector(n.affil),
#              "Positions"     = as.vector(n.positions),
#              "Individuals"   = as.vector(n.ind),
#              "Archived pos." = as.vector(n.archived),
#              "Active pos."   = as.vector(n.active),
#              "Last change"   = recent)
#   
#   
#   
#   
# }
# 
# 
# 
# 
# # # #####################################################################################
# # # # Geo - elite kodning
# # # library(ggmap)
# # # library(geosphere)
# # # library(soc.elite)
# # # library(igraph)
# # # data(den)
# # # data(pe13)
# # # 
# # # 
# # # lon                <- pe13$lon
# # # lat                <- pe13$lat
# # # x                  <- cbind(lon, lat)
# # # y                  <- cbind(lon, lat)
# # # 
# # # afstande           <- distm(x, y)
# # # rownames(afstande) <- pe13$Name
# # # colnames(afstande) <- pe13$Name
# # # 
# # # median(afstande, na.rm = T)
# # # 
# # # afstande.adj <- afstande
# # # afstande.adj[is.na(afstande.adj)] <- 0
# # # #afstande.adj[afstande.adj >= 500] <- 0
# # # 
# # # adjs          <- list()
# # # afs           <- seq(from = 100, to = 10000, by = 200)
# # # for( i in 1:length(afs)){
# # #   nif                                 <- afstande.adj
# # #   nif[afstande.adj >= afs[i]]         <- 0
# # #   adjs[[i]]                           <- nif
# # # }
# # # 
# # # l.graphs      <- lapply(adjs, graph.adjacency, mode = "undirected", weighted = TRUE)
# # # l.plots       <- lapply(l.graphs, graph.plot, vertex.fill = pe13$region, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # for(i in 1:length(l.plots)) l.plots[[i]] <- l.plots[[i]] + ggtitle(afs[i])
# # # 
# # # coms <- lapply(l.graphs, components)
# # # els  <- sapply(coms, getElement, "no")
# # # plot(afs, els)
# # # 
# # # pdf(file = "~/Desktop/geoafstande.pdf", height = 10, width = 10)
# # # l.plots
# # # dev.off()
# # # 
# # # afstand.g <- afstande.adj
# # # afstand.g[afstande.adj >= 2000] <- 0
# # # 
# # # graph.afstand <- graph.adjacency(afstand.g, mode = "undirected", weighted = TRUE)
# # # graph.plot(graph.afstand, vertex.fill = pe13$region, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # 
# # # sort(degree(graph.afstand))
# # # plot(table(degree(graph.afstand)))
# # # 
# # # afstand.2km   <- afstande.adj
# # # afstand.10km  <- afstande.adj
# # # afstand.2km[afstande.adj >= 2000]    <- 0
# # # afstand.10km[afstande.adj >= 10000]  <- 0
# # # 
# # # graph.2km     <- graph.adjacency(afstand.2km, mode = "undirected", weighted = TRUE)
# # # graph.10km    <- graph.adjacency(afstand.10km, mode = "undirected", weighted = TRUE)
# # # 
# # # deg.afstand.2km  <- degree(graph.2km)
# # # deg.afstand.10km <- degree(graph.10km) 
# # # 
# # # as.matrix(table(deg.afstand.2km))
# # # as.matrix(table(deg.afstand.10km))
# # # 
# # # social.geography <- deg.afstand.2km
# # # social.geography[deg.afstand.2km == 0 & deg.afstand.10km == 0] <- "Isolated at 10km"
# # # social.geography[deg.afstand.2km == 0 & deg.afstand.10km != 0] <- "Isolated at 2km"
# # # social.geography[deg.afstand.2km %in% 1:2]                     <- "1-2"
# # # social.geography[deg.afstand.2km %in% 3:6]                     <- "3-6"
# # # social.geography[deg.afstand.2km %in% 7:20]                    <- "7-20"
# # # social.geography[deg.afstand.2km %in% 21:30]                   <- "21-30"
# # # social.geography[deg.afstand.2km %in% 31:max(deg.afstand.2km)] <- "+30"
# # # 
# # # as.matrix(table(social.geography))
# # # 
# # # graph.plot(graph.afstand, vertex.fill = social.geography, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # 
# 
# # ###############################################
# # # REACH ---
# # 
# # # R er hvor langt væk en alter må være fra ego
# # # Først sletter vi alle edges med en vægt over R
# # # Derefter tager vi et neighborhood med make_ego_graph med en "order" på R/min(E(graph)$weight)
# # # Nu tager vi så alle shortests paths for ego graphen og sletter alle over R.
# # 
# # # Vi kan udregne order mere dynamisk tror jeg
# # # Vi kan også klare det sidste step mere smart.
# # library(soc.elite)
# # library(igraph)
# # data(den)
# # data(pe13)
# # graph <- net.elite
# # graph <- elite.network(den[1:9000,])
# # R <- 2.1
# # reach <- function(graph, R = 2.1){
# # g          <- graph
# # g          <- delete.edges(g, which(E(g)$weight > 2.1))
# # order      <- ceiling(R/min(E(g)$weight))
# # order      <- 2
# # 
# # q <- connect(g, order = 2)
# # ecount(q)
# # 
# # sort(degree(q))
# # 
# # e <- make_ego_graph(g, nodes = "Jesper Cramon", order = order)[[1]]
# # E(e)$weight
# # ec <- connect(e, order = 2)
# # E(e)$weight
# # table(is.na(E(ec)$weight))
# # 
# # out        <- numeric(vcount(g))
# # for(i in 1:vcount(g)){
# # ego.name   <- V(g)$name[i]
# # ego        <- make_ego_graph(g, order = order, nodes = i)[[1]]
# # out[i]     <- sum(distances(ego, v = ego.name) <= R)
# # 
# # rr <- function(x, g){
# #   ego.name   <- V(g)$name[x]
# #   ego        <- make_ego_graph(g, order = order, nodes = x)[[1]]
# #   sum(distances(ego, v = ego.name) <= R)
# # }
# # laply(1:vcount(g), rr, g = g, .progress = "text")
# # }
# # out
# # }
# # 
# # egos <- ego(graph = g, order = order)
# # names(egos) <- V(g)$name
# # sapply(egos, function(x, g) sum(distances(g, v = names(x), to = x) <= 2.1), g)
# # sp         <- distances(g)
# # ecount(g)
# # ecount(graph)
# # elite.network()