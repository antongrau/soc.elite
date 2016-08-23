# # # DEN summary -----
# #
# den <- eliteDB.connections(pass )
# b <- den
# load("~/My Dropbox/Elite og Tal/Projekter/Magtelite-projektet 2016/Magtelite R/den.db.Rda")
# den <- den.db
# library(qlcMatrix)
# summary.den <- function(den){
#   incidence         <- xtabs(data = den, formula = ~ NAME + AFFILIATION, sparse = T)
#   adj.affil         <- crossprod(incidence)
#   diag(adj.affil)   <- 0
#   cs                <- colSums(incidence)
#   rs                <- rowSums(incidence)
#   
#   
#   
#   affiliations      <- ncol(incidence)
#   individuals       <- nrow(incidence)
#   positions         <- nrow(den)
#   hangers           <- sum(rs == 1)
#   isolated.affils   <- sum(rowSums(adj.affil) == 0)
#   
#   max.members       <- max(cs)
#   max.memberships   <- max(rs)
#   
#   
# }
# 
# 
# # changes       <- function(den){
# #
# #   # Number of affiliations
# #   n.affil      <- table(den.db$SOURCE[duplicated(den.db$AFFILIATION) == FALSE])
# #   # Number of positions
# #   n.positions  <- table(den.db$SOURCE)
# #   # Number of individuals
# #   n.ind        <- table(den.db$SOURCE[duplicated(den.db$NAME) == FALSE])
# #   # Number of archived positions
# #   n.archived   <- table(den.db$SOURCE[is.na(den.db$ARCHIVED)])
# #   # Number of active positions
# #   n.active     <- n.positions - n.archived
# #   # Last change
# #   recent       <- lapply(split(den.db$MODIFIED, f = den.db$SOURCE), function(x) max(x, na.rm = TRUE))
# #   recent       <- do.call("c", recent)
# #
# #   # Newest affiliation
# #
# #   # Percentage of individuals with CVR numbers
# #
# #   data.frame("Affiliations"  = as.vector(n.affil),
# #              "Positions"     = as.vector(n.positions),
# #              "Individuals"   = as.vector(n.ind),
# #              "Archived pos." = as.vector(n.archived),
# #              "Active pos."   = as.vector(n.active),
# #              "Last change"   = recent)
# #
# #
# #
# #
# # }
# #
# #
# #
# #
# # # # #####################################################################################
# # # # # Geo - elite kodning
# # # # library(ggmap)
# # # # library(geosphere)
# # # # library(soc.elite)
# # # # library(igraph)
# # # # data(den)
# # # # data(pe13)
# # # #
# # # #
# # # # lon                <- pe13$lon
# # # # lat                <- pe13$lat
# # # # x                  <- cbind(lon, lat)
# # # # y                  <- cbind(lon, lat)
# # # #
# # # # afstande           <- distm(x, y)
# # # # rownames(afstande) <- pe13$Name
# # # # colnames(afstande) <- pe13$Name
# # # #
# # # # median(afstande, na.rm = T)
# # # #
# # # # afstande.adj <- afstande
# # # # afstande.adj[is.na(afstande.adj)] <- 0
# # # # #afstande.adj[afstande.adj >= 500] <- 0
# # # #
# # # # adjs          <- list()
# # # # afs           <- seq(from = 100, to = 10000, by = 200)
# # # # for( i in 1:length(afs)){
# # # #   nif                                 <- afstande.adj
# # # #   nif[afstande.adj >= afs[i]]         <- 0
# # # #   adjs[[i]]                           <- nif
# # # # }
# # # #
# # # # l.graphs      <- lapply(adjs, graph.adjacency, mode = "undirected", weighted = TRUE)
# # # # l.plots       <- lapply(l.graphs, graph.plot, vertex.fill = pe13$region, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # # for(i in 1:length(l.plots)) l.plots[[i]] <- l.plots[[i]] + ggtitle(afs[i])
# # # #
# # # # coms <- lapply(l.graphs, components)
# # # # els  <- sapply(coms, getElement, "no")
# # # # plot(afs, els)
# # # #
# # # # pdf(file = "~/Desktop/geoafstande.pdf", height = 10, width = 10)
# # # # l.plots
# # # # dev.off()
# # # #
# # # # afstand.g <- afstande.adj
# # # # afstand.g[afstande.adj >= 2000] <- 0
# # # #
# # # # graph.afstand <- graph.adjacency(afstand.g, mode = "undirected", weighted = TRUE)
# # # # graph.plot(graph.afstand, vertex.fill = pe13$region, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # #
# # # # sort(degree(graph.afstand))
# # # # plot(table(degree(graph.afstand)))
# # # #
# # # # afstand.2km   <- afstande.adj
# # # # afstand.10km  <- afstande.adj
# # # # afstand.2km[afstande.adj >= 2000]    <- 0
# # # # afstand.10km[afstande.adj >= 10000]  <- 0
# # # #
# # # # graph.2km     <- graph.adjacency(afstand.2km, mode = "undirected", weighted = TRUE)
# # # # graph.10km    <- graph.adjacency(afstand.10km, mode = "undirected", weighted = TRUE)
# # # #
# # # # deg.afstand.2km  <- degree(graph.2km)
# # # # deg.afstand.10km <- degree(graph.10km)
# # # #
# # # # as.matrix(table(deg.afstand.2km))
# # # # as.matrix(table(deg.afstand.10km))
# # # #
# # # # social.geography <- deg.afstand.2km
# # # # social.geography[deg.afstand.2km == 0 & deg.afstand.10km == 0] <- "Isolated at 10km"
# # # # social.geography[deg.afstand.2km == 0 & deg.afstand.10km != 0] <- "Isolated at 2km"
# # # # social.geography[deg.afstand.2km %in% 1:2]                     <- "1-2"
# # # # social.geography[deg.afstand.2km %in% 3:6]                     <- "3-6"
# # # # social.geography[deg.afstand.2km %in% 7:20]                    <- "7-20"
# # # # social.geography[deg.afstand.2km %in% 21:30]                   <- "21-30"
# # # # social.geography[deg.afstand.2km %in% 31:max(deg.afstand.2km)] <- "+30"
# # # #
# # # # as.matrix(table(social.geography))
# # # #
# # # # graph.plot(graph.afstand, vertex.fill = social.geography, edge.color = "black", edge.size = 0.3, text = pe13$postnummer)
# # # #
# #
# # # ###############################################
# # # # REACH ---
# # #
# # # # R er hvor langt væk en alter må være fra ego
# # # # Først sletter vi alle edges med en vægt over R
# # # # Derefter tager vi et neighborhood med make_ego_graph med en "order" på R/min(E(graph)$weight)
# # # # Nu tager vi så alle shortests paths for ego graphen og sletter alle over R.
# # #
# # # # Vi kan udregne order mere dynamisk tror jeg
# # # # Vi kan også klare det sidste step mere smart.
# # library(soc.elite)
# # library(igraph)
# # data(den)
# # data(pe13)
# # graph <- net.elite
# # graph.all <- elite.network(droplevels(den))
# # R <- 2.1
# # reach <- function(graph, R = 2.1){
# # g          <- graph
# # g          <- delete.edges(g, which(E(g)$weight > 2.1))
# # #order      <- ceiling(R/min(E(g)$weight))
# # order      <- R
# #
# # rr <- function(id, g, order){
# #   e        <- make_ego_graph(graph = g, order = order, nodes = id)[[1]]
# #   name     <- which(V(e)$name %in% V(g)$name[id])
# #   es       <- e[name,]
# #   min.es   <- order - min(es[es > 0])
# #   e        <- delete.edges(e, which(E(e)$weight > min.es))
# #   sum(distances(graph = e, v = name) >= order)
# # }
# #
# # reach      <- laply(1:vcount(g), rr, g = g, order = order)
# # reach
# # }
# #
# # a <- system.time(reach(graph))
# # a
# #
# # b <- system.time(reach(graph.all))
# # b
# #
# # ?graph.bfs
# #
# # order         <- 2
# # id            <- 1
# #
# #
# # adj           <- cbind(a = c(0, 1.5, 0, 0, 1, 1, 0),
# #                        b = c(1.5, 0, 0.5, 1, 1, 1, 0),
# #                        c = c(0, 0.5, 0, 0, 0, 0, 0),
# #                        d = c(0, 1, 0, 0, 0, 0, 0),
# #                        e = c(1, 1, 0, 0, 0, 0, 1),
# #                        f = c(1, 1, 0, 0, 0, 0, 1),
# #                        g = c(0, 0, 0, 0, 1, 1, 0))
# # rownames(adj) <- colnames(adj)
# # adj <- Matrix(adj)
# #
# # reach.from.adj <- function(adj, id, order){
# # #adj[adj == 0] <- NA
# #
# # adj           <- adj[,which(adj[id, ] > 0)]
# # first         <- adj[id,]
# # ar.i          <- which(adj != 0, arr.ind = T) # Det her kan hives ud af en sparse matrice, hvis det logiske udtryk er langsomt
# # for(i in 1:length(first)){
# # ind <- ar.i[ar.i[,2] == i, ]
# # adj[ind[,1], i]  <- adj[ind[,1], i] + first[i]
# # }
# # adj[id,]      <- first
# #
# # ar            <- which(adj <= order & adj > 0, arr.ind = T) # Det her må være dårligt
# #
# # first.degree  <- ar[,2][ar[,1] == id]
# # second.degree <- ar[,1][ar[,1] != id]
# # length(unique(c(first.degree, second.degree)))
# # }
# #
# #
# #
# # reach.from.adj(adj, id = 2, order = 2)
# #
# # # Test med stor data ----
# # hoods            <- neighborhood(graph, order = 2)
# # id <- 1
# # hood <- hoods[[id]]
# #
# # graph.adj        <- as_adj(graph, attr = "weight")
# #
# # reach       <- function(graph, id, order){
# # hood             <- ego(graph, nodes = id, order = 2)[[1]]
# # adj              <- graph[as.numeric(hood), as.numeric(hood)]
# # reach.from.adj(adj, id = 1, order = order)
# # }
# #
# # graph <- graph.all
# # graph <- delete.edges(graph, edges = which(E(graph)$weight >= order))
# # har.reach <- which(neighborhood.size(graph, order = 2) > 1)
# # system.time(sapply(har.reach[1:1000], reach, graph = graph, order = order))
# #
# # str(hoods)
# #
# #
# # g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = T)
# # plot(g)
# 
# # #
# # # egos <- ego(graph = g, order = order)
# # # names(egos) <- V(g)$name
# # # sapply(egos, function(x, g) sum(distances(g, v = names(x), to = x) <= 2.1), g)
# # # sp         <- distances(g)
# # # ecount(g)
# # # ecount(graph)
# # # elite.network()