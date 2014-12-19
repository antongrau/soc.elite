# ### Plot eksempel
# library(soc.elite)
# data(den)
# 
# # Find de affiliations vi skal bruge
# health.affil  <- has.tags(den, c("Health"))
# den.health    <- den[den$AFFILIATION %in% health.affil,]
# 
# # Lav netværksobjekterne
# net           <- elite.network(den.health)
# net.org       <- elite.network.org(den.health)
# 
# net
# net.org
# 
# # Layouts
# lay.org      <- layout.fruchterman.reingold(net.org)
# lay          <- layout.fruchterman.reingold(net)
# 
# graph.plot(net, layout = lay)
# as.matrix(sort(degree(net)))
# 
# graph.plot(net.org, lay.org)
# as.matrix(sort(degree(net.org)))
# 
# # Den største component
# net         <- largest.component(net)
# lay         <- layout.fruchterman.reingold(net)
# graph.plot(net, lay)
# 
# # Hvad kan vi ændre i plottet?
# ?graph.plot
# 
# #######################
# # Størrelsen på vertex (punktet)
# # V() giver adgang til egenskaber ved vertex
# 
# net.org
# stor.org   <- V(net.org)$members
# stor.org
# graph.plot(net.org, layout = lay.org, vertex.size = stor.org)
# 
# degree.org <- degree(net.org)
# degree.org
# graph.plot(net.org, layout = lay.org, vertex.size = degree.org)
# 
# # Ak men de prikker der er jo meget meget små!
# p          <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org)
# p + scale_size_continuous(range = c(0.1, 20))
# p + scale_size_continuous(range = c(0.1, 20))
# p + scale_size_continuous(range = c(3, 10))
# 
# # Farven på prikken - den kontinuerte variabel
# p          <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org, vertex.fill = degree.org)
# p 
# p + scale_fill_continuous(low = "whitesmoke", high = "magenta") + scale_size_continuous(range = c(3, 10))
# 
# # Farven på prikken med den kategorielle variabel
# læge.org    <- has.tags(den.health, "Standsforening")
# læge.org    <- V(net.org)$name %in% læge.org
# 
# p           <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org, vertex.fill = læge.org)
# p           <- p + scale_size_continuous(range = c(3, 10))
# p
# 
# # Puha det er ikke konsulentagtigt nok
# p + scale_fill_manual(values = c("whitesmoke", "magenta"))
# 
# # Vi gemmen den til senere
# p.org <- p + scale_fill_manual(values = c("whitesmoke", "magenta"))
# 
# #######################
# # Egenskaber ved edges
# # E() giver adgang til egenskaber ved edges
# 
# # Vægten
# E(net.org)$weight
# 
# # Hov den er jo vendt om?
# vægt <- 1/E(net.org)$weight
# 
# p         <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org, vertex.fill = degree.org, edge.alpha = vægt, edge.color = "magenta")
# p         <- p + scale_size_continuous(range = c(3, 10)) + scale_fill_continuous(low = "whitesmoke", high = "magenta")
# p
# 
# p + scale_alpha_continuous(range = c(0,1))
# p + scale_alpha_continuous(range = c(0.4,1))
# p + scale_alpha_continuous(range = c(0.1,1))
# 
# # Edge betweenness
# eb         <- edge.betweenness(net.org)
# 
# p         <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org, vertex.fill = degree.org, edge.alpha = eb, edge.color = "magenta")
# p         <- p + scale_size_continuous(range = c(3, 10)) + scale_fill_continuous(low = "whitesmoke", high = "magenta")
# p + scale_alpha_continuous(range = c(0,1))
# 
# 
# 
# ########################
# # Tekst
# 
# p         <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org, vertex.fill = degree.org, edge.alpha = eb, edge.color = "magenta", text = TRUE)
# p         <- p + scale_size_continuous(range = c(3, 10)) + scale_fill_continuous(low = "whitesmoke", high = "magenta")
# p
# 
# between   <- betweenness(net.org)
# p         <- graph.plot(net.org, layout = lay.org, vertex.size = stor.org, vertex.fill = degree.org, edge.alpha = 0.1, edge.color = "magenta", text = T,
#                         text.alpha = between, text.size = 4)
# p         <- p + scale_size_continuous(range = c(3, 10)) + scale_fill_continuous(low = "whitesmoke", high = "magenta")
# p + scale_alpha_continuous(range = c(0,1))
# 
# 
# 
# ########
# # Layout
# graph.plot(net, layout = lay)
# fc         <- fastgreedy.community(net)
# str(fc)
# fc$membership
# table(fc$membership)
# fc         <- as.factor(fc$membership)
# graph.plot(net, layout = lay, vertex.fill = fc)
# 
# skala <- list()
# skala$alpha <- scale_alpha_continuous(range = c(0.2,1), guide = "none")
# skala$color <- scale_color_continuous(low = "antiquewhite", high = "deeppink", guide = "none")
# skala$size  <- scale_size_continuous(range = c(2.5, 6), guide = "none")
# 
# p          <- graph.plot(net, layout = lay, vertex.fill = fc, vertex.size = degree(net), edge.alpha = 1/E(net)$weight, edge.color = 1/E(net)$weight) 
# p + skala
# 
# ?layout.fruchterman.reingold
# 
# # Kamada.kawai
# # Giver et mere linjet og hierarkisk kort
# lay        <- layout.kamada.kawai(net)
# p          <- graph.plot(net, layout = lay, vertex.fill = fc, vertex.size = degree(net), edge.alpha = 1/E(net)$weight, edge.color = 1/E(net)$weight) 
# p + skala
# 
# # Circle layout
# lay        <- layout.circle(net)
# p          <- graph.plot(net, layout = lay, vertex.fill = fc, vertex.size = degree(net), edge.alpha = 1/E(net)$weight, edge.color = 1/E(net)$weight) 
# p + skala
# 
# # Fruchterman.reingold
# # Hvis man øger betydningen af vægten så trækes forbundne tættere sammen og giver en mere "segmentet"-feel
# # Area afgør i nogen udstrækning "rundheden" af skyen
# # Niter - hvis man sætter mængden af iterationer op så forbedres kvaliteten
# 
# lay        <- layout.fruchterman.reingold(net)
# p          <- graph.plot(net, layout = lay, vertex.fill = fc, vertex.size = degree(net), edge.alpha = 1/E(net)$weight, edge.color = 1/E(net)$weight) 
# p + skala
# 
# lay        <- layout.fruchterman.reingold(net, weights = E(net)$weight^8, niter = 500000, coolexp = 3,
#                                           maxdelta = vcount(net), area = (vcount(net)^2),
#                                           repulserad = vcount(net)*vcount(net)^2)
# p          <- graph.plot(net, layout = lay, vertex.fill = fc, vertex.size = degree(net), edge.alpha = 1/E(net)$weight, edge.color = 1/E(net)$weight) 
# p + skala
# 
# p          <- p + skala
# 
# 
# 
# #########
# # Eksport
# setwd("~/Desktop/")
# 
# # Grafik
# pdf(file = "plot.pdf", height = 30, width = 30)
# p
# p.org
# dev.off()
# 
# jpeg(file = "plot.jpg", width = 800, height = 1000)
# p
# dev.off()
# 
# # Layout og resultater i R format
# # R's data format er hurtigt og nemt, men ikke stabilt. Brug det ikke til noget du ikke kan rekonstruere.
# save(net, lay, file = "layout.Rda")
# load("layout.Rda")
# 
# # CSV
# write.csv(lay, file = "layout.csv")
# 
# 
# ###################################
# # Rodet vægtning
# 
# adj[adj == 1] <- 0
# adj[adj > 10] <- 10
# 
# adj.link <- adj.link * 2
# 
# adj <- adj.link + adj
# 
# adj * adj.link
# 
# 
# event <- "Bal hos dronningen"
# person <- "dronningen"
# 
# incidence[rownames(incidence) %in% person, colnames(incidence) %in% event] <- incidence[, colnames(incidence) %in% event] * 2
# 
