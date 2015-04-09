# Det her eksempel skal danne grobund for meget hurtige skabelon agtige analyser af en mindre sektor.
# Alle resultaterne præsenteres i en enkelt .pdf - funktionen kan derfor fungere som et meget hurtigt overblik inden man går i detaljer.
# Måske skal det istedetfor laves som en markdown rapport - hvor man alene ændrer input. Det kommer.


# Sectoren ved et sæt af tags
# En tabel med de inkluderede affiliations og med deres rangering
# Et organisations kort - med udvalgte navne
# Et zoomet kort over det største komponent af affiliations med alle navne
# Det største komponent af individer farvelagt efter elite
# Ego kort for det bedst integrerede individ

setwd(dir = "~/My Dropbox/R/virk_geo/output_virk_geo/")
library(soc.elite)
library(soc.report)
data(den)
data(pe13)

show.all.tags(den)

tags            <- c("Construction", "Housing", "Architecture", "BYGG", "ENTR")
affils.i.sektor <- has.tags(den, tags)


sektor.report   <- function(affils.i.sektor, den, file = "", pdf.height = 25, pdf.width = 25){

den.sektor      <- den[den$AFFILIATION %in% affils.i.sektor,]
net.all.org     <- elite.network.org(den)
net.org         <- elite.network.org(den.sektor)
measures        <- vertex.measures.directed(net.org)
measures.all    <- vertex.measures.directed(net.all.org)
measures.all    <- measures.all[rownames(measures.all) %in% affils.i.sektor,]

scale.changes   <- list()
scale.changes$fill  <- scale_fill_continuous(high = "darkblue", low = "whitesmoke", guide = "none")
scale.changes$alpha <- scale_alpha_continuous(range = c(0.1, 0.9), guide = "none")
scale.changes$size  <- scale_size_continuous(range = c(2, 10), guide = "none")

layout.vægt         <- 1/E(net.org)$weight/max(1/E(net.org)$weight) * 2
lay                 <- layout.fruchterman.reingold(graph = net.org, weights =  layout.vægt)

# Det interne netværk
p               <- graph.plot(net.org, lay, text = T, vertex.fill = measures$"Reach out", vertex.size = measures$"Reach out", edge.alpha = 1/E(net.org)$weight^5, text.size = 3, edge.size = 0.5)
p               <- p + scale.changes + ggtitle("Sektor centralitet")
p.sektor        <- p

# Deres placering i det store netværk
p               <- graph.plot(net.org, lay, text = T, vertex.fill = measures.all$"Reach out", vertex.size = measures.all$"Reach out", edge.alpha = 1/E(net.org)$weight^5, text.size = 3, edge.size = 0.5)
p               <- p + scale.changes + ggtitle("Elite centralitet")
p.elite         <- p

####################
# Individer

net.all         <- elite.network(den)
net.linkers     <- delete.vertices(net.all, V(net.all)$memberships < 2)



####################
# Out

pdf(file = "kort.pdf", height = pdf.height, width = pdf.width)
p.sektor
p.elite
dev.off()

}

########################################################################
# Dronningens netværk
# Vi skal have alle de individer der er knyttet til dronningen over en hvis vægt og så deres interne forbindelser

net.all <- elite.network(den)

dronningens.plads <- which(V(net.all)$name == "Dronning Margrethe")

distance.to.the.queen.if.directly.connected <- net.all[dronningens.plads,]

net.queen                                   <- delete.vertices(net.all, distance.to.the.queen.if.directly.connected == 0 | distance.to.the.queen.if.directly.connected >= 2)

lay <- layout.drl(net.queen)
graph.plot(net.queen, lay, edge.alpha = E(net.queen)$weight^5) + scale_alpha_continuous(range = c(0, 0.8))
