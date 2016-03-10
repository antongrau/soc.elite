#' Danish Elite Network
#' 
#' A affiliation network
#'
#' @name den
#' @docType data
NULL


#' Power Elite 2013
#' 
#' A propopographic dataset on the Danish power elite in 2013
#'
#' @name pe13
#' @docType data
NULL

#'Directors 2008 dataset
#'
#'Prosopographical data on the top 100 CEO's from the 82 largest Danish 
#'corporations in 2008.
#'@details The directors dataset is prosopographical data collected from a wide 
#'  array of sources on biographic and corporate information. Sources include 
#'  the Danish variant of Who's Who (Blaa Bog), a private business information 
#'  database (Greens Erhvervsinformation), journalistic portrait articles, 
#'  article search engines, bibliographic databases and financial reports. CEOs 
#'  from 82 corporations were selected according to their position as CEO in 
#'  December 2007. 18 executives are included on other criteria, taking into 
#'  account the magnitude of the corporations and issues regarding ownership and
#'  control, resulting in a final population of 100 CEOs. The 82 corporations 
#'  have formal ownership and management located in Denmark and were selected 
#'  through either financial capital, measured as having a turnover of over five
#'  billion DKK (650 million Eur.), or organizational capital, defined as having
#'  at least 5000 employees; 34 corporations were included on both criteria, 45 
#'  on financial capital and three on organizational capital alone. To avoid 
#'  including investors, rather than executives, a minimum of 500 employees was 
#'  also required, excluding 12 firms. Companies acting only as subsidiaries 
#'  were also excluded. Data is for public use  and no author permission is 
#'  needed, but we would love to hear from you if you find the data useful. The 
#'  following example is based on the analysis from the article: "A Very 
#'  Economic Elite: The Case of the Danish Top CEOs".
#'  
#'@name directors08
#'@docType data
#'@author Christoph Ellersgaard
#'@author Anton Grau Larsen
#'@references Ellersgaard, Christoph, Anton Grau Larsen, og Martin D. Munk. 
#'  2012. "A Very Economic Elite: The Case of the Danish Top CEOs". Sociology.
#'@references Ellersgaard, Christoph Houman, og Anton Grau Larsen. 2010. 
#'  "Firmaets Maend". Master Thesis, Copenhagen: University of Copenhagen.
#'@references Ellersgaard, Christoph Houman, og Anton Grau Larsen. 2011. 
#'  "Kulturel kapital blandt topdirektoerer i Danmark - En domineret 
#'  kapitalform?" Dansk Sociologi 22(3):9-29.
#'@references Larsen, Anton Grau, og Christoph Houman Ellersgaard. 2012. "Status
#'  og integration paa magtens felt for danske topdirektoerer". Praktiske 
#'  Grunde. Nordisk tidsskrift for kultur- og samfundsvidenskab 2012(2-3).
#'@keywords data
NULL

#' Top 1000 corporations in Denmark in 2013
#' 
#' Variables like Sector.Børsen, Turnover are from 2010.
#' @name corp13
#' @docType data
NULL

# load("~/My Dropbox/R/Corporate/output_corporate/corporate_data_all.Rda")
# colnames(total.data)
# corp13  <- data.frame(
#            Name                      = total.data$ORG_NAVN,
#            CVR                       = total.data$CVR_NR,
#            Adress                    = total.data$ADRESSE,
# 
#            Sector.Børsen             = total.data$BØRSEN.BRANCHE,
#            Turnover                  = total.data$OMSÆTNING.10,
#            Turnover.change.09.10     = total.data$ÆNDRING.OMSÆTNING.9.10,
#            Result.before.taxes       = total.data$RESULTAT.FØR.SKAT.10,
#            Result                    = total.data$NETTORESULTAT.10,
#            Balance                   = total.data$BALANCE.10,
#            Equity                    = total.data$EGENKAPITAL.10,
#            Employees                 = total.data$ANSATTE.10,
#            Employees.change.09.10    = total.data$ÆNDRING.ANSATTE.9.10,
#            
#            Component                 = total.data$component,
#            Degree                    = total.data$deg,
#            Betweenness               = total.data$between,
#            Closeness                 = total.data$close,
#            Reach                     = total.data$n2,
#            
#            Memberships.corporations    = total.data$Memberships...Corporations,
#            Memberships.business.organisations = total.data$Memberships...Business.organisations,
#            Memberships.interest.groups = total.data$Memberships...Interest.groups,
#            Memberships.state           = total.data$Memberships...State,
#            Memberships.science.and.education = total.data$Memberships...Science.and.education,
#            Memberships.culture         = total.data$Memberships...Culture,
#            Memberships.royal           = total.data$Memberships...Royal,
#            Memberships.leader.networks = total.data$Memberships...Leadership,
#            Memberships.all.noncorporate = total.data$Memberships...All.noncorporate.sectors,
#            
#            All.media           = total.data$all_media,
#            National.newspapers = total.data$national_papers,
#            Regional.newspapers = total.data$regional_papers,
#            Local.newspapers    = total.data$local_papers,
#            Magazines           = total.data$magazines,
#            Radio.TV            = total.data$radio_tv,
#            Webmedia            = total.data$websources,
#            Newsbureaus         = total.data$newsbureau,
#            Books               = total.data$books,
#            
#            Region              = total.data$region,
#            Coop                = total.data$coop,
#            Finance             = total.data$finance,
#            Stockexchange       = total.data$stockexchange,
#            Sector.dominance    = total.data$dominerende,
#            Global500           = total.data$Global
#            )
# 
# save(corp13, file = "~/soc.elite/data/corp13.rda")



##################################################################################
# Generate data 
# source("~/My Dropbox/R/Elite/after_match_2.R")
# 
# rel.all          <- read.csv("~/My Dropbox/Elite/Data/Data/Relation_ALL.csv", sep="|", encoding="UTF-8", stringsAsFactor = FALSE)
# 
# # BIQ LINK
# biq.id           <- rel.all$BIQ_PERSON_ID
# biq.link         <- paste("http://www.biq.dk/people/", biq.id, sep="")
# biq.link[biq.id == 0]    <- ""
# 
# # Essential columns
# # Gender
# load("~/My Dropbox/R/Elite/Navne/names_gender")
# gender.rel        <- find.gender(as.character(rel.all$NAVN), names.gender)
# levels(gender.rel)[2] <- "Undefined"
# 
# # CVR Number
# org.virk.other         <- read.csv("~/My Dropbox/Elite/Dataindsamling/CSV/Organisation_BIQ_andre_virksomheder_connections.csv", sep="|", encoding="UTF-8")
# org.virk               <- read.csv("~/My Dropbox/Elite/Dataindsamling/CSV/Organisation_BIQ.csv", sep="|", encoding="UTF-8")
# org.fond               <- read.csv("~/My Dropbox/Elite/Dataindsamling/CSV/Organisation_BIQ_fonde.csv", sep="|", encoding="UTF-8")
# 
# cvr.virk.other         <- data.frame(ORG_NAVN = org.virk.other$ORG, CVR = org.virk.other$CVR_NR)
# cvr.virk               <- data.frame(ORG_NAVN = org.virk$ORG, CVR = org.virk$CVR_NR)
# cvr.fond               <- data.frame(ORG_NAVN = as.character(org.fond$NAVN), CVR = org.fond$CVR)
# cvr.all                <- rbind(cvr.virk.other, cvr.virk, cvr.fond)
# cvr                    <- vector(length = nrow(rel.all))
# 
# for (i in 1:nrow(cvr.all))  cvr[which(rel.all$ORG_NAVN ==  cvr.all$ORG_NAVN[i])] <- cvr.all$CVR[i]
# 
# kilde            <- as.factor(rel.all$kilde)
# levels(kilde)    <- c("State", "Events", "Parliament", "Foundations", "Commissions", 
#                          "NGO", "State", "Corporations", "VL-networks") # Her er der et ækelt hack der tager nogle grupper ud der havde "" som kilde og angiver dem til stat.
# org              <- rel.all$ORG_NAVN
# 
# # TAGS
# 
# tag.frame <- rel.all[,grep("TAG", colnames(rel.all))]
# tag.frame <- apply(tag.frame, 2,as.character)
# tag.frame[tag.frame == ""] <- NA
# 
# nif          <- as.list(as.data.frame(t(tag.frame)))
# hurma        <- lapply(nif, na.omit)
# tag.label    <- unlist(lapply(hurma, paste, collapse = ", "))
# 
# 
# 
# # Output
# data             <- data.frame(NAME        = rel.all$NAVN_MATCH,
#                                AFFILIATION = rel.all$ORG_NAVN,
#                                ROLE        = rel.all$ROLE,
#                                GENDER      = gender.rel,
#                                DESCRIPTION = rel.all$BESKRIVELSE,
#                                SOURCE      = kilde,
#                                BIQ_LINK    = biq.link,
#                                CVR         = cvr,
#                                TAGS        = tag.label     
#                                )
# 
# 
# # Translated version
# # Export
# den                         <- data
# save(den, file = "~/soc.elite/data/den.rda")
# 

###########################################################################
# Power elite 2013 [pe13]
# ind             <- read.csv("~/My Dropbox/Elite/Data/Data/Individuals_elite.csv", sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8", dec = ".", na.strings = c("", NA))
# load("~/My Dropbox/R/hoved/saved_results")
# 
# ind             <- ind[order(ind$Name),]
# 
# # Check ordering
# all.equal(as.character(ind$Name), V(net.elite)$name)
# 
# # Order of levels in Sector_cat
# oo              <- c(
#   "Business: Top 200", 
#   "Business: Multiposition",
#   "Business: Medium-small",
#   "Business: Investment and Pensions", 
#   
#   "Interest groups: Employers and business",
#   "Interest groups: Unions",
#   
#   "Interest groups: Farming",
#   "Interest groups: Consumers",
#   
#   "State and politics: Royal court", 
#   "State and politics: Politics",
#   "State and politics: Public Officials", 
#   
#   "Science and education: University leaders",
#   "Science and education: Economists and political scientists", 
#   "Science and education: Other scientists", 
#   "Science and education: Education",
#   
#   "Culture and law: Culture and charities" , 
#   "Culture and law: Law"
# )
# 
# ind$Sector_order            <- factor(ind$Sector_order, levels = oo)
# 
# # Rownames
# rownames(ind) <- ind$Name
# 
# net.elite <- upgrade_graph(net.elite)
# 
# # Save
# pe13          <- ind
# save(pe13, net.elite, file = "~/soc.elite/data/pe13.rda")

###########################################################################
# Nested
# 
# indlejrede           <- read.csv("~/My Dropbox/Elite/Dataindsamling/Indlejrede organisationer.csv",
#                                  fileEncoding="UTF-8", as.is = TRUE, na.strings="")
# indlejrede           <- indlejrede[is.na(indlejrede$Kill) == TRUE & is.na(indlejrede$MATCH)==FALSE,]
# nested               <- indlejrede
# colnames(nested)     <- c("X", "X.1", "Nested.org", "Nested.in", "Overlap", 
#                           "Org.id.nested", "Org.id.nested.in", "Same.org", "Kill", 
#                           "Totally.nested.in.less.than.12", "Nested.in.more.than.12", 
#                           "MATCH")
# nested.latin         <- sapply(nested, iconv, from = "UTF-8", to = "latin1", sub = "byte")
# nested.utf           <- sapply(nested.latin, iconv, from = "latin1", to = "UTF-8")
# View(nested.latin)
# View(nested)
# table(is.na(nested))
# table(is.na(nested.latin))
# ###############################################################################
# # # Names.gender
# load("~/My Dropbox/R/Elite/Navne/names_gender")
# 
# ###############################################################################
# # # Postnumre
# postnumre <- read.csv("~/My Dropbox/R/Elite/Data/postnumre.csv", sep = ";", fileEncoding = "UTF-8")
# 
# # 
# # 
# save(nested, names.gender, postnumre, file = "~/soc.elite/R/sysdata.rda")
# # 
# ###############################################################################
# # Firmaets mænd - Directors 2008
# directors08 <- read.csv("~/My Dropbox/Elite/Data/Firmaets Mænd 2008/Data_directors_2008.csv", sep = ",", fileEncoding = "UTF-8", dec = ",")
# save(directors08, file = "~/soc.elite/data/directors08.rda")
