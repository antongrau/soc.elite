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


# ##################################################################################
# # Generate data 
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
# levels(kilde)    <- c(NA, "Events", "Parliament", "Foundations", "Commissions", 
#                          "NGO", "State", "Corporations", "VL-networks")
# org              <- rel.all$ORG_NAVN
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
#                                TAG1        = rel.all$TAG1,
#                                TAG2        = rel.all$TAG2,
#                                TAG3        = rel.all$TAG3,
#                                TAG4        = rel.all$TAG4,
#                                TAG5        = rel.all$TAG5,
#                                TAG6        = rel.all$TAG6,
#                                TAG7        = rel.all$TAG7         
#                                )
# 
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
#   "Culture and Law: Culture and charities" , 
#   "Culture and Law: Law"
# )
# 
# ind$Sector_order            <- factor(ind$Sector_order, levels = oo)
# 
# # Rownames
# rownames(ind) <- ind$Name
# 
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
