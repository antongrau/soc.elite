#' Danish Elite Network
#' 
#' A affiliation network
#'
#' @name den
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
# ###########################################################################
# # Nested
# 
# indlejrede           <- read.csv("~/My Dropbox/Elite/Dataindsamling/Indlejrede organisationer.csv",
#                                  fileEncoding="UTF-8", as.is = TRUE, na.strings="")
# indlejrede           <- indlejrede[is.na(indlejrede$Kill) == TRUE & is.na(indlejrede$MATCH)==FALSE,]
# nested               <- indlejrede
# colnames(nested)     <- c("X", "X.1", "Nested.org", "Nested.in", "Overlap", 
#                           "Org.id.nested", "Org.id.nested.in", "Same.org", "Kill", 
#                           "Totally.nested.in.less.than.12", "Nested.in.more.than.12", 
#                           "MATCH")
# 
# 
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
