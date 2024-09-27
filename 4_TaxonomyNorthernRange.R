#===============================================================================
# TAXA LISTS NORTHEN RANGE (Appendix Tables)
# July 2024
#===============================================================================


#===============================================================================
# Libraries:--------------------------------------------------------------------
#===============================================================================
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table) 
library(taxize)  # for retrieving info on taxonomy & authorities 


rm(list=ls())
table_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/Tables"


#===============================================================================
# Fish:-------------------------------------------------------------------------
#===============================================================================
sps <- c("Awaous banana", "Dajaus monticola", "Astyanax bimaculatus", 
         "Anablepsoides hartii", "Andinoacara pulcher", "Ancistrus maracasae",
         "Corydoras aeneus", "Crenicichla frenata", "Corynopoma riisei", 
         "Cichlasoma taenia","Gymnotus carapo", "Hoplosternum littorale", 
         "Hoplias malabaricus", "Hypostomus robinii", "Hemigrammus unilineatus", 
         "Hemibrycon taeniurus", "Odontostilbe pulchra", "Oreochromis niloticus", 
         "Oreochromis mossambicus", "Poecilia reticulata","Roeboides dientonito", 
         "Rhamdia quelen", "Steindachnerina argentea", "Synbranchus marmoratus")

fish_info <- classification(sps, db = "ncbi") # retrieve info with taxize
fish_info <- do.call(rbind, lapply(fish_info, function(x) {subset(x, rank %in% c("order", "family", "species"))}))
fish_info <- fish_info[,!names(fish_info) == "id"]
fish_info$record_id <- rep(c(1:24), each=3)
fish_info <- fish_info %>%
  pivot_wider(names_from = rank, values_from = name) # format for SM table
fish_info$species[fish_info$species=="Agonostomus monticola"] <- "Dajaus monticola"  # taxonomic update
fish_info$species[fish_info$species=="Osteogaster aeneus"] <- "Corydoras aeneus"     # taxonomic update


fish_auth <- gnr_resolve(sps, data_source_ids=11) # retrieve authorities with taxize
fish_info$authority <- fish_auth$matched_name[match(fish_info$species, fish_auth$user_supplied_name)]
fish_info <- fish_info[,!names(fish_info) == "record_id"]
fish_info$original <- NA
fish_info$original[fish_info$species=="Dajaus monticola"] <- "Agonostomus monticola" # taxonomic update
fish_info <- fish_info %>% relocate(original, .before=authority)
names(fish_info) <- str_to_title(names(fish_info))


write.csv(fish_info, 
          file=paste0(table_path, "/FishSpeciesTaxonomy.csv"), 
          row.names = F) # (basic info) table layout improved in word


#===============================================================================
# Invertebrates: ---------------------------------------------------------------
#===============================================================================
formatted_data_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FormattedData_Trinidad"
load(paste0(formatted_data_path, "/BenthicInv1.RData")) # Northern Range monitoring programme invertebrates
sort(unique(BenthicInv1$Classification))

fams <- sub(".*\\b(\\w+)$", "\\1", unique(BenthicInv1$Classification)) # retrieve more specific name
fams[fams=="spp"] <- "crustacea"                   # fix
fams[fams=="unknown"] <- "lepidoptera"             # fix
fams[fams=="calomoceratidae"] <- "calamoceratidae" # misspelling
fams[fams=="euthyplocidae"] <- "euthyplociidae"    # misspelling


fams <- str_to_title(fams)
family_info <- classification(fams, db = "ncbi")   # retrieve info with taxize
family_info <- do.call(rbind, lapply(family_info, function(x) {subset(x, rank %in% c("phylum", "class", "order", "family"))}))
#family_auth <- gnr_resolve(fams, data_source_ids=11) # authorities (GBIF) added directly on word doc
family_info <- family_info[,!names(family_info) %in% "id"]


family_info1 <- subset(family_info, rownames(family_info) %like% c("idae")) # records identified to family
family_info1$record_id <- rep(c(1:24), each=4)
family_info1 <- family_info1 %>%
  pivot_wider(names_from = rank, values_from = name)
family_info1 <- family_info1[,!names(family_info1) == "record_id"]


family_info2 <- subset(family_info, ! rownames(family_info) %like% c("idae")) # records broader than family
family_info2$record_id <- c(25,25,25,26,26,27,27, 28,28,28, 29, 30,30)
family_info2 <- family_info2 %>%
  pivot_wider(names_from = rank, values_from = name)
family_info2 <- family_info2[,!names(family_info2) == "record_id"]
family_info2$family <- NA

family_info <- as.data.frame(rbind(family_info1, family_info2)) # re-bind
family_info <- family_info[order(family_info[,1], family_info[,2], family_info[,3], family_info[,4]),]

write.csv(family_info, 
          file=paste0(table_path, "/InvFamilyTaxonomy.csv"), 
          row.names = F) # (basic info) table layout improved in word / authorities (GBIF) added on word doc


#===============================================================================
# Diatoms: ---------------------------------------------------------------------
#===============================================================================
# Very conservative key by Dr Amy Deacon:
# https://amydeacon.weebly.com/diatoms-of-the-northern-range.html



# End of script ################################################################

