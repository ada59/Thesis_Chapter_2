################################################################################
# Script to format biodiversity records Northern Range Database 
# (Fish & Macinv data)
# AFE
# June 2023 (Revised in May 2024)
################################################################################


# Libraries:--------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(rfishbase)
library(readxl)
library(data.table)

rm(list=ls())
getwd()


rm(list=ls())

rawdatapath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/RawData_Trinidad"
path_formatted_outputs <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FormattedData_Trinidad"


# Read data: ===================================================================

# fish: ------------------------------------------------------------------------
fish <- read.csv2(paste0(rawdatapath,"/fishAFE2023_typos_corrected.csv"), h=T)
str(fish)
fishlengths <- read.csv2(paste0(rawdatapath,"/fish2023AFE_lengths.csv"), h=T) # sent a posteriori by Amy, typos corrected too
str(fishlengths)
identical(sort(fishlengths$weight), sort(fish$weight)) # TRUE (so only the order might be different)
# NOTE: continue with fish lengths file.

# inv: -------------------------------------------------------------------------
inv <- read.csv(paste0(rawdatapath,"/BioTIME/BioTIME_Trinidad_Invertebrates.csv"), h=T)

# dia: -------------------------------------------------------------------------
dia <- read.csv(paste0(rawdatapath,"/BioTIME/BioTIME_Trinidad_Diatoms.csv"), h=T)


# Format data ==================================================================

# Fish: ------------------------------------------------------------------------
head(fishlengths)

sort(unique(fishlengths$Field))   # comments on weight estimations
sort(unique(fishlengths$Field.2)) # 0
sort(unique(fishlengths$Field.3)) # 0
sort(unique(fishlengths$Field.4)) # 0

fish <- fishlengths
fish <- within(fish, rm(Field, Field.2, Field.3, Field.4, Fish.Family, Genus)) # rm uneeded cols


class(fish$Date)
fish$Date <- gsub("/", "_", fish$Date)
fish$Day <- str_split_fixed(fish$Date, "_", 3)[,1]
fish$Month <- str_split_fixed(fish$Date, "_", 3)[,2]
fish$Year <- str_split_fixed(fish$Date, "_", 3)[,3] # split info on date


fish <- fish %>% relocate(c(Day, Month, Year), .before=Species)
fish <- fish %>% relocate(Disturbed, .after=Site)
fish <- fish %>% relocate(c(number.caught, number.seen), .before=weight)
fish <- fish %>% relocate(length, .after =weight)
fish <- fish %>% relocate(c(females, males, juveniles), .after =length) 
fish <- fish %>% relocate(HCvsEF, .before = weight)
fish <- within(fish, rm(Date)) # reorganise data


sort(unique(fish$Year))
sum(fish$Year=="") # 1
# View(fish[fish$Year=="",])  # just the first obs entry in database, OK
fish <- fish[!fish$Year=="",] # make sure no blank records kept
fish %>% group_by(Year) %>% summarise(n_distinct(Site)) # OK


################################################################################
################################################################################
# Checks to fill in weights for seen inds in May 2024: -------------------------

# NOTES: Data filled in Excel (prev in Filemaker Pro)
# This year we did not use Filemaker Pro. License expired. 
# Guppy weights used: 0.04J, 0.1M & 0.2F (based on prev. data)
# Weights for seen macroinv determined as average ever seen for each sps (no LW
# relationships for macroinv)
# When weight indicated as average in the field, then av. of observed
# individuals of that species in the sampling observation session 
# or average ever observed for the sps depending on the case.
# Rare taxa: weight determined using Bayesian LW estimates from FishBase.
# Common taxa: Weight determined based on prev data entries or Bayesian
# ayesian LW estimates from FishBase.
# During this sampling, we obtained LW in the field for Syn, Riv, Anc, HemiB & Hopl.
# The estimates of "seen weights" will be standardised to one method in the future.

fish$weight <- as.numeric(fish$weight)

# retrieve av. weight M. cren (filled vals in Excel)
sum(fish$weight[fish$Species=="Macrobrachium crenulatum"]) 
sum(fish$number.caught[fish$Species=="Macrobrachium crenulatum"])
sum(fish$number.seen[fish$Species=="Macrobrachium crenulatum"])
677.5/152  # 4.46 --> 4.5g

# retrieve av. weight E.garmani (filled vals in Excel)
sum(fish$weight[fish$Species=="Eudaniela garmani"]) 
sum(fish$number.caught[fish$Species=="Eudaniela garmani"])
sum(fish$number.seen[fish$Species=="Eudaniela garmani"])
3755.1/261 # 14.39 --> 14.5g

# retrieve av. weight H malabaricus (filled vals in Excel)
sum(fish$weight[fish$Species=="Hoplias malabaricus"]) 
sum(fish$number.caught[fish$Species=="Hoplias malabaricus"])
sum(fish$number.seen[fish$Species=="Hoplias malabaricus"])
29515.4/(422+68) # 60.23551 --> 60g

# retrieve av. weight D monticola (see in Checks below for observation 
# from April 2011 in Lower Aripo Undisturbed)
sum(fish$weight[fish$Species=="Agonostomus monticola"]) 
sum(fish$number.caught[fish$Species=="Agonostomus monticola"])
sum(fish$number.seen[fish$Species=="Agonostomus monticola"])
38.2/3 # 12.73333 --> 13g

#View(fish[fish$Species=="Astyanax bimaculatus",])
#View(fish[fish$Species=="Hemibrycon taeniurus",])
#View(fish[fish$Species=="Rhamdia quelen",])
#View(fish[fish$Species=="Rivulus hartii",])
#View(fish[fish$Species=="Hoplias malabaricus",])
#View(fish[fish$Species=="Hypostomus robinii",])
#View(fish[fish$Species=="Synbranchus marmoratus",])
#View(fish[fish$Species=="Ancistrus maracasae",])
#View(fish[fish$Species=="Andinoacara pulcher",])
#View(fish[fish$Species=="Crenicichla frenata",])

# Seen guppies (Consult with AED)
#View(fish[fish$Species=="Poecilia reticulata" & fish$number.seen > 0,])
#View(fish[fish$Species=="Poecilia reticulata" & fish$number.caught > 0,])
#View(fish[fish$Species=="Rivulus hartii" & fish$number.seen > 0,])

################################################################################
################################################################################


# Append data from May 2024: ---------------------------------------------------
# function obtained from: https://www.geeksforgeeks.org/how-to-read-a-xlsx-file-with-multiple-sheets-in-r/
pathData2024 <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/RawData_Trinidad/2024/Fish2024.xlsx"
multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  # print data frame 
  print(data_frame) 
} 

fish2024 <- multiplesheets(pathData2024)
fish2024 <- as.data.frame(do.call(rbind, fish2024))
unique(fish2024$Field2)
fish2024 <- fish2024[,! names(fish2024) %in% c("Field", "Field2")]
identical(sort(names(fish)), sort(names(fish2024))) # TRUE


fish <- rbind(fish, fish2024)                           # FULL DATASET
fish %>% group_by(Year) %>% summarise(n_distinct(Site)) # OK
range(fish$sampleID)                                    # 1 405

str(fish)
fish$Day <- as.integer(fish$Day)
fish$Month <- as.integer(fish$Month)
fish$Year <- as.integer(fish$Year)

# Note on C. aeneus for Lower Aripo Dis 2024: ----------------------------------
# I didn't round this up to 100 because, although that was our initial estimate, it
# seems in the end we managed to sample most of the large shoal that was observed.

# Add sessions:-----------------------------------------------------------------

# 2010:
fish$Session <- ifelse(fish$Year==2010, 0, NA)
reps <- fish %>% group_by(Year, Month, Site) %>% summarise(nDay=n_distinct(Day), nID=n_distinct(sampleID)) # Repeatabilities
reps2 <- fish %>% group_by(Year, Month, Site) %>% distinct(Day)

# 12th and 15th August 2011 Acono
# 1st and 3rd June 2011 Maracas

# 2011:
fish$Session <- ifelse(fish$Year==2011 & fish$Month %in% c(01, 02), 1, fish$Session)
fish$Session <- ifelse(fish$Year==2011 & fish$Month %in% c(05, 06), 2, fish$Session)   # Repeatability in Maracas D & U in June 2011
fish$Session <- ifelse(fish$Year==2011 & fish$Month %in% c(08), 3, fish$Session)       # Repeatability in Acono D & U in August 2011
fish$Session <- ifelse(fish$Year==2011 & fish$Month %in% c(10, 11), 4, fish$Session) 

# 2012
fish$Session <- ifelse(fish$Year==2012 & fish$Month %in% c(01), 5, fish$Session)
fish$Session <- ifelse(fish$Year==2012 & fish$Month %in% c(05), 6, fish$Session) 
fish$Session <- ifelse(fish$Year==2012 & fish$Month %in% c(07), 7, fish$Session)       
fish$Session <- ifelse(fish$Year==2012 & fish$Month %in% c(10, 11), 8, fish$Session) 

# 2013
fish$Session <- ifelse(fish$Year==2013 & fish$Month %in% c(01, 02), 9, fish$Session)
fish$Session <- ifelse(fish$Year==2013 & fish$Month %in% c(04, 05), 10, fish$Session) 
fish$Session <- ifelse(fish$Year==2013 & fish$Month %in% c(07), 11, fish$Session)       
fish$Session <- ifelse(fish$Year==2013 & fish$Month %in% c(10, 11), 12, fish$Session) 

# 2014
fish$Session <- ifelse(fish$Year==2014 & fish$Month %in% c(01, 02), 13, fish$Session)
fish$Session <- ifelse(fish$Year==2014 & fish$Month %in% c(04, 05), 14, fish$Session) 
fish$Session <- ifelse(fish$Year==2014 & fish$Month %in% c(08), 15, fish$Session)       
fish$Session <- ifelse(fish$Year==2014 & fish$Month %in% c(10, 11, 12), 16, fish$Session) 

# 2015
fish$Session <- ifelse(fish$Year==2015 & fish$Month %in% c(01, 02), 17, fish$Session)
fish$Session <- ifelse(fish$Year==2015 & fish$Month %in% c(04, 05), 18, fish$Session) 
fish$Session <- ifelse(fish$Year==2015 & fish$Month %in% c(07, 08), 19, fish$Session)       

# 2016
fish$Session <- ifelse(fish$Year==2016 & fish$Month %in% c(08), 20, fish$Session)

# 2022
fish$Session <- ifelse(fish$Year==2022 & fish$Month %in% c(05), 21, fish$Session) # Different dates in Maracas bc electrofishing broke (OK)

# 2023
fish$Session <- ifelse(fish$Year==2023 & fish$Month %in% c(05), 22, fish$Session)

# 2024
fish$Session <- ifelse(fish$Year==2024 & fish$Month %in% c(05), 23, fish$Session)

sum(is.na(fish$Session)) # 0

# Add seasons:------------------------------------------------------------------
fish$Season <- ifelse(fish$Month %in% c(01, 02), 2, NA)
fish$Season <- ifelse(fish$Month %in% c(04, 05, 06), 3, fish$Season)
fish$Season <- ifelse(fish$Month %in% c(07, 08), 4, fish$Season)
fish$Season <- ifelse(fish$Month %in% c(10, 11, 12), 1 , fish$Season) 

sum(is.na(fish$Season)) # 0


# Rm unecessary 2010 and repeatabilities:---------------------------------------
reps <- fish %>% group_by(Year, Month, Site) %>% summarise(nDay=n_distinct(Day), nID=n_distinct(sampleID)) # Repeatabilities
reps2 <- fish %>% group_by(Year, Month, Site) %>% distinct(Day)

fish <- fish[!fish$Year==2010,]                     # rm session 0

length(unique(fish$Species[fish$Site=="Maracas u" & fish$Year==2011 & fish$Season==3 & fish$Day==01])) # 11, Pick
length(unique(fish$Species[fish$Site=="Maracas u" & fish$Year==2011 & fish$Season==3 & fish$Day==03])) # 9

length(unique(fish$Species[fish$Site=="Maracas d" & fish$Year==2011 & fish$Season==3 & fish$Day==01])) # 11, Pick
length(unique(fish$Species[fish$Site=="Maracas d" & fish$Year==2011 & fish$Season==3 & fish$Day==03])) # 8

length(unique(fish$Species[fish$Site=="Acono u" & fish$Year==2011 & fish$Season==4 & fish$Day==12])) # 6
length(unique(fish$Species[fish$Site=="Acono u" & fish$Year==2011 & fish$Season==4 & fish$Day==15])) # 7, Pick

length(unique(fish$Species[fish$Site=="Acono d" & fish$Year==2011 & fish$Season==4 & fish$Day==12])) # 7
length(unique(fish$Species[fish$Site=="Acono d" & fish$Year==2011 & fish$Season==4 & fish$Day==15])) # 9, Pick

fish <- fish[!(fish$Site %like% "Maracas" & fish$Year==2011 & fish$Season==3 & fish$Day==03),]# rm repeatability
fish <- fish[!(fish$Site %like% "Acono" & fish$Year==2011 & fish$Season==4 & fish$Day==12),]  # rm repeatability


# Retrieve key of sampling dates: ----------------------------------------------
keysamplingdates <- fish[,names(fish) %in% c("Session", "Day", "Month", "Year", "Site", "Disturbed")]
keysamplingdates <- distinct(keysamplingdates) # 368
save(keysamplingdates, file="keysamplingdates.RData")


# Checks: ----------------------------------------------------------------------
check <- fish %>% group_by(Site, Year, Month, Day) %>% summarise(n=n_distinct(sampleID)) # OK, always 1

# View(fish[fish$Species=="Poecilia reticulata" & fish$number.seen>0,])
fish[fish$Site=="Quare u" & fish$sampleID==172 & fish$Species=="Poecilia reticulata",]        # Rm seen
fish[fish$Site=="Upper Aripo d" & fish$sampleID==361 & fish$Species=="Poecilia reticulata",]  # Rm seen
fish[fish$Site=="Acono d" & fish$sampleID==377 & fish$Species=="Poecilia reticulata",]        # Rm seen
fish[fish$Site=="Quare u" & fish$sampleID==390 & fish$Species=="Poecilia reticulata",]        # Rm seen

# DECISION MAY 2024: Remove all "seen" records for guppies.  
# This "seen" records were only added for a few observations,
# but upon discussing it we decided it is more consistent to keep only records 
# sampled, because incomplete sampling of guppies in some sites is often unavoidable.

fish <- fish[!(fish$Species=="Poecilia reticulata" & fish$number.seen>0),]

range(fish$number.caught[fish$Species=="Poecilia reticulata"])           # 3 2886
sort(unique(fish$number.caught[fish$Species=="Poecilia reticulata"]))    # only a few above 1000


# Check D & U well assigned:
sort(unique(fish$Disturbed))
sort(unique(fish$Site[fish$Disturbed=="disturbed"]))   # OK
sort(unique(fish$Site[fish$Disturbed=="undisturbed"])) # OK

# Typos in latin names:
sort(unique(fish$Species))
fish$Species <- tolower(fish$Species) # OK
sort(unique(fish$Species))

# Remove unidentified record:
sum(fish$Species %in% c("a (possibly a. hartii)"))  # 1
fish <- fish[!fish$Species %in% c("a (possibly a. hartii)"),]

# check whether seen & observed make sense:
range(fish$number.caught[fish$number.seen>0]) # 0, OK
range(fish$number.seen[fish$number.caught>0]) # 0, OK

# Check weights: 
fish$weight <- as.numeric(fish$weight)
sum(is.na(fish$weight)) # 0, OK
sum(fish$weight==0)     # 0, OK
sum(fish$weight=="")    # 0, OK

# Correct typos:
fish$Species[fish$Species=="macrobranchium crenulatum"] <- "macrobrachium crenulatum"
fish$Species[fish$Species=="crenincichla frenata"] <- "crenicichla frenata"

# check whether seen & observed make sense:
range(fish$number.caught[fish$number.seen>0]) # 0, OK
range(fish$number.seen[fish$number.caught>0]) # 0, OK

sum(fish$number.seen==0 & fish$HCvsEF=="")    # 24
#View(fish[fish$number.seen==0 & fish$HCvsEF=="",]) # some typos, some just missing EF or HC entry
lengths <- c("12 INCH", "2 inches", "3 inches", "4 inch", "5 inches", "6 inches")
fish$number.seen[fish$number.seen==0 & fish$HCvsEF=="" & fish$length %in% lengths] <- fish$number.caught
fish$number.caught[fish$number.seen>0 & fish$number.caught>0] <- 0 # corrected because these records are likely seen, not caught
range(fish$number.caught[fish$number.seen>0]) # 0, OK
range(fish$number.seen[fish$number.caught>0]) # 0, OK

# A hartii to R hartii here:
fish$Species[fish$Species=="anablepsoides hartii"] <- "rivulus hartii"

# Check weights:
sum(is.na(fish$weight))  # 0, OK
sum(fish$weight==0)      # NA
sum(fish$weight==" ")    # NA (also NA for single space, OK)

# Change P. sphenops to P. sp & O mossambicus to O sp:
sum(fish$Species=="poecilia sphenops") # 1
sum(fish$number.caught[fish$Species=="poecilia sphenops"]) # 1
fish$Species[fish$Species=="poecilia sphenops"] <- "poecilia sp"
fish$Species[fish$Species=="oreochromis mossambicus"] <- "oreochromis sp"
sort(unique(fish$Species))

# Add Two D. monticola for 6/05/2011 in Lower Aripo U (see notes in abiotic data)
str(fish)
Mullet_LA_U_May2011 <- c("sampleID"=58,
                         "Site"="Lower Aripo u",
                         "Disturbed"="undisturbed",
                         "Day"=06,
                         "Month"=05,
                         "Year"=2011,
                         "Species"="agonostomus monticola",
                         "number.caught"=2,
                         "number.seen"=0,
                         "HCvsEF"=NA,
                         "weight"=13, # av. ever seen in obs up to 2023.
                         "length"="av. ever seen in obs up to 2023, added in 2024 after noticing note in sampling",
                         "females"=NA,
                         "males"=NA,
                         "juveniles"=NA,
                         "Session"=2,
                         "Season"=3)
fish <- rbind(fish, Mullet_LA_U_May2011)
# View(fish[fish$sampleID==58,])
str(fish)

fish$sampleID <- as.integer(fish$sampleID)
fish$Day <- as.integer(fish$Day)
fish$Month <- as.integer(fish$Month)
fish$Year <- as.integer(fish$Year)
fish$Session <- as.integer(fish$Session)
fish$Season <- as.integer(fish$Season)
fish$weight <- as.numeric(fish$weight)
fish$number.caught <- as.integer(fish$number.caught)
fish$number.seen <- as.integer(fish$number.seen)


# Taxonomic updates: -----------------------------------------------------------
sort(unique(fish$Species))

fish$Genus <- str_split_fixed(fish$Species, " ", 2)[,1]
fish$Species <- str_split_fixed(fish$Species, " ", 2)[,2]
fish$Genus <- str_to_title(fish$Genus)

fish$Species[fish$Genus %in% "Macrobrachium"] <- "spp" # pool all records of macrobranchium
fish$Genus[fish$Genus=="Agonostomus"] <- "Dajaus"
fish$Genus[fish$Genus=="Rivulus"] <- "Anablepsoides"
fish$Species[fish$Species=="riisei "] <- "riisei"
unique(fish$Species[fish$Genus=="Odontostilbe"])
fish$Species[fish$Genus=="Odontostilbe"] <- "pulchra"
fish$Species[fish$Species=="spp."] <- "spp"

sort(unique(fish$Species))
sort(unique(fish$Genus))

fish$Taxa <- paste0(fish$Genus, " ", fish$Species)    # add Taxa column
fish <- fish[!fish$Taxa=="Poecilia sp",]              # rm unknown record

sort(unique(fish$Species))
sort(unique(fish$Genus))


# Create fish & macinv datasets: -----------------------------------------------

sum(fish$Genus %in% c("Macrobrachium", "Eudaniela", "Atya"))  # 599 records
str(fish)
fish$TotalB <- ifelse(fish$number.seen > 0, (fish$weight * fish$number.seen), (fish$number.caught * fish$weight))
fish$TotalA <- ifelse(fish$number.seen == 0, fish$number.caught, fish$number.seen) # very large number of guppies, appears accurate to rawdata!


# Unaggregated data:
unaggFishMacroInv2024 <- fish
range(unaggFishMacroInv2024$TotalA)
range(unaggFishMacroInv2024$TotalB)


# Aggregated Data (inc Macroinv):
sum(fish$number.caught[fish$number.seen>0]>0)    # 0
sum(fish$number.seen[fish$number.caught>0]>0)    # 0 
sum(fish$number.seen==0 & fish$number.caught==0) # 0

aggFishMacroInv2024 <- fish %>% group_by(Site, Day, Month, Year, Genus, Species, Session, Season) %>%
  summarise(Biomass=sum(TotalB), Abundance=sum(TotalA))
aggFishMacroInv2024 <- as.data.frame(aggFishMacroInv2024)
aggFishMacroInv2010_15 <- as.data.frame(subset(aggFishMacroInv2024, aggFishMacroInv2024$Year < 2016))

range(aggFishMacroInv2024$Abundance)
range(aggFishMacroInv2010_15$Abundance)
range(aggFishMacroInv2024$Biomass)
range(aggFishMacroInv2010_15$Biomass)  


# Aggregated Data (only Fish):
sort(unique(unaggFishMacroInv2024$Genus))

unaggFish2024 <- subset(unaggFishMacroInv2024, !unaggFishMacroInv2024$Genus %in% c("Atya", "Macrobrachium", "Eudaniela"))
aggFish2024 <- subset(aggFishMacroInv2024, !aggFishMacroInv2024$Genus %in% c("Atya", "Macrobrachium", "Eudaniela"))
aggFish2010_15 <- subset(aggFishMacroInv2010_15, !aggFishMacroInv2010_15$Genus %in% c("Atya", "Macrobrachium", "Eudaniela"))

str(unaggFishMacroInv2024)
str(aggFishMacroInv2024)
str(aggFishMacroInv2010_15)


save(unaggFishMacroInv2024, file=paste0(path_formatted_outputs, "/unaggFishMacroInv2024.RData"))
save(aggFishMacroInv2024, file=paste0(path_formatted_outputs, "/aggFishMacroInv2024.RData"))
save(aggFishMacroInv2010_15, file=paste0(path_formatted_outputs, "/aggFishMacroInv2010_15.RData"))

save(unaggFish2024, file=paste0(path_formatted_outputs, "/unaggFish2024.RData"))
save(aggFish2024, file=paste0(path_formatted_outputs, "/aggFish2024.RData"))
save(aggFish2010_15, file=paste0(path_formatted_outputs, "/aggFish2010_15.RData"))


################################################################################
# NOTES FISH:###################################################################
# 1 ) There are two repeatabilities in this series: 12th and 15th August 2011 
# Acono and 1st and 3rd June 2011 Maracas 
# 2) Picked repeatabilities: most parsimonious option --> session when more species were reported
# 3) multiple taxonomic updates made 
# 4) Update to more guppies & rivulus in 2024?


# Check differences with previous files: =======================================
# After removing "seen" records for guppies there might also be a difference 
# in the total count & biomass of guppies for Quare u in April 2013.

dtv1 <- read.csv(paste0(rawdatapath, "/BioTIME/BioTIME_dataset_Trinidad_Fish_Survey.csv"), h=T)
sort(unique(dtv1$species))
dtv1$species[dtv1$species=="Agonostomus monticola"] <- "Dajaus monticola"
dtv1$species[dtv1$species=="Poecilia sphenops"] <- "Poecilia sp"
dtv1$species[dtv1$species=="Oreochromis mossambicus"] <- "Oreochromis sp"

dtv1$Genus <- str_split_fixed(dtv1$species, " ", 2)[,1]
dtv1$Species <- str_split_fixed(dtv1$species, " ", 2)[,2]

dtv1$Day <- str_split_fixed(dtv1$date, "/", 3)[,1]
dtv1$Month <- str_split_fixed(dtv1$date, "/", 3)[,2]
dtv1$Year <- str_split_fixed(dtv1$date, "/", 3)[,3]

dtv1$Day[dtv1$Year==2011 & dtv1$Month=="06" & dtv1$site=="MA1"] # 03
dtv1$Day[dtv1$Year==2011 & dtv1$Month=="08" & dtv1$site=="AC1"] # 15

str(aggFish2010_15)
aggFish2010_15 <- aggFish2010_15[!(aggFish2010_15$Day=="12" & aggFish2010_15$Year=="2011" & aggFish2010_15$Month=="08"),]
aggFish2010_15 <- aggFish2010_15[!(aggFish2010_15$Day=="01" & aggFish2010_15$Year=="2011" & aggFish2010_15$Month=="06"),]
aggFish2010_15 <- aggFish2010_15[!aggFish2010_15$Year==2010,]
#View(aggFish2010_15)
#View(dtv1)


# First checks of formatting:
str(dtv1)
dtv1$Day <- as.integer(dtv1$Day)
dtv1$Month <- as.integer(dtv1$Month)
dtv1$Year <- as.integer(dtv1$Year)
identical(sort(unique(dtv1$Species)), sort(unique(aggFish2010_15$Species))) # TRUE
identical(sort(unique(dtv1$Day)), sort(unique(aggFish2010_15$Day)))         # TRUE


# Random check:
#View(dtv1[dtv1$Year=="2012" & dtv1$Genus=="Dajaus",])                                # OK
#View(aggFish2010_15[aggFish2010_15$Year=="2012" & aggFish2010_15$Genus=="Dajaus",])  # Ok

#View(dtv1[dtv1$Year=="2014" & dtv1$Genus=="Rhamdia",])                                        # OK
sum(dtv1$abundance[dtv1$Year==2014 & dtv1$Genus=="Rhamdia"])                                # OK
#View(aggFish2010_15[aggFish2010_15$Year=="2014" & aggFish2010_15$Genus=="Rhamdia",])          # Ok
sum(aggFish2010_15$Abundance[aggFish2010_15$Year==2014 & aggFish2010_15$Genus=="Rhamdia"])  # OK


# Global check:
c1 <- dtv1 %>% group_by(Genus, Species, Year) %>% summarise(B=sum(biomass), A=sum(abundance))
c2 <- aggFish2010_15 %>% group_by(Genus, Species, Year) %>% summarise(B=sum(Biomass), A=sum(Abundance))

cF <- left_join(c1, c2, by=c("Genus", "Species", "Year")) # Only 1 difference biomass A. maracasae 2012

#View(dtv1[dtv1$Year==2012 & dtv1$Genus=="Ancistrus" & dtv1$Month=="10",])
#View(aggFish2010_15[aggFish2010_15$Year==2012 & aggFish2010_15$Genus=="Ancistrus" & aggFish2010_15$Month=="10",])

# The difference is a minor biomass difference in 
# Lower Aripo d
# 31
# 10
# 2012
# Ancistrus
# maracasae
# Everything is the same otherwise.

check <- subset(fish, fish$Species=="maracasae" & fish$Year==2012 & fish$Site=="Lower Aripo d" & fish$Month==10)
sum(check$TotalB) # OK, not sure what the issue might have been, but very minor difference.
################################################################################
################################################################################


# Inv: -------------------------------------------------------------------------
str(inv)
sum(is.na(inv)) # 0

range(inv$timestep) # already named, Jan 2011 to August 2015 (yet the replicabilities chosen for 2011 are unknown)
range(inv$abundance) # 1 1616

# inv, taxonomy: ---------------------------------------------------------------
length(unique(inv$Classification)) # 47
sort(unique(inv$Classification))


# LARGER GROUPS:
# Hydracarina (unranked under order Trombidiformes)
# Crustacea (subphylum)
# Collembola (subclass)
# Coleoptera (order)
# Diptera  (order)
# Ephemeroptera (order)
# Hemiptera (order)
# Lepidoptera  (order)
# Odonata (order)
# Plecoptera (order)
# Trichoptera (order)
# Insecta (class)
# Oligochaeta
# Turbellaria


inv$Classification <- tolower(inv$Classification) # fix same names with differences in capital letters
inv$Classification[inv$Classification=="turbellaria turbellaria"] <-"turbellaria"

sum(inv$Classification=="crustacea decapoda unknown") # 2
sum(inv$Classification=="crustacea unknown")          # 18

inv$Classification[inv$Classification %in% c("crustacea decapoda unknown", "crustacea unknown")] <- "crustacea spp"

inv$Classification[inv$Classification=="oligochaeta unknown"] <- "oligochaeta"
inv$Classification[inv$Classification=="entognatha collembola unknown"] <- "entognatha collembola"
# "entognatha collembola unknown" to "entognatha collembola"
# "oligochaeta unknown" to ""oligochaeta"
sort(unique(inv$Classification))


sum(inv$Classification=="insecta coleoptera unknown") # 75 
sum(inv$Classification=="insecta diptera unknown")    # 18 
sum(inv$Classification=="insecta unknown")            # 15 
sum(inv$Classification=="insecta trichoptera unknown")# 3
3+75+18+15    # 111 occurrences
111/6104*100  # 1.8 %

# Remove all insecta unknown:
unknowninsecta <- c("insecta coleoptera unknown", "insecta diptera unknown",
                    "insecta unknown", "insecta trichoptera unknown")
inv1 <- inv[!inv$Classification %in% unknowninsecta,]
length(unique(inv1$Classification))  # 30
sort(unique(inv1$Classification))
inv1 <- inv1 %>% group_by(timestep, Site, Classification) %>% summarise(abundance=sum(abundance))

# Keep as insecta spp.:
inv2 <- inv
inv2$Classification <- ifelse(inv2$Classification %in% unknowninsecta, "insecta spp", inv2$Classification)
length(unique(inv2$Classification))  # 31
sort(unique(inv2$Classification))
inv2 <- inv2 %>% group_by(timestep, Site, Classification) %>% summarise(abundance=sum(abundance))

BenthicInv1 <- inv1
BenthicInv2 <- inv2
save(BenthicInv1, file=paste0(path_formatted_outputs, "/BenthicInv1.RData"))  # no insecta unknwons
save(BenthicInv2, file=paste0(path_formatted_outputs, "/BenthicInv2.RData"))  # with insecta spp

################################################################################
# NOTES INVERTEBRATES:##########################################################
# Some unknown records pooled or removed after consulting with Amy.


# Dia: -------------------------------------------------------------------------
str(dia)
sum(is.na(dia)) # 0

range(dia$timestep)  # already named, Jan 2011 to August 2015 (yet the replicabilities chosen for 2011 are unknown)
range(dia$abundance) # 1 3408

# dia, taxonomy: ---------------------------------------------------------------
length(unique(dia$morphospecies)) # 26 
unique(dia$morphospecies)

#"A" (F)  "AE"(F) "AG" (F) "AH"(F) "AL"(F) "AM" (F) "AP"(F) "AQ" (F) "AV"(F) 
#"BB"(F) "BD" (F) "BJ" (F) "BK"(F) "BT"(F) "BV" (F)
#"C" (F)  "CB"(F) "CC" (F) "CI" (F)
#"D"(F)  "E" (F) "G"(F)  "H"(F)  "L"(F)  "N"(F)  U

sum(dia$morphospecies=="U")
unique(dia$site[dia$morphospecies=="U"])
unique(dia$timestep[dia$morphospecies=="U"])

names(dia) <- c("timestep", "site", "taxa", "abundance")

Diatoms <- dia
save(Diatoms, file=paste0(path_formatted_outputs, "/Diatoms.RData"))

################################################################################
# NOTES DIATOMS:################################################################
# 1) There are 2 lists: a more conservative and less conservative list.
# The more conservative list grouped together what were initially given different letters, 
# as they were fairly similar and we could not be certain that they were not just 
# variations of the same species. 

# 2) The PNAS dataset is the conservative list

# 3) Morphospecies U is the only one not in the key, however after consulting with
# Amy, this must be a key error and "U" is a legit category. She said is NOT likely "Unkown".

# 4) Morphospecies key in rawdatapath (conservative is ConListvCL)

# 5) "F" means found in key

# 6) PENDING: ask Amy for the downloadable database to see if I can disentangle 
# what U is (perhaps another letter in diatomConList)


# End of script ################################################################
################################################################################
