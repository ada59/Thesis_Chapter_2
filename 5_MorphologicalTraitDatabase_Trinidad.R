#===============================================================================
# NORTHERN RANGE MORPHOLOGICAL FISH TRAIT DATA
# Final version June 2024 
#===============================================================================


#===============================================================================
# Sources: ---------------------------------------------------------------------
#===============================================================================
# Thomas, R & the Guidebook Team (2017) Data Analysis with R Statistical Softwar, Eco-explore.
# https://cran.r-project.org/web/packages/factoextra/readme/README.html 
# http://www.sthda.com/english/wiki/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7851 



# NOTES TAXONOMY ###############################################################
# Ancistrus maracasae is a synonym of Ancistrus trinitatis according to 
# FishBase & the Eschmeyer's Catalog of Fishes.
# Agonostomus monticola is a synonym of Dajaus monticola.
# A new study indicates that Crenicichla frenata is Saxatila frenata (Varella et al 2023)
################################################################################



#===============================================================================
# Libraries: -------------------------------------------------------------------
#===============================================================================
library(dplyr)
library(tidyverse)
library(stringr)
library(data.table)  # packages for data manipulation
library(rfishbase)   # retriving max. SL 
library(corrplot)    # correlations visualisation
library(missForest)  # missing value imputations
library(factoextra)  # trait space visualisation
library(FactoMineR)  # trait space visualisation
library(ggrepel)     # text repel
library(cluster)     # Euclidean distances

rm(list=ls())


#===============================================================================
# Paths: -----------------------------------------------------------------------
#===============================================================================
biodiv_data_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FormattedData_Trinidad"
trait_data_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_Thesis_TrinidadAnalyses/Traits/All_2022_2023/0_Traits/" # edit to final trait data location for thesis
formatted_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FormattedTrait_Trinidad"
Figure_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/Figures"


#===============================================================================
# READ DATA --------------------------------------------------------------------
#===============================================================================

# Data read are used to create the final UWI FIsh Morphological trait dataset.
# Trait data from the FISHMORPH Database is also retrieved for comparison.


## Measurements UWI Museum Fish Collection:-------------------------------------
file_names <- as.list(dir(path = trait_data_path, pattern="ldfF_*"))
file_names <- lapply(file_names, function(x) paste0(trait_data_path, x))
uwi_mes <- sapply(file_names, function(x) mget(load(x)), simplify = TRUE)

uwi_mes <- as.data.frame(do.call(rbind, uwi_mes))
sum(is.na(uwi_mes[,c(2:15)]))                     # 4total, Jls of R. quelen (395), 
                                                  # H. littorale (349A & 356)
                                                  # H. robinii (Phillip2013)
uwi_mes$HL[uwi_mes$Species=="S_marmoratus"] <- NA # 3 NAs, could not spot operculum aperture

(4+3)/(89*14)*100                                 # 0.56 (4 Jls & 3 Hls - S marmoratus)


## Biomass NR programme:--------------------------------------------------------
load(paste0(biodiv_data_path, "/unaggFish2024.RData")) # unaggregated data

sort(unique(unaggFish2024$Genus))
sort(unique(unaggFish2024$Species))

MaxBio <- unaggFish2024 %>% group_by(Genus, Species) %>% summarise(MaxBio=max(weight)) # max. recorded weight (in g) for an individual over all years of sampling
MaxBio$Abbv <- paste0(substring(MaxBio$Genus, 1, 1), "_", MaxBio$Species)



## Max SL, from FishBase:-------------------------------------------------------
fishbaseMBls <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FishBaseMBls"

trini_sps <- c("Awaous banana", "Dajaus monticola", "Astyanax bimaculatus", 
               "Anablepsoides hartii", "Andinoacara pulcher", "Ancistrus maracasae",
               "Corydoras aeneus", "Crenicichla frenata", "Corynopoma riisei", 
               "Cichlasoma taenia","Gymnotus carapo", "Hoplosternum littorale", 
               "Hoplias malabaricus", "Hypostomus robinii", "Hemigrammus unilineatus", 
               "Hemibrycon taeniurus", "Odontostilbe pulchra", "Oreochromis niloticus", 
               "Oreochromis mossambicus", "Poecilia reticulata","Roeboides dientonito", 
               "Rhamdia quelen", "Steindachnerina argentea", "Synbranchus marmoratus")

#mbls <- rfishbase::estimate(trini_sps)
#save(mbls, file=paste0(fishbaseMBls,"/mbls.RData"))
load(paste0(fishbaseMBls,"/mbls.RData"))              # had to pre-save because connecting to the FishBase API causes a random R error "Error:cannot have attributes on a CHARSXP"

sum(is.na(mbls$MaxLengthTL))  # 0
sum(is.na(mbls$MaxLengthSL))  # 0
mbls$Abbrev <- paste0(substring(mbls$Species, 1, 1), "_", str_split_fixed(mbls$Species, " ", 2)[,2])



## Trait Data from FISHMORPH:---------------------------------------------------
# NOTE: A maracasae not in FMORPH 
trini_sps <- c(trini_sps, "Agonostomus monticola") # D monticola is A monticola in FM
fmo <- read.csv2("C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FishMORPH/FISHMORPH_Database.csv", h=T)
names(fmo)[names(fmo)=="Genus.species"] <- "Genus_species"
fmo_Trinidad <- subset(fmo, fmo$Genus_species %in% trini_sps) # OK
sum(is.na(fmo_Trinidad[,c(6:15)])) # 4
4/(10*23)*100                      # 1.7%
fmo_Trinidad[,c(6:15)] <- as.data.frame(sapply(fmo_Trinidad[,c(6:15)], as.numeric))



#===============================================================================
# Formatting UWI Data ----------------------------------------------------------
#===============================================================================
names(uwi_mes)
str(uwi_mes)

uwi_mes[, c(2:15)] <- as.data.frame(sapply(uwi_mes[, c(2:15)], as.numeric))



## Template: -------------------------------------------------------------------
uwi_Trinidad<- data.frame(matrix(ncol=17, nrow=89)) # template to fill in trait data
names(uwi_Trinidad) <- c("Pic", "Species", "Mouth", "M_F", "MaxBioNR", "MBl", 
                         "BEl", "VEp", "REs", "OGp", "RMl", "BLs", "PFv", 
                         "PFs", "CPt", "REs_h", "SNl_h")

# metadata
uwi_Trinidad[,1] <- uwi_mes$Pic
uwi_Trinidad[,2] <- uwi_mes$Species
uwi_Trinidad[,3] <- uwi_mes$Mouth
uwi_Trinidad[,4] <- uwi_mes$Sex

# max bio in NR
sort(unique(MaxBio$Abbv))        # O_sp
sort(unique(uwi_mes$Species))    # O_mossambicus & O_niloticus
sort(unique(mbls$Abbrev))        # O_mossambicus & O_niloticus
uwi_Trinidad[,5] <- as.numeric(MaxBio$MaxBio[match(uwi_Trinidad$Species, MaxBio$Abbv)])
uwi_Trinidad$MaxBioNR[uwi_Trinidad$Pic %in% c("2010_14_614_22", "2010_14_611A1_23", "2010_14_610A_23",
                                              "Oreochromis_niloticus_FishBase", "Oreochromis_mossambicus_TuiEtAl")] <- MaxBio$MaxBio[MaxBio$Abbv=="O_sp"]


uwi_Trinidad[,6] <- as.numeric(mbls$MaxLengthSL[match(uwi_Trinidad$Species, mbls$Abbrev)]) # Max SL from FishBase

# check if any larger max SL reported in NR data (obs or L-W):
# updated checking observed SL vals in rawdata
uwi_Trinidad$MBl[uwi_Trinidad$Species=="C_frenata"] <- 16.5  # (SL NR)
uwi_Trinidad$MBl[uwi_Trinidad$Species=="A_maracasae"] <- 10  # (SL NR)
uwi_Trinidad$MBl[uwi_Trinidad$Species=="H_taeniurus"] <- 9.5 # (SL NR)

# traits in FM
uwi_Trinidad[,7] <- uwi_mes$SL/uwi_mes$BD        # BEl: hydrodynamism
uwi_Trinidad[,8] <- uwi_mes$EP/uwi_mes$BD        # VEp: position of fish and/or its prey in the water column
uwi_Trinidad[,9] <- uwi_mes$EDv/uwi_mes$HD       # REs: visual acuity
uwi_Trinidad[,10] <- uwi_mes$Mo/uwi_mes$BD       # OGp: feeding position in the water column
uwi_Trinidad[,11] <- uwi_mes$Jl/uwi_mes$HD       # RMl: size of mouth and strength of jaw
uwi_Trinidad[,12] <- uwi_mes$HD/uwi_mes$BD       # BLs: hydrodynamism and head size
uwi_Trinidad[,13] <- uwi_mes$PFp/uwi_mes$BD      # PFv: pectoral fin use for swimming
uwi_Trinidad[,14] <- uwi_mes$PFl/uwi_mes$SL      # PFs: pectoral fin use for swimming
uwi_Trinidad[,15] <- uwi_mes$CAU_D/uwi_mes$PED_D # CPt: caudal propulsion efficiency through reduction of drag

# additional traits (Winemiller 1991):
uwi_Trinidad[,16] <- uwi_mes$EDh/uwi_mes$HL      # REs_h: eye diameter divided by head length
uwi_Trinidad[,17] <- uwi_mes$SNL/uwi_mes$HL      # SNl_h: snout length divided by head length

# updates:
uwi_Trinidad$CPt[uwi_Trinidad$Species %in% c("G_carapo", "S_marmoratus")] <- 1 # CPt should be 1 (see Brosse et al 2021, although FM reports CPt = 0)



## Pic names: ------------------------------------------------------------------
sort(unique(uwi_Trinidad$Pic)) # need to make uniform
class(uwi_Trinidad$Pic)
uwi_Trinidad$Pic <- plyr::revalue(uwi_Trinidad$Pic, c("iNaturalist_Cichlasoma_Taenia"="iNaturalist_Cichlasoma_taenia",
                                                  "Oreochromis_mossambicus_TuiEtAl"="TuiEtAl_Oreochromis_mossambicus",
                                                  "Oreochromis_niloticus_FishBase"="FishBase_Oreochromis_niloticus",
                                                  "Phillip_et_al_2013"="Phillip_et_al_2013_Hypostomus_robinii",
                                                  "Phillip_et_al_2013_AnablepsoidesHartii"="Phillip_et_al_2013_Anablepsoides_hartii",
                                                  "Phillip_et_al_2013_AncistrusMaracasae"="Phillip_et_al_2013_Ancistrus_maracasae",
                                                  "Phillip_et_al_2013_AndinoacaraPulcher"="Phillip_et_al_2013_Andinoacara_pulcher",
                                                  "Phillip_et_al_2013_AstyanaxBimaculatus"="Phillip_et_al_2013_Astyanax_bimaculatus",
                                                  "Phillip_et_al_2013_C_riisei_Male"="Phillip_et_al_2013_Corynopoma_riisei",
                                                  "Phillip_et_al_2013_CorydorasAeneus"="Phillip_et_al_2013_Corydoras_aeneus",
                                                  "Phillip_et_al_2013_DajausMonticola"="Phillip_et_al_2013_Dajaus_monticola",
                                                  "Phillip_et_al_2013_HemibryconTaeniurus"="Phillip_et_al_2013_Hemibrycon_taeniurus",
                                                  "Phillip_et_al_2013_HemigrammusUnilineatus"="Phillip_et_al_2013_Hemigrammus_unilineatus",
                                                  "Phillip_et_al_2013_HopliasMalabaricus"="Phillip_et_al_2013_Hoplias_malabaricus",
                                                  "Phillip_et_al_2013_O_pulchra"="Phillip_et_al_2013_Odontostilbe_pulchra",
                                                  "Phillip_et_al_2013_RoeboidesDientonito"="Phillip_et_al_2013_Roeboides_dientonito",
                                                  "Phillip_et_al_2013_SArgentea"="Phillip_et_al_2013_Steindachnerina_argentea",
                                                  "Phillip_et_al_PoeciliaReticulataM"="Phillip_et_al_Poecilia_reticulata",
                                                  "Valvo_et_al_b"="Valvo_et_al_b_Poecilia_reticulata",
                                                  "Valvo_et_al_c"="Valvo_et_al_c_Poecilia_reticulata",
                                                  "Valvo_et_al_d"="Valvo_et_al_d_Poecilia_reticulata"))



#===============================================================================
# Check outliers, values that might be a mistake: ------------------------------
#===============================================================================
sapply(fmo_Trinidad[,c(6:15)], function(x) {range(x, na.rm=T)})
sapply(uwi_Trinidad[, c(5:17)], function(x) {range(x, na.rm=T)}) 
#View(fmo_Trinidad)
#View(uwi_Trinidad)

# NOTES VISUAL CHECKS ##########################################################
# BEl: looks OK given knowledge of fish species in the NR. Double-checked 
# C.taenia, and definitely specimen bodies appear very deep, oval.
# VEp: classifications in fmo & uwi quite different for some species.
# REs: looks OK, uwi pretty consistent with fmo.
# OGp: looks OK, uwi pretty consistent with fmo.
# RMl: pretty consistent with fmo. Double check Jl is OK for H robinii & A maracasae?
# BLs: Oreochromis heads in uwi align more with O. moss in fmo, 
# everything else looks pretty consistent with fmo.
# PFv: for S. marmoratus is 0 in uwi instead of NA (in fmo), but overall pretty consistent with fmo.
# PFs: looks OK, uwi pretty consistent with fmo.
# CPt: looks OK, uwi pretty consistent with fmo. Set to 1 for G. carapo & S.marmoratus.

# From checks above:
# Corrected Body Depth for G carapo specimen 100. Somehow I largely over estimated 
# this measurement in the initial measurement taking.
# Added pictures of live/colour specimens for as many species as possible 
# (found for all except S.marmoratus)
################################################################################


# NOTES UNIQUE STRUCTURES ######################################################
# A maracasae & H. robinii: OGp & PFv, positions are 0, OK
# Diff uwi & fmo: given that mouth position and pectoral fin position 
# are ventral for Ancistrus and Hypostomus, OGp & PFv are 0 for these species
# (both in FMORPH & in the data collected from the UWI Zoology Museum specimens)
# For the Ancistrus & Hypostomus specimens in the UWI Zoology Museum, 
# ventral pictures could be taken, hence Jl is NOT 0, 
# and RMl is not 0 (this trait is 0 in FMORPH)
# C aeneus: OGp, position is 0, OK
# H littorale: OGp is not 0 (mouth of Corydoras & Hoplosternum are different), 
# RMl of 4 ind are NA in uwi
# S marmoratus: PFv & PFs are 0 because this species doesn't have pectoral fins,
# but PFv is NA in fmo.
################################################################################



#===============================================================================
# Imputing missing data --------------------------------------------------------
#===============================================================================

imputTraitsAv100 <- function(x){
  
  ntimes <- 100
  t_imp <- list()
  t_err <- list()
  
  for (i in 1:ntimes){
    mat <- as.matrix(x)
    miss <- missForest(mat)
    mat_imp <- miss$ximp
    mat_err <- miss$OOBerror
    t_imp[[i]] <- mat_imp
    t_err[[i]] <- mat_err
  }
  
  t_impm <- do.call(cbind, t_imp)
  t_impm <- array(t_impm, dim=c(dim(t_imp[[1]]), length(t_imp)))
  t_impm <- as.matrix(apply(t_impm, c(1, 2), mean))              # Mean across the 100 objects
  
  rownames(t_impm) <- rownames(x)
  colnames(t_impm) <- colnames(x)
  
  t_err <- round(mean(unlist(t_err)), 2)
  
  l <- list(t_impm, t_err)
  return(l)
  
} # av. of 100 missForest runs



## UWI DATASET: ----------------------------------------------------------------
rownames(uwi_Trinidad) <- uwi_Trinidad$Pic
uwi_Trinidad_pre_imp <- uwi_Trinidad[,!names(uwi_Trinidad) %in% c("Pic", "Species", "Mouth", "M_F")]
#uwi_Trinidad_imp <- imputTraitsAv100(uwi_Trinidad_pre_imp)                      # av. 100 imputations
#save(uwi_Trinidad, file=paste0(formatted_path, "/uwi_Trinidad.RData"))          # save raw
#save(uwi_Trinidad_imp, file=paste0(formatted_path, "/uwi_Trinidad_imp.RData"))  # save imp

load(paste0(formatted_path, "/uwi_Trinidad_imp.RData"))
uwi_Trinidad_imp <- as.data.frame(uwi_Trinidad_imp[[1]])
uwi_Trinidad_imp$Pic <- rownames(uwi_Trinidad_imp)
uwi_Trinidad_imp <- full_join(uwi_Trinidad[,c(1:4)], uwi_Trinidad_imp, by="Pic")


## FISHMORPH: ------------------------------------------------------------------
rownames(fmo_Trinidad) <- fmo_Trinidad$Genus_species
fmo_Trinidad_pre_imp <- fmo_Trinidad[,!names(fmo_Trinidad) %in% c("SpecCode", "Suporder", "Order", "Family",
                                                                  "Type.of.illustration", "Reference")]

fmo_Trinidad_pre_imp <- fmo_Trinidad_pre_imp[,!names(fmo_Trinidad_pre_imp) %in% "Genus_species",]
#fmo_Trinidad_imp <- imputTraitsAv100(fmo_Trinidad_pre_imp)
#save(fmo_Trinidad, file=paste0(formatted_path, "/fmo_Trinidad.RData"))
#save(fmo_Trinidad_imp, file=paste0(formatted_path, "/fmo_Trinidad_imp.RData"))

load(paste0(formatted_path, "/fmo_Trinidad_imp.RData"))
fmo_Trinidad_imp <- as.data.frame(fmo_Trinidad_imp[[1]])
fmo_Trinidad_imp$Genus_species <- rownames(fmo_Trinidad_imp)
fmo_Trinidad_imp <- fmo_Trinidad_imp %>% relocate(Genus_species, .before=MBl)



#===============================================================================
# Correlations (UWI Data) ------------------------------------------------------
#===============================================================================
par(mfrow = c(4, 4))
sapply(names(uwi_Trinidad_imp)[5:17], function(x) {
  hist(uwi_Trinidad_imp[[x]], main = x, xlab = x, col = "lightblue", border = "black") #hitograms to decide between Pearson & Spearman
}) 
dev.off() # use Spearman rank

file_path <- paste0(Figure_path, "/SpearmanCorrelationsIndividualTraitData.png")
png(width = 700, height = 700, file=file_path)
cors <- cor(uwi_Trinidad_imp[,c(5:17)], method = "spearman")
corrplot(cors, method = "number", type = "lower", diag = FALSE) 
dev.off() 


uwi_Trinidad_imp <- uwi_Trinidad_imp[,!names(uwi_Trinidad_imp) %in% c("MaxBioNR", 
                                                                      "REs_h",
                                                                      "SNl_h")] # Use MBl(discard MaxBioNR),discard REs_h & SNl_h (high corrs and no comparability)



#===============================================================================
# Formatting for PCA -----------------------------------------------------------
#===============================================================================

uwi_Trinidad_imp$Dataset <- "UWI"
uwi_Trinidad_imp$Type <- ifelse(uwi_Trinidad_imp$Pic %like% "2010_14", "UWI Specimen", "Online")
uwi_Trinidad_imp <- uwi_Trinidad_imp[, ! names(uwi_Trinidad_imp) %in% c("Pic", "M_F")]
uwi_Trinidad_imp <- uwi_Trinidad_imp %>% relocate(c(Dataset, Type), .before=Species)

fmo_Trinidad_imp$Species <- paste0(substring(fmo_Trinidad_imp$Genus_species, 1, 1), "_", str_split_fixed(fmo_Trinidad_imp$Genus_species, " ", 2)[,2])
fmo_Trinidad_imp <- fmo_Trinidad_imp[,!names(fmo_Trinidad_imp) %in% c("Genus_species")]
fmo_Trinidad_imp$Mouth <- NA
fmo_Trinidad_imp$Dataset <- "FMO"
fmo_Trinidad_imp$Type <- "FMO Online"
fmo_Trinidad_imp <- fmo_Trinidad_imp %>% relocate(c(Dataset, Type, Species, Mouth), .before=MBl)

merged_uwi_fmo <- as.data.frame(rbind(fmo_Trinidad_imp, uwi_Trinidad_imp))

merged_uwi_fmo$Species2 <- rep(NA, nrow(merged_uwi_fmo))
merged_uwi_fmo$Species[merged_uwi_fmo$Species=="A_monticola"] <- "D_monticola"
merged_uwi_fmo$Species2 <- substr(merged_uwi_fmo$Species, 1, 5)



#===============================================================================
# ALL INDIVIDUALS PCA (UWI + FMORPH) -------------------------------------------
#===============================================================================

pca_ind <- prcomp(merged_uwi_fmo[,c(5:14)], center = TRUE, scale. = TRUE) # PCA, data scale-centered
coords_ind <- pca_ind$x

# Variable contributions: ------------------------------------------------------
screeplot <- fviz_screeplot(pca_ind, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(pca_ind)
ggsave(screeplot, filename = paste0(Figure_path, "/ind_based_screeplot.jpg"), width=6, height=4)

ind_based_var <- fviz_pca_var(pca_ind, col.var="contrib", axes = c(1,2),
             gradient.cols = c("khaki", "#E7B800", "#FC4E07"),
             repel = TRUE 
) # trait mapping 1-2
ggsave(ind_based_var, filename = paste0(Figure_path, "/ind_based_var.jpg"), width=8, height=6)

fviz_pca_var(pca_ind, col.var="contrib", axes = c(3,4),
             gradient.cols = c("khaki", "#E7B800", "#FC4E07"),
             repel = TRUE 
) # trait mapping 3-4

# Contributions of variables to PC1
fviz_contrib(pca_ind, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(pca_ind, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC3
fviz_contrib(pca_ind, choice = "var", axes = 3, top = 10)

# Contributions of variables to PC4
fviz_contrib(pca_ind, choice = "var", axes = 4, top = 10)


## Individuals per species:-----------------------------------------------------
ValCols <- c("red1", "brown2", "coral1","indianred","darkorange",
             "goldenrod2", "khaki", "gold2",
             "yellowgreen", "lightgreen", "forestgreen", "mediumturquoise",
             "cyan", "dodgerblue", "blue3", "darkslateblue",
             "mediumpurple", "deeppink4", "deeppink", "pink3",
             "rosybrown1", "peachpuff1", "snow3", "lightskyblue4"
             )
ValShap <- c(0,1,2,3,15,
             18,17,8,15,16,
             17,18,18,1,2,
             3,4,5,6,8,
             15,16,17,11)

ind_based_sps_col <- fviz_pca_ind(pca_ind,
             label = "none",
             habillage = merged_uwi_fmo$Species2, # color by taxon
             repel = TRUE,invisible="quali",
) + scale_color_manual(values= ValCols) + scale_shape_manual(values=ValShap) +
  geom_point(aes(color=merged_uwi_fmo$Species2, shape=merged_uwi_fmo$Species2), size=3.5) # last line maybe not necessary
ggsave(ind_based_sps_col, filename = paste0(Figure_path, "/ind_based_sps_col.jpg"), width=8, height=6)


fviz_pca_ind(pca_ind, axes = c(3,4),
             label = "none",
             habillage = merged_uwi_fmo$Species2, # color by taxon
             repel = TRUE
) + scale_color_manual(values= ValCols) + scale_shape_manual(values=ValShap) +
  geom_point(aes(color=merged_uwi_fmo$Species2, shape=merged_uwi_fmo$Species2), size=3.5) # last line maybe not necessary


## Individuals per data type: --------------------------------------------------
fviz_pca_ind(pca_ind,
             label="none",
             habillage = merged_uwi_fmo$Type,
             addEllipses = TRUE, ellipse.type = "convex"
) +
  geom_point(aes(color=merged_uwi_fmo$Type, shape=merged_uwi_fmo$Type), size=3.5) # last line maybe not necessary
fviz_pca_ind(pca_ind, axes=c(3,4),
             label="none",
             habillage = merged_uwi_fmo$Type,
             addEllipses = TRUE, ellipse.type = "convex"
) +
  geom_point(aes(color=merged_uwi_fmo$Type, shape=merged_uwi_fmo$Type), size=3.5) # last line maybe not necessary

## Individuals per mouth aperture: ---------------------------------------------
fviz_pca_ind(pca_ind,
             label = T,
             habillage = merged_uwi_fmo$Species2, # color by taxon
             repel = T
) + scale_color_manual(values= ValCols) + scale_shape_manual(values=ValShap) +
  geom_text(aes(label = merged_uwi_fmo$Mouth), size=2, hjust=0, nudge_x = 0.1)
fviz_pca_ind(pca_ind, axes = c(3,4),
             label = T,
             habillage = merged_uwi_fmo$Species2, # color by taxon
             repel = T
) + scale_color_manual(values= ValCols) + scale_shape_manual(values=ValShap) +
  geom_text(aes(label = merged_uwi_fmo$Mouth), size=2, hjust=0, nudge_x = 0.1)



#===============================================================================
# FINAL TRAIT DATABASE ---------------------------------------------------------
#===============================================================================


## Dataset (trait averages): ---------------------------------------------------
traits <- names(uwi_Trinidad)
traits <- traits[! traits %in% c("Pic","Species","Mouth","M_F", 
                                 "MaxBioNR", "REs_h", "SNl_h")]
uwi_Trinidad <- uwi_Trinidad[,!names(uwi_Trinidad) %in% c("MaxBioNR", "REs_h", "SNl_h")] # rm after assessing correltions on ind data (above)
uwi_Trinidad_av <- uwi_Trinidad %>% group_by(Species) %>% summarise_at(traits, mean, na.rm = TRUE)

uwi_Trinidad_av$RMl[uwi_Trinidad_av$Species=="A_banana"]
mean(uwi_Trinidad$RMl[uwi_Trinidad$Species=="A_banana"])

uwi_Trinidad_av$PFs[uwi_Trinidad_av$Species=="H_robinii"]
mean(uwi_Trinidad$PFs[uwi_Trinidad$Species=="H_robinii"]) 

# Av RMl of H littorale (only 2 specimens)
# (0.2658677 + 0.2950347) / 2 # check
# Av RMl of R quelen (only 3 specimens)
# Av RMl of H robinii (only 3 specimens)

uwi_Trinidad_av$Species[uwi_Trinidad_av$Species %in% c("O_mossambicus", "O_niloticus")] <- "O_sp"
uwi_Trinidad_av <- uwi_Trinidad_av %>% group_by(Species) %>% summarise_at(traits, mean)    # O sp is av of O moss & O nil


## Correlations: ---------------------------------------------------------------
par(mfrow = c(3, 4))
sapply(names(uwi_Trinidad_av)[2:11], function(x) {
  hist(uwi_Trinidad_av[[x]], main = x, xlab = x, col = "lightblue", border = "black") #hitograms to decide between Pearson & Spearman
}) 
dev.off() # use Spearman

file_path <- paste0(Figure_path, "/SpearmanCorrelationsFinal.png")
png(width = 500, height = 500, file=file_path)
cors <- cor(uwi_Trinidad_av[,c(2:11)], method = "spearman")
corrplot(cors, method = "number", type = "lower", diag = FALSE) 
dev.off() 


#===============================================================================
# FINAL TRAIT DATABASE PCA -----------------------------------------------------
#===============================================================================
# https://stackoverflow.com/questions/36208909/how-to-calculate-centroids-in-pca

str(uwi_Trinidad_av)
testNormality <- scale(uwi_Trinidad_av[,c(2:11)])
hist(testNormality[,1]) # 1 & 2 not normally distributed
hist(log(testNormality[,1]), breaks = 5) # logging doesn't fix much...

pca <- prcomp(uwi_Trinidad_av[,c(2:11)], center = TRUE, scale = TRUE) # PCA, data scale-centered
coords <- pca$x

screeplot_final <- fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))
ggsave(screeplot_final, filename = paste0(Figure_path, "/final_screeplot.jpg"), width=6, height=4)

var <- get_pca_var(pca)
var$cor


## Variable contributions: -----------------------------------------------------
var12_final <- fviz_pca_var(pca, col.var="contrib", axes = c(1,2),
             gradient.cols = c("khaki", "#E7B800", "#FC4E07"),
             repel = TRUE 
) # trait mapping 1-2
ggsave(var12_final, filename = paste0(Figure_path, "/var12_final.jpg"), width=8, height=6)

fviz_pca_var(pca, col.var="contrib", axes = c(3,4),
             gradient.cols = c("khaki", "#E7B800", "#FC4E07"),
             repel = TRUE 
) # trait mapping 3-4

# Contributions of variables to PC1
cont1_final <- fviz_contrib(pca, choice = "var", axes = 1, top = 10)
ggsave(cont1_final, filename = paste0(Figure_path, "/cont1_final.jpg"), width=6, height=4)

# Contributions of variables to PC2
cont_2_final <- fviz_contrib(pca, choice = "var", axes = 2, top = 10)
ggsave(cont_2_final, filename = paste0(Figure_path, "/cont2_final.jpg"), width=6, height=4)

# Contributions of variables to PC3
fviz_contrib(pca, choice = "var", axes = 3, top = 10)

# Contributions of variables to PC4
fviz_contrib(pca, choice = "var", axes = 4, top = 10)


## Individuals per species:-----------------------------------------------------
ValCols23 <- c("red1", "brown2", "coral1","indianred","darkorange",
             "goldenrod2", "khaki", "gold2",
             "yellowgreen", "lightgreen", "forestgreen", "mediumturquoise",
             "cyan", "dodgerblue", "blue3", "darkslateblue",
             "mediumpurple", "deeppink4", "deeppink", "pink3",
             "rosybrown1", "snow3", "lightskyblue4"
)
ValShap23 <- c(0,1,2,3,15,
             18,17,8,15,16,
             17,18,18,1,2,
             3,4,5,6,8,
             15,16,11)


species_final <- fviz_pca_ind(pca, 
             label = "none",
             habillage = uwi_Trinidad_av$Species, # color by taxon
             repel = TRUE,
) + scale_color_manual(values= ValCols23) + scale_shape_manual(values=ValShap23)

ggsave(species_final, filename = paste0(Figure_path, "/species_final.jpg"), width=8, height=6)


#===============================================================================
# FINAL TRAIT DATA MATRIX ------------------------------------------------------
#===============================================================================
uwi_Trinidad_avE <- as.data.frame(uwi_Trinidad_av)
rownames(uwi_Trinidad_avE) <- uwi_Trinidad_avE$Species
uwi_Trinidad_avE <- uwi_Trinidad_avE[, !names(uwi_Trinidad_avE) %in% c("Species")]
uwi_Trinidad_avE <- as.data.frame(scale(uwi_Trinidad_avE, center = TRUE, scale = TRUE))

dist_Euc_av <- as.matrix(daisy(uwi_Trinidad_avE, metric = c("euclidean")))
save(dist_Euc_av, file=paste0(formatted_path, "/dist_Euc_av.RData"))

#euclidean_distances_pca <- dist(coords)
#dist_Euc_av2 <- as.matrix(euclidean_distances_pca) # idem dist_Euc_av


# check whther coords average idem Euclidean distance from av.: ----------------
#traits <- names(uwi_Trinidad_imp)
#traits <- traits[! traits %in% c("Dataset","Type","Species","Mouth")]
#uwi_Trinidad_imp$Species[uwi_Trinidad_imp$Species %in% c("O_mossambicus", "O_niloticus")] <- "O_sp"
#uwi_Trinidad_av3 <- uwi_Trinidad_imp %>% group_by(Species) %>% summarise_at(traits, mean)
#pcaB <- prcomp(uwi_Trinidad_imp[,c(5:14)], center = TRUE, scale = TRUE)
#coordsB <- pcaB$x
#groupsB <- uwi_Trinidad_imp$Species
#centroidsB <- as.data.frame(coordsB) %>%
#  mutate(group = groupsB) %>%
#  group_by(group) %>%
#  summarise(across(everything(), mean))
#centroid_coords <- centroidsB %>% select(-group)
#euclidean_distances_pcaB <- dist(centroid_coords)
#dist_Euc_av3 <- as.matrix(euclidean_distances_pcaB) # different from 1 & 2



#===============================================================================
# FMO DATABASE PCA -------------------------------------------------------------
#===============================================================================
pca_fmo <- prcomp(fmo_Trinidad_imp[,c(5:14)], center = TRUE, scale. = TRUE) # PCA, data scale-centered
fviz_contrib(pca_fmo, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_fmo, choice = "var", axes = 2, top = 10)
fviz_pca_ind(pca_fmo,
             label = fmo_Trinidad_imp$Species,
             repel = TRUE,
) + geom_text(aes(label = fmo_Trinidad_imp$Species), size=2, hjust=0, nudge_x = 0.1)
# Most substantial difference is C. frenata



#===============================================================================
# SENSITIVITY (GOWER DISTANCES) ------------------------------------------------
#===============================================================================
# https://doi.org/10.1111/j.0030-1299.2007.15894.x (between Gower & Euclidean)
# PCOA?
dist_Gow_av <- as.matrix(daisy(uwi_Trinidad_avE, metric = c("gower")))
save(dist_Gow_av, file=paste0(formatted_path, "/dist_Gow_av.RData"))


# End of script ################################################################






# RESIDUAL:
# PCA  & distance mats =========================================================
# Correlations (UWI vs FM) =====================================================



#(i <- fviz_pca_ind(pca,
#                   axes = c(1,2),
#                   col.ind = as.numeric(dt$Rank), # Color by the quality of representation
#                   gradient.cols = c("lightblue", "darkblue"), 
#                   repel = TRUE     # Avoid text overlapping
#))
#(i <- i + labs(color="Size"))
#(i <- i + geom_point(aes(shape=dt$Species)))




#(i2 <- fviz_pca_ind(P1,
#                    axes = c(1,2),
#                    col.ind = as.numeric(dt$Rank), # Color by the quality of representation
#                    gradient.cols = c("lightblue", "darkblue"), 
#                    repel = TRUE     # Avoid text overlapping
#))
#(i2 <- i2 + labs(color="Size"))
#(i <- i + geom_point(aes(shape=dt$Species)))



