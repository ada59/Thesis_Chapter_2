################################################################################
# Script to format biodiversity records Northern Range Database (Fish Data, L-W)
# AFE
# June 2023 (Revised in August 2023)
################################################################################


# Libraries:====================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(rfishbase)
library(data.table)
library(pracma)
library(patchwork)


rm(list=ls())
getwd()


# Read data: ===================================================================
load(paste0("C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FormattedData_Trinidad", "/unaggFish2024.RData"))
dt <- unaggFish2024

sort(unique(dt$length))     

dt$length2 <- dt$length
dt$length2 <- ifelse(dt$length %in% c("", " ", "0",
                                      "as large as one caught", "average",
                                      "AVERAGE", "average range",
                                      "large", "larger than one caught...",
                                      "NA", "Same as caught",
                                      "NS", "same as caught fish",
                                      "small class like in site",
                                      "very large"), NA, dt$length2)
unique(dt$number.seen)
dt$Measured <- ifelse(!is.na(dt$length2) & dt$number.seen==0, "Y", "N")
unique(dt$Measured)
# View(dt[dt$Measured=="Y",])



# NOTE: I have indicated the specimens that have been measured (TL or TL SL). 
# The script is not further updated from here on.



################################################################################
################################################################################

#dt$lengthTL <- rep(NA, nrow(dt))
#dt$lengthSL <- rep(NA, nrow(dt))


#sort(unique(dt$length2))
#dt$length_units <-rep(NA, nrow(dt))
#dt$length_units <- ifelse(dt$length2 %in% c("10", "12", "13", "14", "2.5", "6", "7", "8", "9"), "Unreported", dt$length_units)
#dt$length_units <- ifelse(is.na(dt$length2), "Unreported", dt$length_units)


# Inches:
dt$length2 <- gsub("INCHES", "I tl", dt$length2)
dt$length2 <- gsub("inches", "I tl", dt$length2)
dt$length2 <- gsub("Inches ", "I tl", dt$length2)
dt$length2 <- gsub("Inches", "I tl", dt$length2)
dt$length2 <- gsub("inch", "I tl", dt$length2)
dt$length2 <- gsub("INCH", "I tl", dt$length2)
dt$length2 <- gsub("inich", "I tl", dt$length2)

dt$length2[dt$length=="1.5I"] <- "1.5 I tl"
dt$length2[dt$length2=="1.5I"] <- "1.5 I tl"
dt$length2[dt$length=="13cm"] <- "13 cm tl"
dt$length2[dt$length=="25cm"] <- "25 cm tl"
dt$length2[dt$length=="30 cm"] <- "30 cm tl"
dt$length2[dt$length=="4cm"] <- "4 cm tl"
dt$length2[dt$length=="50cm"] <- "50 cm tl"
dt$length2[dt$length=="5I"] <- "5 I tl"
dt$length2[dt$length=="7cmtl"] <- "7 cm tl"
dt$length2[dt$length=="8TLcm"] <- "8 cm tl"
dt$length2[dt$length=="9-15cm"] <- "9-15 cm tl"
dt$length2[dt$length=="9cm"] <- "9 cm tl"
dt$length2[dt$length2=="10 tl"] <- "10 cm tl"
dt$length2[dt$length2=="10 tl cm"] <- "10 cm tl"
dt$length2[dt$length2=="10cm"] <- "10 cm tl"
dt$length2[dt$length2=="10 cm"] <- "10 cm tl"
dt$length2[dt$length2=="1.5I tl"] <- "1.5 I tl"
dt$length2[dt$length2=="12 cm"] <- "12 cm tl"
dt$length2[dt$length2=="14 cm"] <- "14 cm tl"
dt$length2[dt$length2=="15 tl"] <- "15 cm tl"
dt$length2[dt$length2=="16 cm"] <- "16 cm tl"
dt$length2[dt$length2=="18 cm"] <- "18 cm tl"
dt$length2[dt$length2=="2 cm"] <- "2 cm tl"
dt$length2[dt$length2=="2 cm(aprox 1 I tl)"] <- "2 cm tl"
dt$length2[dt$length2=="25 cm"] <- "25 cm tl"
dt$length2[dt$length2=="3 cm"] <- "3 cm tl"
dt$length2[dt$length2=="3.5 cm"] <- "3.5 cm tl"
dt$length2[dt$length2=="30cm"] <- "30 cm tl"
dt$length2[dt$length2=="34 cm"] <- "34 cm tl"
dt$length2[dt$length2=="4 cm"] <- "4 cm tl"
dt$length2[dt$length2=="5I tl"] <- "5 I tl"
dt$length2[dt$length2=="6 cm"] <- "6 cm tl"
dt$length2[dt$length2=="6 cm (aprox 2 I tl)"] <- "6 cm tl"
dt$length2[dt$length2=="6 TL cm"] <- "6 cm tl"
dt$length2[dt$length2=="7 tl"] <- "7 cm tl"
dt$length2[dt$length2=="7 tl cm"] <- "7 cm tl"
dt$length2[dt$length2=="8 tl cm"] <- "8 cm tl"
dt$length2[dt$length2=="9 tl cm"] <- "9 cm tl"
dt$length2[dt$length2=="AVERAGE / 8 I tl"] <- "8 I tl"
dt$length2[dt$length2=="TL=32, SL=26 (>300g)"] <- "32 TL, 26 SL"
dt$length2[dt$length2=="5 I tl/Top length class in site"] <- "5 I tl"

sort(unique(dt$length2))
dt$length2 %like% c("TL")
dt$length_units <- ifelse(dt$length2 %like% c("I"), "inch", dt$length_units)
dt$length_units <- ifelse(dt$length2 %like% c("cm") | dt$length2 %like% c("TL") | dt$length2 %like% c("SL"), "cm", dt$length_units)


dt$length_units <- ifelse(dt$length2 %like% c("as large as one caught"), "A", dt$length_units)
dt$length_units <- ifelse(dt$length2 %like% c("average"), "A", dt$length_units)
dt$length_units <- ifelse(dt$length2 %like% c("AVERAGE"), "A", dt$length_units)
dt$length_units <- ifelse(dt$length2 %like% c("average range"), "A", dt$length_units)
dt$length_units <- ifelse(dt$length2 %like% c("larger than one caught..."), "A", dt$length_units)
dt$length_units <- ifelse(dt$length2 %like% c("same as caught fish"), "A", dt$length_units)


sort(unique(dt$length_units))

#dt$length3 <- str_split_fixed()

# L-W: =========================================================

dt$Taxa <- paste0(dt$Genus, " ", dt$Species)
vec <- sort(unique(dt$Taxa)) # OK
vec

# ll <- rfishbase::length_length("Anablepsoides_hartii") # Not working (18-8-2023)
# lw <- rfishbase::length_weight("Anablepsoides_hartii") # Not working (18-8-2023)

dt$a <- rep(NA, nrow(dt))
dt$b <- rep(NA, nrow(dt))

# 1) Bayesian estimates from Fishbase (sourced from web 18/8/2023)

estimates <- read.csv2("estimatesLW_NorthernRange.csv", h=T)
identical(sort(unique(dt$Taxa)), sort(unique(estimates$Species))) # TRUE

dt$a <- as.numeric(estimates$a[match(dt$Taxa, estimates$Species)])
dt$b <- as.numeric(estimates$b[match(dt$Taxa, estimates$Species)])

sum(is.na(dt$a))   # 0, OK
sum(is.na(dt$b))   # 0, OK

# Formula : W = a Lb
# isolating L is L=nth root b of W/a
# isolating L is (W/a)^(1/b)
range(dt$weight)
str(dt)
dt$l_calc <- round((dt$weight/dt$a)^(1/dt$b), 1) 
View(dt$l_calc) # Seems very reasonable.

# 2) Empirical relationships: ----------------------------------

# From the Northern range: -------------------------------------
#TBC

# A) Common taxa:
a_hartii <- read.csv("LW_anablepsoides_hartii.csv", h=T)       # Anablepsoides hartii
h_malabaricus <- read.csv("LW_hoplias_malabaricus.csv", h=T)   # Hoplias malabaricus
p_reticulata <- read.csv("LW_poecilia_reticulata.csv", h=T)    # Poecilia reticulata
a_bimaculatus <- read.csv("LW_astyanax_bimaculatus.csv", h=T)  # Astyanax bimaculatus
c_aeneus <- read.csv("LW_corydoras_aeneus.csv", h=T)           # Corydoras aeneus
a_pulcher <- read.csv("LW_andinoacara_pulcher.csv")            # Andinoacara pulcher
c_frenata <- read.csv("LW_crenicichla_saxatilis.csv")          # C frenata (used C. saxatilis)
s_marmoratus <- read.csv("LW_synbranchus_marmoratus.csv", h=T) # Synbranchus marmoratus
r_quelen <- read.csv("LW_rhamdia_quelen.csv", h=T)             # Rhamdia quelen
h_robinii <- read.csv("LW_hypostomus_plecostomus.csv", h=T)    # H robinii (used H. plecostomus)
r_dientonito <- read.csv("LW_roeboides_dayi.csv", h=T)             # Roeboides dientonito(used R dayi)

# Ancistrus hoplogenys:
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1439-0426.2011.01895.x
# a=0.0188/b=2.790 Brazilian Negro River
# https://onlinelibrary.wiley.com/doi/full/10.1111/jai.12712
# a=0.0138 [0.0113–0.0168]/b=3.25 [3.11–3.40] French Guaiana

# Ancistrus leucostictus:
# https://onlinelibrary.wiley.com/doi/full/10.1111/jai.12712 (others available in this reference)
# a=0.0159 [0.0142–0.0179]/b=3.27 [3.18–3.35] French Guaiana

# Hemibrycon surinamensis:
# https://onlinelibrary.wiley.com/doi/full/10.1111/jai.12712
# a=0.0140 [0.0122–0.0160]/b=3.23 [3.15–3.31] French Guaiana


# B) Rare taxa:
h_littorale <- read.csv("LW_hoplosternum_littorale.csv", h=T) # Hoplosternum littorale
g_carapo <- read.csv("LW_gymnotus_carapo.csv", h=T)           # Gymnotus carapo
o_sp <- read.csv("LW_oreochromis_mossambicus.csv", h=T)       # Oreochromis sp (used mossambicus)
a_banana <- read.csv("LW_Awaous_banana.csv", h=T)             # Awaous banana
d_monticola <- read.csv("LW_Awaous_banana.csv", h=T)          # Dajaus monticola
h_unilineatus <- read.csv("LW_hemigrammus_unilineatus.csv", h=T) # Hemigrammus unilineatus


############### TBC


# Plots lengths ================================================
dt$Taxa2 <- paste0(substring(dt$Taxa, 1, 1), " ", str_split_fixed(dt$Taxa, " ", 2)[,2])
p1 <- dt %>%
  #mutate(text = fct_reorder(Taxa, l_calc)) %>% # Reorder data
  ggplot( aes(x=Taxa2, y=l_calc, color=Taxa2)) +
  geom_point(aes(x=as.factor(Taxa2), y=l_calc), shape=19, size=2, alpha=0.5) +
  #geom_boxplot(aes(x=as.factor(Taxa2), y=l_calc), width=2.5, size=0.3) +
  #scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE) +
  theme_bw() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Fish Species Lengths (Calculated in TL cm)")
p1 # Plot of calculated TLs by species


# Quartiles ====================================================
dtT <- dt %>% group_by(Taxa2) %>% summarise(nC=sum(number.caught), nS=sum(number.seen))
dtT$TotalInd <- dtT$nC + dtT$nS

dt$TotalIndSeries <- dtT$TotalInd[match(dt$Taxa2, dtT$Taxa2)]

dtq <- dt %>% group_by(Taxa2, TotalIndSeries) %>%
  summarise(Q0=quantile(l_calc,probs=0), Q25=quantile(l_calc,probs=0.25), Q33=quantile(l_calc,probs=0.33), 
            Q50=quantile(l_calc,probs=0.5), Q66=quantile(l_calc,probs=0.66), Q75=quantile(l_calc,probs=0.75), Q100=quantile(l_calc,probs=1))

max(dt$l_calc[dt$Species=="bimaculatus"])
max(dt$weight[dt$Species=="bimaculatus"])

max(dt$l_calc[dt$Species=="banana"]) # TRUE, D. phillip 2013 indicates max TL = 30 cm(300mm)
max(dt$weight[dt$Species=="banana"]) 


# Quartiles (Biodiversity data) ================================

dt$Q0 <- dtq$Q0[match(dt$Taxa2, dtq$Taxa2)]
dt$Q25 <- dtq$Q25[match(dt$Taxa2, dtq$Taxa2)]
dt$Q33 <- dtq$Q33[match(dt$Taxa2, dtq$Taxa2)]
dt$Q50 <- dtq$Q50[match(dt$Taxa2, dtq$Taxa2)]
dt$Q66 <- dtq$Q66[match(dt$Taxa2, dtq$Taxa2)]
dt$Q75 <- dtq$Q75[match(dt$Taxa2, dtq$Taxa2)]
dt$Q100 <- dtq$Q100[match(dt$Taxa2, dtq$Taxa2)]


dt$Group33 <- ifelse(dt$l_calc <= dt$Q33, "S", "M")
dt$Group33 <- ifelse(dt$l_calc > dt$Q66, "L", dt$Group33)
sum(is.na(dt$Group33))
sort(unique(dt$Group33))
table(dt$Group33)
dt$Code33 <- paste0(dt$Taxa2, "_", dt$Group33)

dt$Group25 <- ifelse(dt$l_calc <= dt$Q25, "S", NA)
dt$Group25 <- ifelse(dt$l_calc <= dt$Q50 & dt$l_calc > dt$Q25, "M1", dt$Group25)
dt$Group25 <- ifelse(dt$l_calc > dt$Q50 & dt$l_calc <= dt$Q75, "M2", dt$Group25)
dt$Group25 <- ifelse(dt$l_calc > dt$Q75, "L", dt$Group25)
sum(is.na(dt$Group25))
sort(unique(dt$Group25))
table(dt$Group25)
dt$Code25 <- paste0(dt$Taxa2, "_", dt$Group25)

dt$Group50 <- ifelse(dt$l_calc < dt$Q50, "S", "L")
dt$Code50 <- paste0(dt$Taxa2, "_", dt$Group50)

Test1 <- dt %>% group_by(Site, Day, Month, Year) %>%
  summarise(nS=n_distinct(Taxa2), nC33=n_distinct(Code33), nC25=n_distinct(Code25), n50=n_distinct(Code50))

dim(Test1[Test1$nC33  >  Test1$nC25,])
dim(Test1[Test1$nC33  <  Test1$nC25,])

range(Test1$nS)
mean(Test1$nS)
range(Test1$nC33)
mean(Test1$nC33)
range(Test1$nC25)
mean(Test1$nC25)
sum(Test1$n50 > Test1$nS)  # 347
sum(Test1$n50 < Test1$nS)  # 0
sum(Test1$n50 == Test1$nS) # 25
range(Test1$n50)           # 2 to 21


# Visual checks of occurrence change over time:

LAd <- subset(Test1, Test1$Site=="Lower Aripo d")
UAd <- subset(Test1, Test1$Site=="Upper Aripo d")

ACu <- subset(Test1, Test1$Site=="Acono u")
TUu <- subset(Test1, Test1$Site=="Turure u")

Test1$Date <- as.Date(with(Test1,paste(Year,Month,Day,sep="-")),"%Y-%m-%d")

Test1 <- Test1[Test1$Year < 2016,]
a <- ggplot(Test1, aes(x=Date, y=nS, group=Site)) +
  geom_line(size=0.5)
b <- ggplot(Test1, aes(x=Date, y=nC33, group=Site)) +
  geom_line(size=0.5)

a + b

a <- ggplot(Test1, aes(x=Date, y=nS, group=Site)) +
  geom_smooth(size=0.5, method = "lm", se=F)
b <- ggplot(Test1, aes(x=Date, y=nC33, group=Site)) +
  geom_smooth(size=0.5, method = "lm", se=F)

a + b # Very very similar shapes with any of the categorisations.

Test2 <- dt %>% group_by(Session, Taxa) %>%
  summarise(n_distinct(Group50))

Test2AG <- subset(Test2, Test2$Taxa=="Anablepsoides hartii")
Test2AG <- subset(Test2, Test2$Taxa=="Poecilia reticulata")  # Just one?¿?¿
Test2AG <- subset(Test2, Test2$Taxa=="Astyanax bimaculatus")
Test2AG <- subset(Test2, Test2$Taxa=="Hemibrycon taeniurus")
Test2AG <- subset(Test2, Test2$Taxa=="Crenicichla frenata")
Test2AG <- subset(Test2, Test2$Taxa=="Hoplias malabaricus")


# Length-Length Table ==========================================
# TBC
# sps <- rfishbase::length_length()
# fb_tbl("estimate")


# End of script ================================================