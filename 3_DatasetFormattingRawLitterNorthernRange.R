#===============================================================================
# RAW LITTER-TIME NORTHERN RANGE
# August 2024 
#===============================================================================



# ==============================================================================
# Libraries:--------------------------------------------------------------------
# ==============================================================================
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(rfishbase)
library(readxl)
library(data.table)
library(patchwork)

rm(list=ls())
getwd()


# NOTE: due to issue in reading & exporting FileMaker Pro Database files, no
# single fully updated file (i.e., data in multiple files)



# ==============================================================================
# Read data: -------------------------------------------------------------------
# ==============================================================================
rawdatapath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/RawData_Trinidad/Environmental"


## to retrieve data from 2022 & 2023:-------------------------------------------
# 2011-2015 also contained here, but instead retrived below
dt <- read.csv2(paste0(rawdatapath,"/env2023AFE.csv"), h=T)            
names(dt)
dt <- dt[names(dt) %in% c("Date", "Site", "Disturbed", "Raw.Litter")]

dt22 <- subset(dt, dt$Date %like% "2022") # subset 2022
length(unique(dt22$Date))                 # 9 (Maracas u re-sampled bs electro broke)
dt22$Session <- rep(46, nrow(dt22))

dt23 <- subset(dt, dt$Date %like% "2023") # subset 2023
length(unique(dt23$Date))                 # 8 
dt23$Session <- rep(50, nrow(dt23))


## data August 2016: -----------------------------------------------------------
# missing Lower Aripo raw litter records (missing sheet?)
dt16 <- read_excel("C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/RawData_Trinidad/Environmental/Env2016.xlsx", sheet="Sheet1")
dt16$Date <- as.Date(paste0(dt16$Year, "-", dt16$Month, "-", dt16$Day), "%Y-%m-%d")
dt16$Session <- rep(23, nrow(dt16))
dt16 <- dt16[names(dt16) %in% c("Date", "Site", "Disturbed", "Raw.Litter", "Session")]


## data for sampling in May 2024:-----------------------------------------------
dt24 <- read_excel("C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/RawData_Trinidad/Environmental/Env2024.xlsx", sheet="Sheet1")
dt24$Date <- as.Date(paste0(dt24$Year, "-", dt24$Month, "-", dt24$Day), "%Y-%m-%d")
dt24$Session <- rep(54, nrow(dt24))
dt24 <- dt24[names(dt24) %in% c("Date", "Site", "Disturbed", "Raw.Litter", "Session")]


## data 2011-2015: -------------------------------------------------------------
dt1115 <- read_excel("C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/RawData_Trinidad/Environmental/ExportUpTo2019_T3All/Biotime environmental data 7 Jun export.xlsx", sheet="test")
dt1115 <- dt1115[names(dt1115) %in% c("Date", "Site", "Disturbed", "Raw Litter")]


## for sessions 2011-15: -------------------------------------------------------
load("C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/FormattedData_Trinidad/aggFishMacroInv2010_15.RData") # for session



# ==============================================================================
# Add Session to 2011-2015 data: -----------------------------------------------
# ==============================================================================
forsess <- aggFishMacroInv2010_15 # retrieve session ID x date
forsess$Date <- as.Date(paste0(forsess$Year, "-", forsess$Month, "-", forsess$Day), "%Y-%m-%d") 
class(forsess$Date)

c1 <- forsess %>%
  group_by(Site, Session) %>%
  summarise(n=n_distinct(Date)) # always 1
c2 <- forsess %>%
  group_by(Session) %>%
  summarise(n=n_distinct(Date)) # always 8 except in session 16 & 6

sort(unique(forsess$Date[forsess$Session==6]))
#View(forsess[forsess$Session==6,])  # seems Quare in two different days, OK
#View(forsess[forsess$Session==16,]) # also seems Quare in two different days, OK


class(dt1115$Date)
dt1115$Date <- as.Date(as.character(dt1115$Date), "%Y-%m-%d")
dt1115 <- dt1115[!dt1115$Date %like% "2010",]

dt1115$Session <- forsess$Session[match(dt1115$Date, forsess$Date)]
sum(is.na(dt1115$Session))            # 4
#View(dt1115[is.na(dt1115$Session),]) # OK, these are the repeatabilities.
dt1115 <- dt1115[!is.na(dt1115$Session),]

c3 <- dt1115 %>%
  group_by(Session) %>%
  summarise(n=n_distinct(Date))       # idem above


## Append: ----------------------------------------------------------------------
names(dt1115)[names(dt1115)=="Raw Litter"] <- "Raw.Litter"
dt <- as.data.frame(rbind(dt22, dt23, dt1115, dt24, dt16))
dt$Disturbed <- str_to_title(dt$Disturbed)


#===============================================================================
# Check Raw Litter: ------------------------------------------------------------
#===============================================================================
sum(is.na(dt$Raw.Litter))         # 1, OK, unreported once in 2022
sum(dt$Raw.Litter=="NA", na.rm=T) # 2 in 2016, OK
dt$Raw.Litter[dt$Raw.Litter=="NA"] <- NA
dt$Raw.Litter[dt$Raw.Litter=="20/290?"] <- 290 # looks more like 290 in pic of data sheet

dt$Raw.Litter <- as.numeric(dt$Raw.Litter)
range(dt$Raw.Litter, na.rm=T) # 0 - 1000
hist(dt$Raw.Litter)  # mostly between 0 & 100

dt$lograwLitter <- log(dt$Raw.Litter + 1) # transform to log for normaly distributed
hist(dt$lograwLitter) # OK


#===============================================================================
# Plots: -----------------------------------------------------------------------
#===============================================================================
sort(unique(dt$Disturbed))
dt$Disturbed[dt$Disturbed=="Undistubed"] <- "Undisturbed"

summary(lm(Raw.Litter ~ as.numeric(Session), data = dt))   # includes 23,46,50,54
summary(lm(lograwLitter ~ as.numeric(Session), data = dt)) # includes 23,46,50,54

summary(lm(Raw.Litter ~ as.numeric(Session), data = dt[dt$Session<20,]))   # sessions 1 to 19
summary(lm(lograwLitter ~ as.numeric(Session), data = dt[dt$Session<20,])) # sessions 1 to 19

(praw <- ggplot(dt, aes(x=Session, y=Raw.Litter, color = Disturbed)) + 
  geom_point(alpha=0.5) +
  labs(y = "Raw Litter", x="") +
  geom_smooth(formula = y ~ x, method = "lm", linetype="dashed")+
  geom_smooth(data=dt[dt$Session<20,], aes(x=Session, y=Raw.Litter, color = Disturbed),formula = y ~ x, method = "lm")+
  theme_classic()+
  theme(legend.position="none")+
  scale_color_manual(values=c("#CE780F", "#0A7D9D"))+
  facet_wrap(~Disturbed, scales = "free_y")+
  geom_vline(aes(xintercept = 19.5), linetype = "dashed", color = "black")) # warning OK, NA in 2022 above

(plograw <- ggplot(dt, aes(x=Session, y=lograwLitter, color = Disturbed)) + 
  geom_point(alpha=0.5) +
  labs(y = "log (Raw Litter)", x="Sampling session") +
  geom_smooth(formula = y ~ x, method = "lm", linetype="dashed")+
  geom_smooth(data=dt[dt$Session<20,], aes(x=Session, y=lograwLitter, color = Disturbed),formula = y ~ x, method = "lm")+
  theme_classic()+
  theme(legend.position="none")+
  scale_color_manual(values=c("#CE780F", "#0A7D9D"))+
  facet_wrap(~Disturbed, scales = "free_y")+
  geom_vline(aes(xintercept = 19.5), linetype = "dashed", color = "black")) # warning OK as above


rawlitter_time <- (praw/plograw) 

plot_path <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/Figures"
ggsave(filename=paste0(plot_path, "/RawLitter_Time.png"), width=6, height=6, dpi = 600, device='png', rawlitter_time)


# End of script #################################################################