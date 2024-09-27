#===============================================================================
# STUDY AREA FIGURES (NR & CM)
# November 2023 & May 2024
#===============================================================================



#===============================================================================
# Main source: -----------------------------------------------------------------
# This code is available online & written by Milos Popovic May 14, 2022
# https://milospopovic.net/crisp-topography-map-with-r/
# ==============================================================================



# NOTE I: code was run in the St Andrews BioTIME server & figures were downloaded.
# NOTE II: code contains outdated figure ids (figure 2.1 is now in chapter 5,
# and figure 2.2 is figure 2.1 in chapter 2)


#===============================================================================
# Libraries: -------------------------------------------------------------------
#===============================================================================
library(tidyverse)
library(sf)
library(giscoR)
library(terra)
library(marmap)
library(elevatr)
library(ggspatial)


rm(list = ls())
plotpath <- "C:/Users/afe1/OneDrive - University of St Andrews/PHD/0_GLOBAL_THESIS_CHAPTER_GITHUB_REPOSITORIES/Thesis_GeneralMethods/Figures"



# ==============================================================================
# Country data: ----------------------------------------------------------------
# ==============================================================================
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

country_sfMexico <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "01",
  country = "Mexico")
country_sfTT <- giscoR::gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "01",
  country = "Trinidad & Tobago")

get_sf <- function(country_sf=NULL) {
  country_transformed <- st_transform(country_sf, crs = crsLONGLAT)
  return(country_transformed)
}

country_transformedMexico <- get_sf(country_sfMexico)
country_transformedTT <- get_sf(country_sfTT)



# ==============================================================================
# Elevation data: --------------------------------------------------------------
# ==============================================================================
get_elevation_data <- function(country_transformed=NULL) {
  
  country_elevation <- get_elev_raster(
    locations = country_transformed, 
    z = 9, 
    clip = "locations") 
  
  country_elevation_df <- as.data.frame(country_elevation, xy = T) %>%
    na.omit()
  
  colnames(country_elevation_df)[3] <- "elevation"
  
  return(country_elevation_df)
}

#country_elevation_df21 <- get_elevation_data(country_transformedMexico) # study area CM
#country_elevation_df22 <- get_elevation_data(country_transformedTT)     # study area NR

#save(country_elevation_df21, file="country_elevation_df21.RData")
#save(country_elevation_df22, file="country_elevation_df22.RData")



#===============================================================================
# Maps: ------------------------------------------------------------------------
#===============================================================================

#load("country_elevation_df21.RData")
#load("country_elevation_df22.RData") # save & re-load 

## Topographic map: ------------------------------------------------------------
get_elevation_map <- function(country_elevation_df=NULL, ti=NULL) {
  
  country_map <- ggplot() +
    geom_tile(data = country_elevation_df, 
              aes(x = x, y = y, fill = elevation)) +
    scale_fill_etopo() +
    coord_sf(crs = crsLONGLAT)+
    theme_minimal() +
    theme(text = element_text(family = "georg", color = "#22211d"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_line(color = "white", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
          plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
          #plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank()) +
    labs(x = "", 
         y = NULL, 
         title = ti, 
         subtitle = "")
  
  return(country_map)
}

country_map21 <- get_elevation_map(country_elevation_df21, ti="Topographic map of Mexico") # with title
country_map21nt <- get_elevation_map(country_elevation_df21, ti="")                        # nt

country_map22 <- get_elevation_map(country_elevation_df22, ti="Topographic map of Trinidad & Tobago")
country_map22nt <- get_elevation_map(country_elevation_df22, ti="")


ggsave(filename="Figure2.1_empty.png", width=7, height=8.5, dpi = 600, device='png', country_map21)
ggsave(filename="Figure2.1_emptynt.png", width=7, height=8.5, dpi = 600, device='png', country_map21nt)

ggsave(filename="Figure2.2_empty.png", width=7, height=8.5, dpi = 600, device='png', country_map22)
ggsave(filename="Figure2.2_emptynt.png", width=7, height=8.5, dpi = 600, device='png', country_map22nt)



## Cropped for zoom view: ------------------------------------------------------
country_map21Croppednt <- country_map21nt +
  coord_sf(xlim = c(-107, -97), ylim = c(16.5,28))
ggsave(filename="Figure2.1_emptyCropped.png", width=7, height=8.5, dpi = 600, device='png', country_map21Croppednt)  

country_map22Croppednt <- country_map22nt +
  coord_sf(xlim = c(-61.80, -60.85), ylim = c(10.55,10.88))
ggsave(filename="Figure2.2_emptyCropped.png", width=7, height=8.5, dpi = 600, device='png', country_map22Croppednt)  



#===============================================================================
# Convex hulls: ----------------------------------------------------------------
#===============================================================================
load("NDT67.RData") # coordinates of selected sites in CM
dtTT <- read.csv("coords_TT.csv") # contains coordinates of sites in NR
dtMexico <- NDT67

dt_coordMexico <- dtMexico %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

dt_coordTT <- dtTT %>% select(Longitude, Latitude) %>% distinct() %>%
  st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% st_union()

centroidMexico <- dt_coordMexico %>% st_convex_hull() %>% st_centroid() %>% unlist 
centroidMexico

centroidTT <- dt_coordTT %>% st_convex_hull() %>% st_centroid() %>% unlist 
centroidTT

areaMexico <- st_transform(dt_coordMexico, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
areaMexico

areaTT <- st_transform(dt_coordTT, st_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>%
  st_convex_hull() %>% st_area()  ##get area in sq km
areaTT


country_map21CH <- country_map21Croppednt +
  geom_point(data = dtMexico, aes(x = Longitude, y = Latitude), size = 2, shape = 1, colour = 'navy') +
  geom_point(aes(x = centroidMexico[1], y = centroidMexico[2]), size = 3, colour = 'navy') +
  geom_sf(data = dt_coordMexico %>% st_convex_hull(), color= 'blue', fill = 'blue', alpha=0.3)

country_map22CH <- country_map22Croppednt +
  geom_point(data = dtTT, aes(x = Longitude, y = Latitude), size = 2, shape = 1, colour = 'navy') +
  geom_point(aes(x = centroidTT[1], y = centroidTT[2]), size = 3, colour = 'navy') +
  geom_sf(data = dt_coordTT %>% st_convex_hull(), color= 'blue', fill = 'blue', alpha=0.3)

ggsave(filename="Figure2.1_CH.png", width=7, height=8.5, dpi = 600, device='png', country_map21CH)
ggsave(filename="Figure2.2_CH.png", width=7, height=8.5, dpi = 600, device='png', country_map22CH)


## prev won't save the cropped version:
country_map21CroppedCH <- ggplot() +
  geom_tile(data = country_elevation_df21, 
            aes(x = x, y = y, fill = elevation)) +
  scale_fill_etopo() +
  #coord_sf(crs = crsLONGLAT)+
  coord_sf(xlim = c(-107, -97), ylim = c(16.5,28))+
  theme_minimal() +
  theme(text = element_text(family = "georg", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
        plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
        #plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()) +
  labs(x = "", 
       y = NULL, 
       title = "", 
       subtitle = "")+
  annotation_north_arrow(which_north = "true", location = "tr")+
  annotation_scale() +
  geom_point(data = dtMexico, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'navy') +
  geom_point(aes(x = centroidMexico[1], y = centroidMexico[2]), size = 3, colour = 'navy') +
  geom_sf(data = dt_coordMexico %>% st_convex_hull(), color= 'blue', fill = 'blue', alpha=0.3)

country_map21CroppedCH <- country_map21CroppedCH + 
  coord_sf(xlim = c(-107, -97), ylim = c(16.5,28)) # cropped CM



country_map22CroppedCH <- ggplot() +
  geom_tile(data = country_elevation_df22, 
            aes(x = x, y = y, fill = elevation)) +
  scale_fill_etopo() +
  #coord_sf(crs = crsLONGLAT)+
  coord_sf(xlim = c(-61.80, -60.85), ylim = c(10.55,10.88))+
  theme_minimal() +
  theme(text = element_text(family = "georg", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
        plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
        #plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()) +
  labs(x = "", 
       y = NULL, 
       title = "", 
       subtitle = "")+
  annotation_north_arrow(which_north = "true", location = "tr")+
  annotation_scale() +
  geom_point(data = dtTT, aes(x = Longitude, y = Latitude), size = 2, shape = 21, colour = 'navy') +
  geom_point(aes(x = centroidTT[1], y = centroidTT[2]), size = 3, colour = 'navy') +
  geom_sf(data = dt_coordTT %>% st_convex_hull(), color= 'blue', fill = 'blue', alpha=0.3)

country_map22CroppedCH <- country_map22CroppedCH + 
  coord_sf(xlim = c(-61.80, -60.85), ylim = c(10.55,10.88)) # cropped NR



ggsave(filename="Figure2.1_CHCropped.png", width=7, height=8.5, dpi = 600, device='png', country_map21CroppedCH)
ggsave(filename="Figure2.2_CHCropped.png", width=7, height=8.5, dpi = 600, device='png', country_map22CroppedCH)


# End of script ################################################################
