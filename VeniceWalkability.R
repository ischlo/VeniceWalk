#####INSTALL PACKAGES IF NOT IN THE LIST ####

install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), repos = "https://www.stats.bris.ac.uk/R/")

#sf: simple features, standard way to encode spatial vector data
#tmap: layer-based and easy approach to make thematic maps
#tmaptools: set of tools for reading and processing spatial data
#RSQLite: embeds the SQLite database engine in R

install.packages("rgdal") # FOR MAC
install.packages("tidygraph")
install.packages("raster") # TO MANIPULATE RASTERS
install.packages("shinyjs") #FOR COOL COLORED MAPS
install.packages("leaflet.extras")
install.packages("igraph")
install.packages("stplanr")

#### LOAD LIBRARIEs ####

library(rgdal)
library(sf) #TO READ SHAPEFILES 
library(sp) 
library(tidyverse) #TO MANIPULATE CSV FILES 
library(tidygraph)
library(tmap) #TO PLOT MAPS
library(tmaptools) 
library(readr)
library(RSQLite)  #TO CONNECT CSV DATA TO GEOPACKAGE
library(raster)
library(tibble)
library(leaflet)
library(leaflet.extras)
library(maptools)
library(rgeos)
library(osmdata)
library(ggmap)
library(ggplot2)
library(osmar)
library(igraph)
library(stplanr)
library(gdalUtils)
library(plotly)
library(spatstat)
library(tibble)

getwd() #GET WORKING DIRECTORY
#setwd("/Users/ivann/Desktop/") #TO SET WORKING DIRECTORY



#### highway key ####
# points
summarize(phigh$osm_points)
qtm(phigh$osm_points)

# lines
summarise(phigh$osm_lines)
qtm(phigh$osm_lines)

# keep the edges that are walkable
VeniceRoads_Lines <- phigh$osm_lines[-which(phigh$osm_lines$highway %in% c("service","trunk")),]

# polygons
summarise(phigh$osm_polygons)
qtm(phigh$osm_polygons)

#multilines
summarize(phigh$osm_multilines)
qtm(phigh$osm_multilines)

#multipolygons
summarise(phigh$osm_multipolygons)
qtm(phigh$osm_multipolygons)

# investigation of the lines

unique(VeniceRoads_Lines$highway)

qtm(VeniceRoads_Lines)

#### PROJECTION OF THE MAP  ####
crs <-  "+proj=longlat +datum=WGS84 +no_defs"
#### TRANSFORMING THE ROADS NETWORK FROM LINESTRING OBJECTS TO A SPATIAL LINES NETWORK ####

# INSPIRED BY CHAPTER 12.8 https://geocompr.robinlovelace.net/transport.html#route-networks 
VeniceRoads <- as(VeniceRoads_Lines,"Spatial") %>% SpatialLinesNetwork() 

# CHECKS 
slotNames(VeniceRoads)
VeniceRoads@weightfield
class(VeniceRoads@nb)
VeniceRoads@nb

##
######### Analysis #############

#Edge betweenness 
e = edge_betweenness(VeniceRoads@g) %>% as.data.frame(.)
summary(e)
colnames(e)[1] <- "e"
e$id <- c(1:length(e$e))
colnames(e)

# closeness 

C <- closeness(VeniceRoads@g,mode = "all", normalized = T) %>% as.data.frame(.) 
# inversing the values for easier manipulation
C <- round(1/C)
summary(C)
C$id <- c(1:length(C$.))
colnames(C)[1] <- "c"

hist(C$c)

#### Merging with the Spatial lines ####

VeniceE <- merge(VeniceRoads@sl,e, by.x = "id", by.y = "id")

VeniceC <- merge(VeniceRoads@sl,C,by.x = "id", by.y = "id")

#### maps #### 

tmap_mode("view")

hist(VeniceC$c)

mapC <- tm_shape(VeniceC, bbox = bbVenice) +
  tm_lines(col = "c",
           style = "fixed"
           ,palette = "-plasma"
           ,lwd=2
           ,n=4
           #,contrast = c(.7,1)
           ,breaks =  c(8700,8800,9200,9250,9300)
           ,alpha = 1
           ,legend.col.show = T,
           title.col = "Closeness Score :",
           labels = c("high","","", "low")
  ) + tm_layout(main.title = "Closeness",
                legend.outside = T,
                legend.text.size = 1) + tm_view(set.zoom.limits = c(13, 16),
                                                set.bounds = bbVenice)

mapC

hist(VeniceE$e)

mapE <- tm_shape(VeniceE, bbox = bbVenice) +
  tm_lines(col = "e",
           style = "fixed"
           ,palette = "plasma"
           ,lwd=2
           ,n=6
           ,contrast = c(0, .7)
           ,breaks =  c(0,1,250,500,20000,40000,80100)
           ,alpha = 1
           ,legend.col.show = T,
           title.col = "Edge Betweenness Score :",
           labels = c("low","","","","", "high")
  ) + tm_layout(scale = 1.5,
                legend.outside = T,
                legend.text.size = 1,
                )
mapE

map <- mapE + mapC

map
#
#### load data ####

bbVenice <- c(12.2996,45.4219,12.3680,45.4503)

# KEY = HIGHWAYS FOR ALL THE ROADS 
phigh <- opq(bbox = bbVenice,timeout = 600) %>% 
  add_osm_feature(key = "highway"
                  #,value = "appartments"
                  ,key_exact = TRUE
                  #,value_exact = FALSE
                  #,match_case = FALSE
  ) %>%
  osmdata_sf() %>% osm_poly2line()
view(phigh)