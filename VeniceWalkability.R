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

# We'll use remotes to install packages, install it if needs be:
if(!"remotes" %in% installed.packages()) {
  install.packages("remotes")
}

cran_pkgs = c(
  "dplyr",
  "tibble",
  "ggplot2",
  "units",
  "rgrass7",
  "link2GI",
  "nabor"
)

remotes::install_cran(cran_pkgs)


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
library(rgdal)
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
library(ggplot2)
library(units)
library(rgrass7)
library(link2GI)
library(nabor)

getwd() #GET WORKING DIRECTORY
#setwd("/Users/ivann/Desktop/") #TO SET WORKING DIRECTORY

link = findGRASS()

#### highway ####
# points
summarize(phigh$osm_points)
qtm(phigh$osm_points)

# lines
summarise(phigh$osm_lines)
qtm(phigh$osm_lines)

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

#### buildings ####
i <- 1
for (i in c(1:153)){
unique(pbuild$osm_polygons[,i])  
}

qtm(pbuild$osm_polygons)


#### PROJECTION OF THE MAP  ####
crs <-  "+proj=longlat +datum=WGS84 +no_defs"
#### Other method with edges and nodes ####

# taken from: https://www.r-spatial.org/r/2019/09/26/spatial-networks.html


VeniceRoads_Lines <- VeniceRoads_Lines[,c("name","highway","geometry")]

edges <- VeniceRoads_Lines %>%
  mutate(edgeID = c(1:n()))

edges

nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

nodes

nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

nodes

source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

edges

nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

nodes

graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph

graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

graph

tmap_mode('plot')

tm_shape(graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), bbox = bbVenice) +
  tm_lines(col = "betweenness",lwd = 2, palette = "plasma",contrast = c(0,.7)) +
  tm_shape(graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()) +
  tm_dots(col = "betweenness") +
  tmap_options(basemaps = 'OpenStreetMap')

graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

graph

graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

graph

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

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
#WITH THE NODS OF THE NETWORK TO CALCULATE THE CONNECTIVITY OF A ROAD
#e <- NULL
e = edge_betweenness(VeniceRoads@g) %>% as.data.frame(.)
summary(e)
colnames(e)[1] <- "e"
e$id <- c(1:length(e$e))
colnames(e)

b = betweenness(VeniceRoads@g,normalized = F,directed = F) %>% as.data.frame()
summary(b)
b$id <- c(1:length(b$.))
colnames(b)[1] <- "b"

C <- closeness(VeniceRoads@g, normalized = T) %>% as.data.frame(.)
summary(C)
C$id <- c(1:length(C$.))
colnames(C)[1] <- "c"
C

# component <- components(VeniceRoads@g)
# 
# veniceClusters <- data.frame(c(1:length(component$membership)),component$membership)
# colnames(veniceClusters) <- c("id","membership")

# 
# CEB <- cluster_edge_betweenness(VeniceRoads@g,weights = VeniceRoads@sl$length , directed = F)

####  ADAPTING CONNECTIVITY DATA  ####

# view(VeniceRoads@sl)
# view(e)
# 
# hist(C$c)
# range(C$c)
# max()

VeniceWalk <- merge(VeniceRoads@sl,e, by.x = "id", by.y = "id")

VeniceWalk$b <- b

VeniceWalk$c <- c
 
# VeniceB <- merge(VeniceRoads@sl,b, by.x = "id", by.y = "id")
# 
# VeniceC <- merge(VeniceRoads@sl,C,by.x = "id", by.y = "id")

#VeniceClusters <- merge(VeniceRoads@sl,veniceClusters ,by.x = "id", by.y = "id")

#### maps #### 

tmap_mode("view")

hist(VeniceClusters$membership)

# betweenness

tm_shape(VeniceB, bbox = bbVenice) +
  tm_lines(col = "b"
           ,style = "fixed"
           ,palette = "plasma"
           ,lwd=2
           ,n=6
           ,contrast = c(0, .7)
           ,breaks =  c(0,1,250,500,20000,40000,80100) ## log or lin scale  log : c(0,1,7,11,13) ; lin : c(0,100,1000,10000,350000,60000)
           ,alpha = 1
           ,legend.col.show = T,
           title.col = "Score :",
           labels = c("low","","","","", "high")
  ) + tm_layout(main.title = "Betweenness",
                legend.outside = T,
                legend.text.size = 1)
# closeness

hist(VeniceC$c)

tm_shape(VeniceC, bbox = bbVenice) +
  tm_lines(col = "c",
           style = "fixed"
           ,palette = "inferno"
           ,lwd=2
           ,n=6
           ,contrast = c(0, .7)
           ,breaks =  c(0,1,50,500,20000,40000,80100) ## log or lin scale  log : c(0,1,7,11,13) ; lin : c(0,100,1000,10000,350000,60000)
           ,alpha = 1
           ,legend.col.show = T,
           title.col = "Score :",
           labels = c("low","","","","", "high")
  ) + tm_layout(main.title = "Closeness",
                legend.outside = T,
                legend.text.size = 1)

hist(VeniceE$e)

tm_shape(VeniceE, bbox = bbVenice) +
  tm_lines(col = "e",
           style = "fixed"
           ,palette = "plasma"
           ,lwd=3
           ,n=6
           ,contrast = c(0, .7)
           ,breaks =  c(0,1,250,500,20000,40000,80100) ## log or lin scale  log : c(0,1,7,11,13) ; lin : c(0,100,1000,10000,350000,60000)
           ,alpha = 1
           ,legend.col.show = T,
           title.col = "Score :",
           labels = c("low","","","","", "high")
  ) + tm_layout(title = "Pedestrian density",
                scale = 1.5,
                legend.outside = T,
                legend.text.size = 1,
                ) +
  tmap_options(basemaps = "Esri.WorldGrayCanvas")

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


# Not ised 
pbuild <- opq(bbox = bbVenice,timeout = 600) %>% #timeout in seconds
  add_osm_feature(key = "building"
                  #,value = "appartments"
                  ,key_exact = TRUE
                  #,value_exact = FALSE
                  #,match_case = FALSE
  ) %>%
  osmdata_sf()
