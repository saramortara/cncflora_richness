### Script to calculate species richness
# test w/ endemic species of Rio de Janeiro
# RJ data from: http://geonode.jbrj.gov.br/layers/geonode%3Aspp_poligono_endemicas_do_rio_sirgas
# BR endangereds data from: http://geonode.jbrj.gov.br/layers/geonode%3Apoligonos_ameacadas_atualizado_22042015_portaria_443_2014

# laoding packages
library("rgdal")
library("redlistr")
library("dplyr")

# loading functions
source("R/make_grid.R")

# loading files
rj <- readOGR("data/shapefile/endemicas_rj/spp_poligono_endemicas_do_rio_sirgas.shp")
ame <- readOGR("data/shapefile/poligonos_ameacadas/poligonos_ameacadas_atualizado_22042015_portaria_443_2014.shp")
br <- readOGR("data/shapefile/BRA/BRA_adm0.shp")

plot(rj)
plot(br)

#### 1. creating hexagonal grid ####
# following http://strimas.com/spatial/hexagonal-grids/

size <- 1
hex_points <- spsample(br, type = "hexagonal", cellsize = size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)

plot(br, col = "grey50", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)

# albers equal area for south america
albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=km +no_defs"

study_area_alb <- CRS(albers) %>%
  spTransform(br, .)
# without clipping
hex_grid <- make_grid(study_area_alb, cell_area = 2500, clip = FALSE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "orange", add = TRUE)
box()
# with clipping
hex_grid <- make_grid(study_area_utm, cell_area = 625, clip = TRUE)
plot(study_area_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "orange", add = TRUE)
box()
# reproject to a projected coordinate system to albers SA
isLonLat(rj)
head(rj)
crs(rj)


rj.proj <- spTransform(rj, CRS(albers))

plot(rj.proj)

isLonLat(rj.proj)

getArea(rj.proj)

getAOO(rj.proj, 1000)

rj.sp <- as(rj.proj, "SpatialPolygons")

AOO.grid <- makeAOOGrid(rj.sp, grid.size = 1000,
                        min.percent.rule = F)
min.percent.rule = F)
