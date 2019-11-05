#### Script to calculate species richness  ####
# test w/ 302 Brazilian endemic species evaluated in 2019
## species data
# http://geonode.jbrj.gov.br/layers/geonode%3Areavaliacoes2019_cncflora
## Brazil shapefile
# https://data.humdata.org/dataset/f5f0648e-f085-4c85-8242-26bf6c942f40/resource/87e9a35e-4894-4950-972f-b565372df3e2/download/bra_adm0.zip

# laoding packages
library("rgdal")
library("dplyr")
library("viridis")
library("ggplot2")
library("raster")

## function from Matt Strimas-Mackey
# http://strimas.com/spatial/hexagonal-grids/
source("R/make_grid.R")

# loading files
re <- readOGR("data/shapefile/reavaliacoes2019/reavaliacoes2019_cncflora.shp",
              use_iconv = TRUE,
              encoding = "UTF-8")
br <- readOGR("data/shapefile/BRA/BRA_adm0.shp")

head(re)

# reprojecting to albers equal area for south america
albers <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=km +no_defs"

re_alb <- spTransform(re, CRS(albers))
br_alb <- spTransform(br, CRS(albers))

plot(re_alb)
plot(br_alb, add = TRUE)

head(re_alb)

#### Richness per grid ####
# without clipping
hex_br <- make_grid(br_alb, cell_area = 10000, clip = FALSE)

hex_df <- data.frame(ID = row.names(hex_br))

hex_spdf <- SpatialPolygonsDataFrame(hex_br, hex_df)

plot(hex_br)

class(hex_br)

# writeOGR(hex_spdf,
#          "results/hex_br",
#          "hex_brasil",
#          driver = "ESRI Shapefile")

hex_re <- over(hex_br, re_alb, returnList = TRUE)

head(hex_re)

sapply(hex_re, nrow)

length(unique(hex_re[[27]]$specie))

rich <- data.frame(id = row.names(hex_br),
                   N = sapply(hex_re, function(x) length(unique(x$specie))))


hex_ft <- fortify(hex_spdf)
hex_rich <- merge(hex_ft, rich, by = "id")

head(hex_rich)

write.table(hex_rich, "results/hex_richness.csv",
            col.names = TRUE,
            row.names = FALSE,
            sep = ",")

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

map <- ggplot(hex_rich) +
  aes(long, lat, group = group, fill = N) +
  theme_void() +
  scale_fill_gradientn(colours = c("grey", pal)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  labs(title = "Richness of endangered species")

map





########################### do not run ############
# testing for 1 species

#library("redlistr")

species <- unique(re_alb$specie) %>%
  sort() %>%
  as.character()

re_alb$specie <- as.character(re_alb$specie)

spp <- list()
for (i in 1:length(species)) {
  spp[[i]] <- re_alb[re_alb$specie == species[i],]
}

spp[[1]]



# we are excluding species with less than 3 records
N <- sapply(spp, nrow)
count <- which(N > 2)

# calculating polygon for each species
EOO.polygon <- lapply(spp[count], makeEOO)
# calculating area
EOO.area <- sapply(EOO.polygon, getAreaEOO)

# creating grid
AOO.grid <- lapply(spp[count], makeAOOGrid, grid.size = 100)


AOO.grid[[1]]
plot(AOO.grid)
plot(sp001, add = T, col = "green", legend = FALSE)

