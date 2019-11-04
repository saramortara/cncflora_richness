#### Script to calculate species richness  ####
# test w/ 302 endemic species from 2019
# http://geonode.jbrj.gov.br/layers/geonode%3Areavaliacoes2019_cncflora

# laoding packages
library("rgdal")
library("redlistr")
library("dplyr")
library("viridis")
library("ggplot2")

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
dim(hex_br)

hex_re <- over(hex_br, re_alb, returnList = TRUE)

head(hex_re)

sapply(hex_re, nrow)

length(unique(hex_re[[27]]$specie))

rich <- data.frame(id = as.factor(row.names(hex_br)),
                   N = sapply(hex_re, function(x) length(unique(x$specie))))

hex_rich <- SpatialPolygonsDataFrame(hex_br, rich)

head(hex_rich, 27)

head(hex_rich)
names(hex_rich)
summary(hex_rich)
dim(hex_rich)

rich_df <- broom::tidy(hex_rich)

dim(rich_df)

summary(rich_df)

rich_df$piece

map <- ggplot() +
  geom_polygon(data = hex_br,
               aes(x = long, y = lat, group = group, fill = rich_df$piece ), colour = "black") +
  theme_void()

# point_density <- over(hex_br, re_alb, returnList = TRUE) %>%
#   plyr::ldply(.fun = function(x) x, .id = "id") %>%
#   mutate(id = as.character(id)) %>%
#   count(id, specie) %>%
#   left_join(fill_missing, ., by = c("id", "specie")) %>%
#   bind_rows()



head(fill_missing)
dim(fill_missing)


head(point_density)
summary(point_density)





map

spplot(hex_rich)

plot(hex_rich)


map <- ggplot(hex_rich) +
  geom_sf(aes(fill = id)) +
  theme_void()

map

summary(hex_rich)

head(hex_rich)
dim(hex_rich)


########################### do not run ############
# testing for 1 species
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

