# R code for testing fuzzy classification of Sentinel-2 imageries using RSTollbox

install.packages("raster")
install.packages("rgdal")
install.packages("scales")

library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)

setwd("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata")

#---BURNT AREA IN PANTANAL

# change resolution pantanal B08 10m to 20m 
# p_band8_10 <- raster("T21KWB_20201003T140059_B08_10m.jp2")
# p_band8_20 <- aggregate(p_band8_10, fact=2)
# writeRaster(p_band8_20, filename=file.path("T21KWB_20201003T140059_B08_20m.tif"), format="GTiff", overwrite=TRUE)

# import shapefile of pantanal area and crop images
shp_pantanal <-shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/shp_pantanal")
p1 <- crop(raster("T21KWB_20201003T140059_B02_20m.jp2"),shp_pantanal)
p2 <- crop(raster("T21KWB_20201003T140059_B03_20m.jp2"),shp_pantanal)
p3 <- crop(raster("T21KWB_20201003T140059_B04_20m.jp2"),shp_pantanal)
p4 <- crop(raster("T21KWB_20201003T140059_B05_20m.jp2"),shp_pantanal)
p5 <- crop(raster("T21KWB_20201003T140059_B06_20m.jp2"),shp_pantanal)
p6 <- crop(raster("T21KWB_20201003T140059_B07_20m.jp2"),shp_pantanal)
p7 <- crop(raster("T21KWB_20201003T140059_B8A_20m.jp2"),shp_pantanal)
p8 <- crop(raster("T21KWB_20201003T140059_B11_20m.jp2"),shp_pantanal)
p9 <- crop(raster("T21KWB_20201003T140059_B12_20m.jp2"),shp_pantanal)
p10 <- crop(raster("T21KWB_20201003T140059_B08_20m.tif"),shp_pantanal)

# put all the images together with the stack() and save as .tif
burnt_p <- stack(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
burnt_p
# writeRaster(burnt_p, filename=file.path("burnt_p.tif"), format="GTiff", overwrite=TRUE)

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# unsupervised classification
# burnt area in Pantanal
set.seed(42)
burnt_uc <- unsuperClass(burnt_p, nClasses = 3, output = "classes")
burnt_uc

ggR(burnt_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

# distances
set.seed(42)
burnt_ucd <- unsuperClass(burnt_p, nClasses = 3, output = "distances")
burnt_ucd

ggR(burnt_ucd$map, layer = 1:3, stretch="lin", geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance", direction = -1, option = "inferno") + MapTheme

# spectral angle mapper - sam ("A “more supervised” alternative")
p_classCentroids <- burnt_ucd$model$centers
burnt_sam <- sam(burnt_p, em = p_classCentroids, angles = TRUE)
burnt_sam

ggR(burnt_sam, 1:3, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle", direction = -1, option = "inferno") + MapTheme

# Spectral unmixing
burnt_umx <- mesma(burnt_p, em = p_classCentroids)
burnt_umx

ggR(burnt_umx, 1:3, geom_raster = TRUE) + scale_fill_viridis_c(name = "Probability", option = "inferno") + MapTheme

#---DEFORESTATION IN CERRADO

# change resolution pantanal B08 10m to 20m 
c_band8_10 <- raster("T21KWB_20201003T140059_B08_10m.jp2")
c_band8_20 <- aggregate(p_band8_10, fact=2)
writeRaster(p_band8_20, filename=file.path("T21KWB_20201003T140059_B08_20m.tif"), format="GTiff", overwrite=TRUE)

# import shapefile of pantanal area and crop images
shp_pantanal <-shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/shp_pantanal")
p1 <- crop(raster("T21KWB_20201003T140059_B02_20m.jp2"),shp_pantanal)
p2 <- crop(raster("T21KWB_20201003T140059_B03_20m.jp2"),shp_pantanal)
p3 <- crop(raster("T21KWB_20201003T140059_B04_20m.jp2"),shp_pantanal)
p4 <- crop(raster("T21KWB_20201003T140059_B05_20m.jp2"),shp_pantanal)
p5 <- crop(raster("T21KWB_20201003T140059_B06_20m.jp2"),shp_pantanal)
p6 <- crop(raster("T21KWB_20201003T140059_B07_20m.jp2"),shp_pantanal)
p7 <- crop(raster("T21KWB_20201003T140059_B8A_20m.jp2"),shp_pantanal)
p8 <- crop(raster("T21KWB_20201003T140059_B11_20m.jp2"),shp_pantanal)
p9 <- crop(raster("T21KWB_20201003T140059_B12_20m.jp2"),shp_pantanal)
p10 <- crop(raster("T21KWB_20201003T140059_B08_20m.tif"),shp_pantanal)

# put all the images together with the stack() and save as .tif
burnt_p <- stack(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
burnt_p
# writeRaster(burnt_p, filename=file.path("burnt_p.tif"), format="GTiff", overwrite=TRUE)

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# unsupervised classification
# burnt area in Pantanal
set.seed(42)
burnt_uc <- unsuperClass(burnt_p, nClasses = 3, output = "classes")
burnt_uc

ggR(burnt_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

# distances
set.seed(42)
burnt_ucd <- unsuperClass(burnt_p, nClasses = 3, output = "distances")
burnt_ucd

ggR(burnt_ucd$map, layer = 1:3, stretch="lin", geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance", direction = -1, option = "inferno") + MapTheme

# spectral angle mapper - sam ("A “more supervised” alternative")
p_classCentroids <- burnt_ucd$model$centers
burnt_sam <- sam(burnt_p, em = p_classCentroids, angles = TRUE)
burnt_sam

ggR(burnt_sam, 1:3, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle", direction = -1, option = "inferno") + MapTheme

# Spectral unmixing
burnt_umx <- mesma(burnt_p, em = p_classCentroids)
burnt_umx

ggR(burnt_umx, 1:3, geom_raster = TRUE) + scale_fill_viridis_c(name = "Probability", option = "inferno") + MapTheme

#--------- OLD CODE --------- import Sentinel 2 data
# creat lists with the images
list1 <- list.files(pattern = "T21KVU_20211207T140049_B") # Pantanal's area (p)
list1 # 9 images

list2 <- list.files(pattern = "T22KBC_20211226T134211_B") # Cerrado's area (c)
list2 # 9 images

# import the single layers with raster()
import1 <- lapply(list1,raster)
import1

import2 <- lapply(list2,raster)
import2

# put all the images together with the stack()
pstack <- stack(import1)
pstack

cstack <- stack(import2)
cstack

# burnt area in Cerrado (c)
set.seed(42)
cs2_uc <- unsuperClass(cstack, nClasses = 4, output = "classes")
cs2_uc

ggR(cs2_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

# land use area in Cerrado 
cs2_ucd <- unsuperClass(cstack, nClasses = 4, output = "distances")
cs2_ucd

ggR(cs2_ucd$map, layer = 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance land use area", direction = -1) + MapTheme
dev.off()

# land use area in Cerrado
c_classCentroids <- cs2_ucd$model$centers

cs2_sa <- sam(pstack, em = c_classCentroids, angles = TRUE)
cs2_sa

ggR(cs2_sa, 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle land use", direction = -1) + MapTheme
dev.off()

# plot true/false colour images
plotRGB(pstack, r=3, g=2, b=1, scale=maxValue(pstack[[1]]), stretch="lin") #true image pantanal

plotRGB(cstack, r=3, g=2, b=1, scale=maxValue(cstack[[1]]), stretch="lin") #true image cerrado

plotRGB(pstack, r=9, g=2, b=1, scale=maxValue(pstack[[1]]), stretch="lin") #false image pantanal

plotRGB(cstack, r=9, g=3, b=2, scale=maxValue(cstack[[1]]), stretch="lin") #false image cerrado
