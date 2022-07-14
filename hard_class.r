# R code for testing fuzy classification of Sentinel-2 imageries using RSTollbox

install.packages("raster")
install.packages("rgdal")
install.packages("scales")

library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)

setwd("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata")

# change resolution pantanal B08 10m to 20m 
p_band8_10 <- raster("T21KWB_20201003T140059_B08_10m.jp2")
p_band8_20 <- aggregate(p_band8_10, fact=2)
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
writeRaster(burnt_p, filename=file.path("burnt_p.tif"), format="GTiff", overwrite=TRUE)

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# testing RStoolbox: unsupervised classification default
# burnt area in Pantanal (p)
set.seed(42)
ps2_uc <- unsuperClass(pstack, nClasses = 4, output = "classes")
ps2_uc

ggR(ps2_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

# burnt area in Cerrado (c)
set.seed(42)
cs2_uc <- unsuperClass(cstack, nClasses = 4, output = "classes")
cs2_uc

ggR(cs2_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

# testing RStoolbox new option: DISTANCES ("Return distance to all class centroids. There is one layer per class containing the euclidean distance of each pixel to its centroid.")
# burnt area in Pantanal (p)
set.seed(42)
ps2_ucd <- unsuperClass(pstack, nClasses = 4, output = "distances")
ps2_ucd

ggR(ps2_ucd$map, layer = 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance burnt area", direction = -1) + MapTheme
dev.off()

# land use area in Cerrado 
cs2_ucd <- unsuperClass(cstack, nClasses = 4, output = "distances")
cs2_ucd

ggR(cs2_ucd$map, layer = 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance land use area", direction = -1) + MapTheme
dev.off()

# testing RStoolbox new option: SPECTRAL ANGLE MAPPER ("A “more supervised” alternative, if you already know the class spectra, e.g. of burnt areas, would be to use the spectral angle mapper. I’m taking the class centers from kmeans just as an example here.")
# burnt area in Pantanal (p)
p_classCentroids <- ps2_ucd$model$centers
ps2_sa <- sam(pstack, em = p_classCentroids, angles = TRUE)
ps2_sa

ggR(ps2_sa, 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle burnt area", direction = -1) + MapTheme
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

# plot both images on a multiframe graph
par(mfrow=c(2, 2))
plotRGB(pstack, r=3, g=2, b=1, scale=maxValue(pstack[[1]]), stretch="lin") 
plotRGB(pstack, r=9, g=2, b=1, scale=maxValue(pstack[[1]]), stretch="lin")
plotRGB(cstack, r=3, g=2, b=1, scale=maxValue(cstack[[1]]), stretch="lin") 
plotRGB(cstack, r=9, g=3, b=2, scale=maxValue(cstack[[1]]), stretch="lin")



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
