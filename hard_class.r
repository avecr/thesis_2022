# R code for testing RSTollbox distances and spectral angle mapper

install.packages("raster")
install.packages("rgdal")
install.packages("scales")

library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)
library(scales)

setwd("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata")

# re-scale B08 10m to 20m 
p_band8A <- raster("T22KBC_20211226T134211_B8A_20m.jp2")
p_band8_10 <- raster("T21KWB_20201003T140059_B08_10m.jp2")
p_band8_20 <- rescale(p_band8_10, p_band8A)
rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE))

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# import Sentinel 2 data
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
