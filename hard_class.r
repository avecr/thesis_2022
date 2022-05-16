# R code for testing RSTollbox distances and spectral angle mapper

install.packages("raster")
install.packages("rgdal")

library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)

setwd("/Users/anareis/OneDrive/MSc_Unibo/Thesis/data/")

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

# plot RGB images 
pRGB <- raster("T21KVU_20211207T140049_TCI_10m.jp2") # TCI being the RGB image
pRGB
plot(pRGB) # pantanal's image

cRGB <- raster("T22KBC_20211226T134211_TCI_10m.jp2") # TCI being the RGB image
cRGB
plot(cRGB) # cerrado's image

# plot false colour images
# combine the bands 8, 4 and 3 of s2 10 m
# pantanal's images
pfc_stack <-  stack(raster("T21KVU_20211207T140049_B08_10m.jp2"), raster("T21KVU_20211207T140049_B04_10m.jp2"), raster("T21KVU_20211207T140049_B03_10m.jp2"))
plotRGB(pfc_stack, r=3, g=2, b=1, stretch="Lin")

# plot both images on a multiframe graph
par(mfrow=c(4, 4))
plot(pRGB)
plot(cRGB)
ggR(ps2_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme
ggR(cs2_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme
ggR(ps2_ucd$map, layer = 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance burnt area", direction = -1) + MapTheme
ggR(cs2_ucd$map, layer = 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance burnt area", direction = -1) + MapTheme
ggR(ps2_sa, 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle burnt area", direction = -1) + MapTheme
ggR(cs2_sa, 1:4, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle burnt area", direction = -1) + MapTheme

plot(l2011$B1_sre, col=clb)
plot(l2011$B2_sre, col=clg)
