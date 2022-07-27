# R code for testing fuzzy classification of Sentinel-2 imageries using RSTollbox

install.packages("raster")
install.packages("rgdal")
install.packages("scales")
install.packages("rasterVis")
install.packages("rpart")

library(raster)
library(RStoolbox)
library(rgdal)
library(ggplot2)
library(rasterVis)
library(rpart)

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

# Supervised classification
burnt_p <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/burnt_p.tif")
names(burnt_p) <- c('blue', 'green', 'red', 'red_5', 'red_6', 'red_7', 'n_NIR', 'SWIR1', 'SWIR2', 'NIR')

# The class names and colors for plotting
nlcdclass <- c("Burnt", "Mixed burnt", "Not burnt")
classdf <- data.frame(classvalue1 = c(1,2,3), classnames1 = nlcdclass)
# Hex codes of colors
classcolor <- c("#5475A8", "#B50000", "#D2CDC0")

# Import shp of pantanal with the polygons from each class (burnt, mixed burnt and not burnt)
samp <- shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/s_shp_pantanal")
samp
# generate 300 point samples from the polygons
ptsamp <- spsample(samp, 300, type='regular')
# add the land cover id to the points
ptsamp$id <- over(ptsamp, samp)$id
# extract values with points
df <- raster::extract(burnt_p, ptsamp)
# to see some of the reflectance values
head(df)

# plot the training sites over the burnt_p bands to visualize the distribution of sampling locations 
plt <- levelplot(burnt_p, col.regions = classcolor, main = "Distribution of Training Sites")
plt

# extract the layer values for the locations
sampvals <- extract(burnt_p, ptsamp, df = TRUE)
sampvals <- sampvals[, -1]
# combine the id information with extracted values
sampdata <- data.frame(classvalue = ptsamp$id, sampvals)

# train the classification algorithm using training dataset

# Train the model (trained classification model - cart)
cart <- rpart(as.factor(classvalue)~., data=sampdata, method = 'class', minsplit = 5)
# plot classification tree
plot(cart, uniform=TRUE, main="Classification Tree")
text(cart, cex = 0.8)

# classify all cells in the burnt_p stack (make predictions)
pr2020 <- predict(burnt_p, cart, type='class')
pr2020
# plot using rasterVis
pr2020 <- ratify(pr2020)
rat <- levels(pr2020)[[1]]
rat$legend <- c("Burnt","Mixed burnt","Not burnt")
levels(pr2020) <- rat
levelplot(pr2020, maxpixels = 1e6, col.regions = classcolor, att = "legend", scales=list(draw=FALSE), main = "Supervised classification - Burnt Area")


#---DEFORESTATION IN CERRADO

# change resolution cerrado B08 10m to 20m 
c_band8_10 <- raster("T23MNN_20220628T132251_B08_10m.jp2")
c_band8_20 <- aggregate(c_band8_10, fact=2)
writeRaster(c_band8_20, filename=file.path("T23MNN_20220628T132251_B08_20m.tif"), format="GTiff", overwrite=TRUE)

# import shapefile of cerrado area and crop images
shp_cerrado <-shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/shp_cerrado")
c1 <- crop(raster("T23MNN_20220628T132251_B02_20m.jp2"),shp_cerrado)
c2 <- crop(raster("T23MNN_20220628T132251_B03_20m.jp2"),shp_cerrado)
c3 <- crop(raster("T23MNN_20220628T132251_B04_20m.jp2"),shp_cerrado)
c4 <- crop(raster("T23MNN_20220628T132251_B05_20m.jp2"),shp_cerrado)
c5 <- crop(raster("T23MNN_20220628T132251_B06_20m.jp2"),shp_cerrado)
c6 <- crop(raster("T23MNN_20220628T132251_B07_20m.jp2"),shp_cerrado)
c7 <- crop(raster("T23MNN_20220628T132251_B8A_20m.jp2"),shp_cerrado)
c8 <- crop(raster("T23MNN_20220628T132251_B11_20m.jp2"),shp_cerrado)
c9 <- crop(raster("T23MNN_20220628T132251_B12_20m.jp2"),shp_cerrado)
c10 <- crop(raster("T23MNN_20220628T132251_B08_20m.tif"),shp_cerrado)

# put all the images together with the stack() and save as .tif
# defores_c <- stack(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
# defores_c
# writeRaster(defores_c, filename=file.path("defores_c.tif"), format="GTiff", overwrite=TRUE)
defores_c <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/defores_c.tif")

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# deforestation area in Cerrado
#- Unsupervised classification
set.seed(42)
defores_uc <- unsuperClass(defores_c, nClasses = 2, output = "classes")
defores_uc

ggR(defores_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

#- distances
set.seed(42)
defores_ucd <- unsuperClass(defores_c, nClasses = 2, output = "distances")
defores_ucd

ggR(defores_ucd$map, layer = 1:2, stretch="lin", geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance", direction = -1, option = "inferno") + MapTheme

#- spectral angle mapper
c_classCentroids <- defores_ucd$model$centers
defores_sam <- sam(defores_c, em = c_classCentroids, angles = TRUE)
defores_sam

ggR(defores_sam, 1:2, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle", direction = -1, option = "inferno") + MapTheme

#- Spectral unmixing
defores_umx <- mesma(defores_c, em = c_classCentroids)
defores_umx

ggR(defores_umx, 1:2, geom_raster = TRUE) + scale_fill_viridis_c(name = "Probability", option = "inferno") + MapTheme

#- Supervised classification
defores_c <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/defores_c.tif")
names(defores_c) <- c('blue', 'green', 'red', 'red_5', 'red_6', 'red_7', 'n_NIR', 'SWIR1', 'SWIR2', 'NIR')

# The class names and colors for plotting
nlcdclass2 <- c("Vegetation", "Land use change")
classdf2 <- data.frame(classvalue1 = c(1,2), classnames1 = nlcdclass)
# Hex codes of colors
classcolor2 <- c("#5475A8", "#B50000")

# Import shp of pantanal with the polygons from each class (vegetation and land use change)
samp2 <- shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/s_shp_cerrado")
samp2
# generate 300 point samples from the polygons
ptsamp2 <- spsample(samp2, 300, type='regular')
# add the land cover id to the points
ptsamp2$id <- over(ptsamp2, samp2)$id
# extract values with points
df2 <- raster::extract(defores_c, ptsamp2)
# to see some of the reflectance values
head(df2)

# plot the training sites over the burnt_p bands to visualize the distribution of sampling locations 
plt2 <- levelplot(defores_c, col.regions = classcolor2, main = "Distribution of Training Sites")
plt2

# extract the layer values for the locations
sampvals2 <- extract(defores_c, ptsamp2, df = TRUE)
sampvals2 <- sampvals2[, -1]
# combine the id information with extracted values
sampdata2 <- data.frame(classvalue = ptsamp2$id, sampvals2)

# train the classification algorithm using training dataset

# Train the model (trained classification model - cart)
cart2 <- rpart(as.factor(classvalue)~., data=sampdata2, method = 'class', minsplit = 5)
# plot classification tree
plot(cart2, uniform=TRUE, main="Classification Tree")
text(cart2, cex = 0.8)

# classify all cells in the burnt_p stack (make predictions)
pr2022 <- predict(defores_c, cart2, type='class')
pr2022
# plot using rasterVis
pr2022 <- ratify(pr2022)
rat2 <- levels(pr2022)[[1]]
rat2$legend <- c("Vegetation","Land use change")
levels(pr2022) <- rat2
levelplot(pr2022, maxpixels = 1e6, col.regions = classcolor2, att = "legend", scales=list(draw=FALSE), main = "Supervised classification - Land use Area")


#---FOREST TYPES BOLZANO ITALY

# change resolution bolzano B08 10m to 20m 
b_band8_10 <- raster("T23MNN_20220628T132251_B08_10m.jp2")
b_band8_20 <- aggregate(b_band8_10, fact=2)
writeRaster(c_band8_20, filename=file.path("T23MNN_20220628T132251_B08_20m.tif"), format="GTiff", overwrite=TRUE)

# import shapefile of cerrado area and crop images
shp_cerrado <-shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/shp_cerrado")
c1 <- crop(raster("T23MNN_20220628T132251_B02_20m.jp2"),shp_cerrado)
c2 <- crop(raster("T23MNN_20220628T132251_B03_20m.jp2"),shp_cerrado)
c3 <- crop(raster("T23MNN_20220628T132251_B04_20m.jp2"),shp_cerrado)
c4 <- crop(raster("T23MNN_20220628T132251_B05_20m.jp2"),shp_cerrado)
c5 <- crop(raster("T23MNN_20220628T132251_B06_20m.jp2"),shp_cerrado)
c6 <- crop(raster("T23MNN_20220628T132251_B07_20m.jp2"),shp_cerrado)
c7 <- crop(raster("T23MNN_20220628T132251_B8A_20m.jp2"),shp_cerrado)
c8 <- crop(raster("T23MNN_20220628T132251_B11_20m.jp2"),shp_cerrado)
c9 <- crop(raster("T23MNN_20220628T132251_B12_20m.jp2"),shp_cerrado)
c10 <- crop(raster("T23MNN_20220628T132251_B08_20m.tif"),shp_cerrado)

# put all the images together with the stack() and save as .tif
# defores_c <- stack(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
# defores_c
# writeRaster(defores_c, filename=file.path("defores_c.tif"), format="GTiff", overwrite=TRUE)
defores_c <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/defores_c.tif")

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# deforestation area in Cerrado
#- Unsupervised classification
set.seed(42)
defores_uc <- unsuperClass(defores_c, nClasses = 2, output = "classes")
defores_uc

ggR(defores_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

#- distances
set.seed(42)
defores_ucd <- unsuperClass(defores_c, nClasses = 2, output = "distances")
defores_ucd

ggR(defores_ucd$map, layer = 1:2, stretch="lin", geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance", direction = -1, option = "inferno") + MapTheme

#- spectral angle mapper
c_classCentroids <- defores_ucd$model$centers
defores_sam <- sam(defores_c, em = c_classCentroids, angles = TRUE)
defores_sam

ggR(defores_sam, 1:2, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle", direction = -1, option = "inferno") + MapTheme

#- Spectral unmixing
defores_umx <- mesma(defores_c, em = c_classCentroids)
defores_umx

ggR(defores_umx, 1:2, geom_raster = TRUE) + scale_fill_viridis_c(name = "Probability", option = "inferno") + MapTheme

#- Supervised classification
defores_c <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/defores_c.tif")
names(defores_c) <- c('blue', 'green', 'red', 'red_5', 'red_6', 'red_7', 'n_NIR', 'SWIR1', 'SWIR2', 'NIR')

# The class names and colors for plotting
nlcdclass2 <- c("Vegetation", "Land use change")
classdf2 <- data.frame(classvalue1 = c(1,2), classnames1 = nlcdclass)
# Hex codes of colors
classcolor2 <- c("#5475A8", "#B50000")

# Import shp of pantanal with the polygons from each class (vegetation and land use change)
samp2 <- shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/s_shp_cerrado")
samp2
# generate 300 point samples from the polygons
ptsamp2 <- spsample(samp2, 300, type='regular')
# add the land cover id to the points
ptsamp2$id <- over(ptsamp2, samp2)$id
# extract values with points
df2 <- raster::extract(defores_c, ptsamp2)
# to see some of the reflectance values
head(df2)

# plot the training sites over the burnt_p bands to visualize the distribution of sampling locations 
plt2 <- levelplot(defores_c, col.regions = classcolor2, main = "Distribution of Training Sites")
plt2

# extract the layer values for the locations
sampvals2 <- extract(defores_c, ptsamp2, df = TRUE)
sampvals2 <- sampvals2[, -1]
# combine the id information with extracted values
sampdata2 <- data.frame(classvalue = ptsamp2$id, sampvals2)

# train the classification algorithm using training dataset

# Train the model (trained classification model - cart)
cart2 <- rpart(as.factor(classvalue)~., data=sampdata2, method = 'class', minsplit = 5)
# plot classification tree
plot(cart2, uniform=TRUE, main="Classification Tree")
text(cart2, cex = 0.8)

# classify all cells in the burnt_p stack (make predictions)
pr2022 <- predict(defores_c, cart2, type='class')
pr2022
# plot using rasterVis
pr2022 <- ratify(pr2022)
rat2 <- levels(pr2022)[[1]]
rat2$legend <- c("Vegetation","Land use change")
levels(pr2022) <- rat2
levelplot(pr2022, maxpixels = 1e6, col.regions = classcolor2, att = "legend", scales=list(draw=FALSE), main = "Supervised classification - Land use Area")


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
