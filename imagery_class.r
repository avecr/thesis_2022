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

# import shapefile of pantanal area, read all bands
shp_pantanal <-shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/shp_pantanal")
p_lst <- list.files(pattern = "T21KWB_20201003T140059_B.*20m", full.names = TRUE)
p_rst <- lapply(p_lst, FUN = raster)
crop_p_rst <- lapply(p_rst, FUN = raster::crop, y = shp_pantanal)
burnt_p <- stack(crop_p_rst)
burnt_p
writeRaster(burnt_p, filename=file.path("burnt_p.tif"), format="GTiff", overwrite=TRUE)

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
b_band8_10 <- raster("T32TPS_20200905T101031_B08_10m.jp2")
b_band8_20 <- aggregate(b_band8_10, fact=2)
writeRaster(b_band8_20, filename=file.path("T32TPS_20200905T101031_B08_20m.tif"), format="GTiff", overwrite=TRUE)

# import shapefile of cerrado area and crop images
shp_bolzano <-shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/extent_polygon")
b1 <- crop(raster("T32TPS_20200905T101031_B02_20m.jp2"),shp_bolzano)
b2 <- crop(raster("T32TPS_20200905T101031_B03_20m.jp2"),shp_bolzano)
b3 <- crop(raster("T32TPS_20200905T101031_B04_20m.jp2"),shp_bolzano)
b4 <- crop(raster("T32TPS_20200905T101031_B05_20m.jp2"),shp_bolzano)
b5 <- crop(raster("T32TPS_20200905T101031_B06_20m.jp2"),shp_bolzano)
b6 <- crop(raster("T32TPS_20200905T101031_B07_20m.jp2"),shp_bolzano)
b7 <- crop(raster("T32TPS_20200905T101031_B8A_20m.jp2"),shp_bolzano)
b8 <- crop(raster("T32TPS_20200905T101031_B11_20m.jp2"),shp_bolzano)
b9 <- crop(raster("T32TPS_20200905T101031_B12_20m.jp2"),shp_bolzano)
b10 <- crop(raster("T32TPS_20200905T101031_B08_20m.tif"),shp_bolzano)

# put all the images together with the stack() and save as .tif
# bolzano_c <- stack(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
# bolzano_c
# writeRaster(bolzano_c, filename=file.path("bolzano_c.tif"), format="GTiff", overwrite=TRUE)
bolzano_c <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/bolzano_c.tif")

# MapTheme for plotting
MapTheme <- list(theme(
 axis.text = element_text(size = 6),  
 axis.text.y = element_text(angle = 90, hjust = 0.5),
 axis.title = element_blank()
))  

# continuous environment in Bolzano
#- Unsupervised classification
set.seed(42)
bolzano_uc <- unsuperClass(bolzano_c, nClasses = 3, output = "classes")
bolzano_uc

ggR(bolzano_uc$map, forceCat = TRUE, geom_raster = TRUE) + scale_fill_viridis_d(name = "Cluster", option = "A") + MapTheme

#- distances
set.seed(42)
bolzano_ucd <- unsuperClass(bolzano_c, nClasses = 3, output = "distances")
bolzano_ucd

ggR(bolzano_ucd$map, layer = 1:3, stretch="lin", geom_raster = TRUE) + scale_fill_viridis_c(name = "Distance", direction = -1, option = "inferno") + MapTheme

#- spectral angle mapper
c_classCentroids <- bolzano_ucd$model$centers
bolzano_sam <- sam(bolzano_c, em = c_classCentroids, angles = TRUE)
bolzano_sam

ggR(bolzano_sam, 1:3, geom_raster = TRUE) + scale_fill_viridis_c(name = "Spectral angle", direction = -1, option = "inferno") + MapTheme

#- Spectral unmixing
bolzano_umx <- mesma(bolzano_c, em = c_classCentroids)
bolzano_umx

ggR(bolzano_umx, 1:3, geom_raster = TRUE) + scale_fill_viridis_c(name = "Probability", option = "inferno") + MapTheme

#- Supervised classification
bolzano_c <- stack("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/bolzano_c.tif")
names(bolzano_c) <- c('blue', 'green', 'red', 'red_5', 'red_6', 'red_7', 'n_NIR', 'SWIR1', 'SWIR2', 'NIR')

# The class names and colors for plotting
nlcdclass3 <- c("Beech forest", "Spruce forest", "Pine forest")
classdf3 <- data.frame(classvalue1 = c(1, 2, 3), classnames1 = nlcdclass3)
# Hex codes of colors
classcolor3 <- c("#5475A8", "#B50000", "#D2CDC0")

# Import shp of bolzano with the polygons from each class/forest (spruce, beech and pine)
samp3 <- shapefile("/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata/s_shp_bolzano")
samp3
# generate 300 point samples from the polygons
ptsamp3 <- spsample(samp3, 300, type='regular')
# add the land cover id to the points
ptsamp3$id <- over(ptsamp3, samp3)$id
# extract values with points
df3 <- raster::extract(bolzano_c, ptsamp3)
# to see some of the reflectance values
head(df3)

# plot the training sites over the burnt_p bands to visualize the distribution of sampling locations 
plt3 <- levelplot(bolzano_c, col.regions = classcolor3, main = "Distribution of Training Sites - Bolzano Area")
plt3

# extract the layer values for the locations
sampvals3 <- extract(bolzano_c, ptsamp3, df = TRUE)
sampvals3 <- sampvals3[, -1]
# combine the id information with extracted values
sampdata3 <- data.frame(classvalue = ptsamp3$id, sampvals3)

# train the classification algorithm using training dataset

# Train the model (trained classification model - cart)
cart3 <- rpart(as.factor(classvalue)~., data=sampdata3, method = 'class', minsplit = 5)
# plot classification tree
plot(cart3, uniform=TRUE, main="Classification Tree")
text(cart3, cex = 0.8)

# classify all cells in the burnt_p stack (make predictions)
pr2022 <- predict(bolzano_c, cart3, type='class')
pr2022
# plot using rasterVis
pr2022 <- ratify(pr2022)
rat2 <- levels(pr2022)[[1]]
rat2$legend <- c("Spruce forest","Beech forest", "Pinewood forest")
levels(pr2022) <- rat2
levelplot(pr2022, maxpixels = 1e6, col.regions = classcolor3, att = "legend", scales=list(draw=FALSE), main = "Supervised classification - Bolzano Area")

#--- validate the methods
# burnt area unsupervised class
# polygons to points - read the training polygons
poly_burnt <- rgdal::readOGR(dsn   = "/Users/anareis/Library/CloudStorage/OneDrive-Personal/Thesis/data/auxdata", 
                       layer = "ref_burnt", 
                       stringsAsFactors = FALSE)
poly_burnt@data$id <- as.integer(factor(poly_burnt@data$burnt_area))
setDT(poly_burnt@data)

# the polygons need to be projected using the Sentinel's CRS
poly_burnt_utm <- sp::spTransform(poly_burnt, CRSobj = crop_burnt_p[[1]]@crs)

# Create raster template
template_rst <- raster(extent(burnt_p$burnt_p.1), 
                       resolution = 20,
                       crs = projection(burnt_p))

poly_burnt_rst <- rasterize(poly_burnt_utm, template_rst, field = 'id')
plot(poly_burnt_rst)

poly_burnt_dt <- as.data.table(rasterToPoints(poly_burnt_rst))
setnames(poly_burnt_dt, old = "layer", new = "id_cls")

burnt_points <- SpatialPointsDataFrame(coords = poly_burnt_dt[, .(x, y)],
                                 data = poly_burnt_dt,
                                 proj4string = poly_burnt_rst@crs)

# re sample bands
#rst_for_prediction <- vector(mode = "list", length = length(rst_crop_lst))
#names(rst_for_prediction) <- names(rst_crop_lst)

brick_crop_burnt_p <- brick(crop_burnt_p)
plot(crop_burnt_p$T21KWB_20201003T140059_B08_20m)

# center and scale raster images
brick_burnt_norm <- normImage(brick_crop_burnt_p)
names(brick_burnt_norm) <- names(brick_crop_burnt_p)

# extract band values to points
burnt_dt <- brick_burnt_norm %>% 
  extract(y = burnt_points) %>% 
  as.data.table %>% 
  .[, id_cls := burnt_points@data$id_cls] %>%  # add the class names to each row
  merge(y = unique(poly_burnt@data), by.x = "id_cls", by.y = "id", all = TRUE, sort = FALSE) %>% 
  .[, id_cls := NULL]

View(burnt_dt)

# histogram of predictors
burnt_dt %>% 
  select(-"burnt_area") %>% 
  melt(measure.vars = names(.)) %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  geom_vline(xintercept = 0, color = "gray70") +
  facet_wrap(facets = vars(variable), ncol = 3)

# split into train and test
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(burnt_dt$burnt_area,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)
b_dt_train <- burnt_dt[idx_train]
b_dt_test <- burnt_dt[idx_train]

table(b_dt_train$burnt_area)
table(b_dt_test$burnt_area)

# fit models
# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(b_dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model

ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

# Register a doParallel cluster, using 3/4 (75%) of total CPU-s
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
make.names(b_dt_train, unique = FALSE, allow_ = TRUE)
model_rf <- caret::train(burnt_area ~ ., method = "rf", data = b_dt_train,
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)

stopCluster(cl); remove(cl)
# Unregister the doParallel cluster so that we can use sequential operations
# if needed; details at https://stackoverflow.com/a/25110203/5193830
registerDoSEQ()
saveRDS(model_rf, file = "./model_rf.rds")

# Model summary & confusion matrix
model_rf$times$everything # total computation time
ggplot(model_rf) # tuning results

# setting burnt_area to a factor
burnt_area <- as.factor(b_dt_test$burnt_area)
str(b_dt_test)

test_predictions = predict(model_rf, b_dt_test)
predictrf <- raster :: predict(object = burnt_p, model = model_rf, type = "raw") # raster of the supervised classification  
test_predictions

# Compute the confusion matrix
cm_burnt <- confusionMatrix(test_predictions, burnt_area)
cm_burnt

# Confusion matrix for the final model (entire train dataset)
model_rf$finalModel



