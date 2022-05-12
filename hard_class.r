# R code for testing RSTollbox distances and spectral angle mapper

install.packages("raster")
install.packages("rgdal")

library(raster)
library(RStoolbox)
library(ggplot2)

setwd("/Users/anareis/OneDrive/MSc_Unibo/Thesis/data/")

# import Sentinel 2 data
# creat lists with the images
list1 <- list.files(pattern = "T21KVU") 
list1 # 9 images

list2 <- list.files(pattern = "T22KBC")
list2 # 9 images

# import the single layers with raster()
import1 <- lapply(list1,raster)
import1

import2 <- lapply(list2,raster)
import2


