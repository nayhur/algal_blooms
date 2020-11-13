# Script for extracting NIR:Red and FAI/NDWI data from OSMP waterbodies
# Data were preprocessed in Google Earth Engine using top of atmosphere reflectance

# Keith Jennings
# kjennings@lynkertech.com
# 2020-11-13

# Load packages
library(tidyverse)
library(raster)
library(sp)
library(rgdal)

################################################################################
# 1 Import data

# Add data directory string
data.dir <- "../data/gee/images/"

# List files in Google Earth Engine folder
# And get relevant sensor, algorithm, and year info
files <- data.frame(file = list.files(data.dir, pattern = "tif$")) %>% 
  mutate(sensor = case_when(str_detect(file, "landsat5") == T ~ "landsat5",
                            str_detect(file, "landsat7") == T ~ "landsat7",
                            str_detect(file, "landsat8") == T ~ "landsat8",
                            str_detect(file, "sentinel2") == T ~ "sentinel2"),
         algorithm = case_when(str_detect(file, "nir_red") == T ~ "nir_red_br",
                               str_detect(file, "fai") == T ~ "fai_ndwi"),
         year = str_sub(file, -8, -5) %>% as.numeric())

# Import waterbody data (both full size and 30 m buffer)
wb_

# Reproject waterbodies to be same projection as raster data
# Loop through each file
# Import as raster
# Set all values < 2 or 1 to 0
# Raster extract to sum cells per waterbody for both shapefiles
# Also get max, min, and mean
# Record "hits" for fai/ndwi and nir:red per waterbody per year per sensor