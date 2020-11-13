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

# Get CRS of the GEE imagery
crs_obs <- raster(paste0(data.dir, files[1, "file"])) %>% crs(.)

# Import waterbody data (both full size and 30 m buffer)
# Then reproject to CRS of imagery
wb_whole <- readOGR(dsn = "../data/geospatial/important_water_bodies", 
                    layer = "osmp_waterbodies_all_v2") %>% 
  spTransform(., crs_obs)
wb_30mbf <- readOGR(dsn = "../data/geospatial/important_water_bodies", 
                    layer = "osmp_waterbodies_all_v2_30m_buffer")%>% 
  spTransform(., crs_obs)

################################################################################
# 2 Extract data from each raster

# Create a threshold for counting number of cells over n occurrences 
cell_thresh = 3 # means the pixel must have exceeded the threshold this many times

# Create dummy data frame to store data
wb_extract <- data.frame()

# Loop through each file
for(i in 1:length(files$file)){
  
  # Get temporary info
  tmp.info <- files[i, ]
  
  # Import imagery as raster
  tmp.r <- raster(paste0(data.dir, tmp.info[, "file"]))
  
  # Extract max value using whole and buffered waterbodies
  tmp.max.whole <- extract(tmp.r, wb_whole, fun = max)
  tmp.max.30mbf <- extract(tmp.r, wb_30mbf, fun = max)
  
  # Count cells over count threshold
  tmp.count.whole <- extract(tmp.r, wb_whole, 
                         fun = function(x, ...) sum(x >= cell_thresh))
  tmp.count.30mbf <- extract(tmp.r, wb_30mbf, 
                             fun = function(x, ...) sum(x >= cell_thresh))
  
  
  # Make dataframe for wb_whole
  tmp.whole <- wb_whole@data %>% 
    mutate(max_count = tmp.max.whole,
           over_count = tmp.count.whole,
           sensor = tmp.info[1, "sensor"],
           algorithm = tmp.info[1, "algorithm"],
           year = tmp.info[1, "year"],
           wb_cat = "whole")
  
  # Make dataframe for wb_30mbf
  tmp.30mbf <- wb_30mbf@data %>% 
    mutate(max_count = tmp.max.30mbf,
           over_count = tmp.count.30mbf,
           sensor = tmp.info[1, "sensor"],
           algorithm = tmp.info[1, "algorithm"],
           year = tmp.info[1, "year"],
           wb_cat = "30mbf")
  
  # Bind all to existing dataframe
  wb_extract <- bind_rows(wb_extract, tmp.whole, tmp.30mbf)
}

# Record "hits" for fai/ndwi and nir:red per waterbody per year per sensor