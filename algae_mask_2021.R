# Script to mask rasters for mapping

# Load packages
library(raster)
library(rgdal)

# Specify the directory and name of input raster
input.dir = "../data/gee/images/"
input.ras = "fai_ndwi_pix_over_thresh_sentinel2_toa_2021.tif"

# Specify the mask polygon
mask.dir  = "../data/geospatial/important_water_bodies/"
mask.poly = "osmp_waterbodies_all_v2_4326"

# Specify the output directory and file name
output.dir = "../data/gee/processed/"
output.ras = "fai_ndwi_sentinel2_2021_masked.tif"

# Import raster
r <- raster(paste0(input.dir, input.ras))

# Import mask
m <- readOGR(dsn = mask.dir, layer = mask.poly)

# Mask the raster
rm <- mask(r, m)

# Export the raster
writeRaster(rm, paste0(output.dir, output.ras))