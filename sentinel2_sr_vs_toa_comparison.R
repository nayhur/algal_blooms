# Comparison script for Sentinel 2 surface reflectance vs. top of atmosphere
# TOA has longer record relative to TOA

# Keith Jennings
# kjennings@lynkertech.com
# 2020-11-06

# Load packages
library(tidyverse)

# Add data directory string
data.dir <- "../data/gee/"

# Import the two datasets and join
df <- left_join(
  read.csv(paste0(data.dir, "sentinel2_sr_pts.csv"),
           stringsAsFactors = F),
  read.csv(paste0(data.dir, "sentinel2_toa_pts.csv"),
           stringsAsFactors = F),
  by = "system.index"
)
