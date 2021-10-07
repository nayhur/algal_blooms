# 2021 OSMP algae analysis
# Remote sensing imagery processed with Google Earth Engine

# Keith Jennings
# kjennings@lynker.com
# 2021-10-06

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(lubridate)

# Import data

# Source date functions
devtools::source_url("https://raw.githubusercontent.com/SnowHydrology/date_functions/main/doy_dowy.R")

# Bind into a single data frame
df <- read.csv("../data/gee/time_series/sentinel2_toa_pts_2021.csv")

# Add prefix to beginning of sentinel 2 strings to match landsat patterns
# Add sensor and date columns
df <- df %>% 
  mutate(system.index = case_when(str_sub(system.index, 1, 2) == "20" ~ 
                                    paste0("SE02_123456_", system.index),
                                  TRUE ~ system.index),
         sensor = str_sub(system.index, 1, 4),
         date = str_sub(system.index, 13, 20) %>% as.Date(format = "%Y%m%d")) %>% 
  select(-.geo)


# Remove system.index and extra QA columns
df <- df %>% 
  select(., -system.index, -QA60)

# Add date information
df <- df %>% 
  mutate(year = year(date),
         month = month(date),
         doy = doy_FUN(date))

# Export as RDS file
df %>% 
  saveRDS(paste0("../data/gee/processed/sensors_all_aggregated_2021.RDS"))

