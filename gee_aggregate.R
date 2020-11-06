# Script for aggregating surface reflectance data processed
# in Google Earth Engine

# Keith Jennings
# kjennings@lynkertech.com
# 2020-09-01

# Load packages
library(tidyverse)

# Add data directory string
data.dir <- "../data/gee/"

# List files in Google Earth Engine folder
files <- list.files(data.dir, pattern = "*.csv")

# Import files into list
data.l <- lapply(paste0(data.dir, files), read.csv, stringsAsFactors = F)

# Bind into a single data frame
df <- plyr::ldply(data.l, bind_rows)

# Add prefix to beginning of sentinel 2 strings to match landsat patterns
# Add sensor and date columns
df <- df %>% 
  mutate(system.index = case_when(str_sub(system.index, 1, 2) == "20" ~ 
                                     paste0("SE02_123456_", system.index),
                                   TRUE ~ system.index),
         sensor = str_sub(system.index, 1, 4),
         date = str_sub(system.index, 13, 20) %>% as.Date(format = "%Y%m%d"))

# Put QA values into same column
# Identify whether data are from a sampling point or waterbody average
df <- df %>% 
  mutate(qa_val = case_when(!is.na(pixel_qa) ~ pixel_qa,
                            !is.na(QA60) ~ QA60),
         type = case_when(!is.na(id2) ~ "pt",
                          TRUE ~ "wb"))

# Export as RDS file
df %>% 
  select(sensor, date, type, id2, bcpos_id, osmp_id, 
         BLU:hrwi, lswi:nir_red_br, qa_val) %>% 
  saveRDS(paste0(data.dir, "sensors_all_aggregated.RDS"))

# Example plot
filter(df, id2 == 4 & date > as.Date("2017-04-01")) %>% 
  ggplot() +
  geom_line(aes(date, nir_red_br, color = sensor))
