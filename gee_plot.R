# Script for analyzing processed data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-09-24

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())

# Add data directory string
data.dir <- "../data/gee/"

# Import processed data
gee <- readRDS(paste0(data.dir, "sensors_all_aggregated.RDS"))

# Compute new FAI
# 'NIR - (RED + (SWIR1 - RED) * ((865 - 655) / (1610 - 655)))'
band_scalar = 10000
gee <- gee %>% 
  mutate(fai2 = NIR/band_scalar - (RED/band_scalar + (SWIR1/band_scalar - RED/band_scalar) * ((865 - 655) / (1610 - 655))))

filter(gee, id2 == 200) %>% ggplot(aes(date, nir_red_br, color = sensor)) + geom_line()


test <- left_join(filter(gee, id2 == 3 & sensor == "SE02"),
                  filter(gee, id2 == 2 & sensor == "SE02"),
                  by = "date")

ggplot(test, aes(RED.x, RED.y)) + geom_point()

