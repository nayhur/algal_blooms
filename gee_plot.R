# Script for analyzing processed data

# Keith Jennings
# kjennings@lynkertech.com
# 2020-09-24

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(lubridate)

# Add data directory string
data.dir <- "../data/gee/"

# Import processed data
gee <- readRDS(paste0(data.dir, "processed/sensors_all_aggregated.RDS"))

# Import pt metadata
pts_meta <- read.csv("../data/geospatial/metadata/osmp_sampling_pts_meta.csv")

# Add month and year columns
gee <- gee %>% 
  mutate(month = month(date),
         year = year(date))

# Join the pts metadata
gee <- left_join(gee, 
                 pts_meta,
                 by = "id2")

# Compute % of obs over nir:red thresh per year, per sensor
# note: could also do this per point
# need to add edge info via metadata 
nir_red_thresh <- 1.5
pts_nir_red_over <- gee %>% 
  filter(month %in% 4:10 & !is.na(id2)) %>% 
  group_by(sensor, year, edge) %>% 
  summarise(n_over = sum(nir_red_br >= nir_red_thresh),
            n_obs = n()) %>% 
  mutate(pct_over = n_over / n_obs * 100)


# plot
ggplot(pts_nir_red_over, aes(year, pct_over, color = sensor)) + 
  geom_line() + 
  facet_wrap(~edge)

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

