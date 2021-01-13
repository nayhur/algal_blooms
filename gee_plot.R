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
plot.dir <- "../plots/"

# Import processed time series data
gee <- readRDS(paste0(data.dir, "processed/sensors_all_aggregated.RDS"))

# Import pt metadata
pts_meta <- read.csv("../data/geospatial/metadata/osmp_sampling_pts_meta_v2.csv")

# Join the pts metadata
gee <- left_join(gee, 
                 pts_meta,
                 by = "id2")

# Add a binary column for yes-no decision on cyanobac
fai_thresh = 0.05
ndwi_thresh = 0.63
gee <- gee %>% 
  mutate(cyano = case_when(fai >= fai_thresh & ndwi >= ndwi_thresh ~ 1,
                           TRUE ~ 0))

# Recode edge column with more descriptive names
gee <- gee %>% 
  mutate(edge2 = case_when(edge == "yes" ~ "Edge",
                           edge == "maybe" ~ "Near Edge",
                           edge == "no" ~ "Open Water"))

# Import the processed waterbody imagery
wbs <- readRDS(paste0(data.dir, "processed/wb_extract.RDS"))

# Compute % of obs over nir:red thresh per year, per sensor
# note: could also do this per point
nir_red_thresh = 1
pts_nir_red_over <- gee %>% 
  filter(month %in% 5:10 & !is.na(id2) & edge %in% c("yes", "no", "maybe")) %>% 
  group_by(sensor, year, edge2) %>% 
  summarise(n_over = sum(nir_red_br >= nir_red_thresh),
            n_obs = n()) %>% 
  mutate(pct_over = n_over / n_obs * 100)


# plot
filter(pts_nir_red_over, sensor %in% c("LT05", "LE07") & year >= 1990) %>% 
  ggplot( aes(year, pct_over, color = sensor)) + 
  geom_line(lwd = 1) + 
  facet_wrap(~edge2) +
  labs(x = "Year", 
       y = "Observations with NIR:Red > 1 (%)") +
  scale_color_manual(name = "Sensor", 
                     breaks = c("LT05", "LE07"),
                     labels = c("Landsat 5", "Landsat 7"),
                     values = c("brown", "cyan4")) +
  theme(axis.text.x = element_text(angle = -30, hjust =0)) +
  geom_smooth(method = "lm")

# Compute % of obs over FAI and NDWI thresh per year, per sensor
# note: could also do this per point
pts_fai_ndwi_over <- gee %>% 
  filter(month %in% 5:10 & !is.na(id2) & edge %in% c("yes", "no", "maybe")) %>% 
  group_by(sensor, year, edge2) %>% 
  summarise(n_over = sum(fai > fai_thresh & ndwi >= ndwi_thresh),
            n_obs = n()) %>% 
  mutate(pct_over = n_over / n_obs * 100)


# plot
# plot
pts_fai_ndwi_over_plot <-
  filter(pts_fai_ndwi_over, sensor %in% c("LT05", "LE07") & year >= 1990) %>% 
  ggplot( aes(year, pct_over, color = sensor)) + 
  geom_line(lwd = 1) + 
  facet_wrap(~edge2) +
  labs(x = "Year", 
       y = "Obs. Over FAI & NDWI Thresholds (%)") +
  scale_color_manual(name = "Sensor", 
                     breaks = c("LT05", "LE07"),
                     labels = c("Landsat 5", "Landsat 7"),
                     values = c("brown", "cyan4")) +
  theme(axis.text.x = element_text(angle = -30, hjust =0))
save_plot(plot = pts_fai_ndwi_over_plot,
          filename = paste0(plot.dir, "pts_fai_ndwi_over_by_year.png"),
          base_height = 5, base_width = 7)

# Analysis showing NIR:Red evolution at Wonderland Lake for LT05 versus LE07
#make 10 day movign average filter
ma10 <- function(x, n = 10){stats::filter(x, rep(1 / n, n), sides = 2)}
wonderland <- filter(gee, id2 == 3 & sensor %in% c("LT05", "LE07") & month %in% 4:10) %>% 
  group_by(sensor, doy) %>% 
  summarise(nir_red_br_av = mean(nir_red_br)) %>% 
  mutate(nir_red_br_av_ma10 = ma10(nir_red_br_av))

# Plot wonderland
ggplot(wonderland, aes(doy, nir_red_br_av_ma10, color = sensor))+
  geom_line(lwd = 1) +
  labs(x = "Day of Year",
       y = "Average NIR:Red Ratio") +
  scale_color_manual(name = "Sensor", 
                     breaks = c("LT05", "LE07"),
                     labels = c("Landsat 5", "Landsat 7"),
                     values = c("brown", "cyan4")) +
  theme(axis.text.x = element_text(angle = -30, hjust =0)) +
  scale_x_continuous(breaks = c(91, 152, 213, 274),
                     labels = c("Apr. 1", "Jun. 1", "Aug. 1", "Oct. 1"))



##
# Summarize the pct of occurrences per waterbody for L5 and l7 for near edge and open
wbs_nir_over <- filter(gee, !is.na(edge2) & month %in% 6:10) %>% 
  group_by(waterbody, edge2) %>% 
  summarise(n_over = sum(nir_red_br >= nir_red_thresh),
            n_obs = n()) %>% 
  mutate(pct_over = n_over / n_obs * 100)

# Plot
wbs_nir_over_plot <-
  ggplot(wbs_nir_over, aes(waterbody, pct_over)) + 
  geom_bar(stat = "identity", color = "black", fill = "darkgreen") + 
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) + 
  facet_wrap(~edge2, ncol = 1) + 
  labs(x = "Waterbody", 
       y = "Observations Over NIR:Red Threshold (%)")
save_plot(plot = wbs_nir_over_plot,
          filename = paste0(plot.dir, "nir_red_over_by_waterbody.png"),
          base_height = 8, base_width = 12)
##
# Summarize the pct of occurrences per waterbody for L5 and l7 for near edge and open
wbs_fai_over <- filter(gee, !is.na(edge2) & month %in% 6:10) %>% 
  group_by(waterbody, edge2) %>% 
  summarise(n_over = sum(cyano),
            n_obs = n()) %>% 
  mutate(pct_over = n_over / n_obs * 100)

# Plot
wbs_fai_over_plot <-
  ggplot(wbs_fai_over, aes(waterbody, pct_over)) + 
  geom_bar(stat = "identity", color = "black", fill = "darkblue") + 
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) + 
  facet_wrap(~edge2, ncol = 1) + 
  labs(x = "Waterbody", 
       y = "Obs. Over FAI & NDWI Thresholds (%)")
save_plot(plot = wbs_fai_over_plot,
          filename = paste0(plot.dir, "fai_ndwi_over_by_waterbody.png"),
          base_height = 8, base_width = 12)


################################################################################
################################################################################
# Some data analyses:

# View top and bottom NIR open water exceedances
filter(wbs_nir_over, edge2 == "Open Water") %>% arrange(pct_over) %>% head(n = 15)
filter(wbs_nir_over, edge2 == "Open Water") %>% arrange(-pct_over) %>% head(n = 10)

# View top and bottom FAI & NDWI open water exceedances
filter(wbs_fai_over, edge2 == "Open Water") %>% arrange(pct_over) %>% head(n = 15)
filter(wbs_fai_over, edge2 == "Open Water") %>% arrange(-pct_over) %>% head(n = 15)

# Compare Landsat 7 2001-2010 to 2011-2020 for each pixel type
filter(pts_nir_red_over, year %in% 1999:2009 & sensor == "LE07") %>% 
  group_by(edge2) %>% 
  summarise(pct_over_av = mean(pct_over))
filter(pts_nir_red_over, year %in% 2010:2020 & sensor == "LE07") %>% 
  group_by(edge2) %>% 
  summarise(pct_over_av = mean(pct_over))

