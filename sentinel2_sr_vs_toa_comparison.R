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
  by = c("system.index", "id2")
)

# Extract date and sensor info from system.index
df <- df %>% 
  mutate(system.index = case_when(str_sub(system.index, 1, 2) == "20" ~ 
                                    paste0("SE02_123456_", system.index),
                                  TRUE ~ system.index),
         sensor = str_sub(system.index, 1, 4),
         date = str_sub(system.index, 13, 20) %>% as.Date(format = "%Y%m%d"))

# Plot comparison

# Plot nir_red_br
filter(df, id2 == 2) %>% 
  ggplot()+
  geom_line(aes(date, nir_red_br.x)) + 
  geom_line( aes(date, nir_red_br.y), color = "red")

# Add FAI data 
df <- df %>% 
  mutate(fai2.x = NIR.x/10000 - (RED.x/10000 + (SWIR1.x/10000 - RED.x/10000) * ((865 - 655) / (1610 - 655))),
         fai2.y = NIR.y/10000 - (RED.y/10000 + (SWIR1.y/10000 - RED.y/10000) * ((865 - 655) / (1610 - 655))))

# Plot the FAI
filter(df, id2 == 3) %>% 
  ggplot()+
  geom_line(aes(date, fai2.x)) + 
  geom_line( aes(date, fai2.y), color = "red")


# Add NDWI from NIR and SWIR1
df <- df %>% 
  mutate(ndwi.x = (NIR.x - SWIR1.x) / (NIR.x + SWIR1.x) ,
         ndwi.y = (NIR.y - SWIR1.y) / (NIR.y + SWIR1.y))

# Plot the NDWI
filter(df, id2 == 1) %>% 
  ggplot()+
  geom_line(aes(date, ndwi.x)) + 
  geom_line( aes(date, ndwi.y), color = "red")


# Determine which points meet Oyama cyonabac thresholds
test <- filter(df, ndwi.y > 0.6 & fai2.y > 0.05)
unique(test$id2)
test_summary <- test %>% group_by(id2) %>% summarise(n = n())



