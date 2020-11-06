# Comparison script for Landsat surface reflectance vs. top of atmosphere

# Keith Jennings
# kjennings@lynkertech.com
# 2020-11-06

# Load packages
library(tidyverse)

# Add data directory string
data.dir <- "../data/gee/"

# Import the two datasets and join
df <- left_join(
  read.csv(paste0(data.dir, "landsat8_sr_pts.csv"),
           stringsAsFactors = F),
  read.csv(paste0(data.dir, "landsat8_toa_pts.csv"),
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
ggplot(df, aes(nir_red_br.x, nir_red_br.y)) +
  geom_point()

# Plot nir_red_br
filter(df, id2 == 2) %>% 
  ggplot()+
  geom_line(aes(date, nir_red_br.x)) + 
  geom_line( aes(date, nir_red_br.y), color = "red")

# Add FAI data 
df <- df %>% 
  mutate(fai2.x = NIR.x/10000 - (RED.x/10000 + (SWIR1.x/10000 - RED.x/10000) * ((865 - 655) / (1610 - 655))),
         fai2.y = NIR.y - (RED.y + (SWIR1.y - RED.y) * ((865 - 655) / (1610 - 655))))

# Plot the FAI
filter(df, id2 == 2) %>% 
  ggplot()+
  geom_line(aes(date, fai2.x)) + 
  geom_line( aes(date, fai2.y), color = "red")



