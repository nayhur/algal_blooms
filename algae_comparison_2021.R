# Comparison script for remote sensing output versus chl-a sampling

# Keith Jennings
# kjennings@lynker.com
# 2021-10-25

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(lubridate)
library(RColorBrewer)

# Add data directory string
data.dir <- "../data/"
plot.dir <- "../plots/"

# Import processed remote sesning data and chl-a data
gee <- readRDS(paste0(data.dir, "gee/processed/sensors_all_aggregated_2021.RDS"))
chla <- read.csv(paste0(data.dir, "water_quality/in_situ_2021/chla_2021.csv")) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"))
cyano <- read.csv(paste0(data.dir, "water_quality/in_situ_2021/cyano_2021.csv")) %>% 
  rename(sample = Sample.site,
         date = Date,
         cyano = Cyanobacteria) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         sample = tolower(sample))


# Add a second waterbody column with label-ready names
gee <- gee %>% 
  mutate(waterbody2 = case_when(waterbody == "sawhill1" ~ "Sawhill No. 1",
                                waterbody == "sombrero" ~ "Sombrero Marsh",
                                waterbody == "teller5" ~ "Teller No. 5",
                                waterbody == "wonderland" ~ "Wonderland Lake"))

# Add a binary column for yes-no decision on cyanobac
fai_thresh = 0.05
ndwi_thresh = 0.63
gee <- gee %>% 
  mutate(cyano = case_when(fai >= fai_thresh & ndwi >= ndwi_thresh ~ 1,
                           TRUE ~ 0))

# Create the color scale for the plots
sample_color_scale <-
  scale_color_manual(
    breaks = c("hill point",
               "loading shore",
               "perch",
               "orange island",
               "stinky peat",
               "thorne experience",
               "north rocky",
               "rocky",
               "downwind east",
               "peninsula"),
    labels = c("Hill Point",
               "Loading Shore",
               "Perch",
               "Orange Island",
               "Stinky Peat",
               "Thorne Experience",
               "North Rocky",
               "Rocky",
               "Downwind East",
               "Peninsula"),
    values = c(brewer.pal(3, "Set2")[1],
               brewer.pal(3, "Set2")[2],
               brewer.pal(3, "Set2")[3],
               brewer.pal(3, "Set1")[1],
               brewer.pal(3, "Set1")[2],
               brewer.pal(3, "Set1")[3],
               brewer.pal(3, "Dark2")[1],
               brewer.pal(3, "Dark2")[2],
               brewer.pal(3, "Paired")[1],
               brewer.pal(3, "Paired")[2]),
    name = "Location"
  )

# Loop through to get the NIR:Red ratio
chla$nir_red_br <- NULL
chla$fai <- NULL

for(i in 1:length(chla$code)){
  tmp.sample  = chla[i, "sample"]
  tmp.date    = chla[i, "date"]
  tmp.gee     = filter(gee, sample == tmp.sample & date == tmp.date & edge == "no")
  
  tmp.gee <- filter(gee, sample == tmp.sample & 
                      date >= tmp.date - 15 &
                      date <= tmp.date + 15 & 
                      edge == "no") 
  tmp.nir_red_br = mean(tmp.gee$nir_red_br)
  tmp.fai        = mean(tmp.gee$fai)
  chla[i, "nir_red_br"] = tmp.nir_red_br
  chla[i, "fai"]        = tmp.fai
  
}
ggplot() + 
  geom_point(data = chla, aes(nir_red_br, chl.a, color = sample)) + sample_color_scale +
  labs(x = "NIR:Red Ratio", y = "Chlorophyll-a (µg/L)") +
  geom_smooth(data = chla, aes(nir_red_br, chl.a),
              method = "lm", se = F, lty = "dashed", color = "gray") +
  ylim(0, 300)


# Loop through to get the cyano output
cyano$cyano_rs <- -9999
for(i in 1:length(cyano$cyano)){
  tmp.sample  = cyano[i, "sample"]
  tmp.date    = cyano[i, "date"]
  tmp.gee <- filter(gee, sample == tmp.sample & 
                      date >= tmp.date - 15 &
                      date <= tmp.date + 15 & 
                      edge == "no")
  tmp.cyano = max(tmp.gee$cyano)
  cyano[i, "cyano_rs"] = tmp.cyano
}
# Compute confusion matrix
cyano <- cyano %>% 
  mutate(cyano = as.factor(cyano),
         cyano_rs = as.factor(cyano_rs))
caret::confusionMatrix(data = cyano$cyano_rs, reference = cyano$cyano)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
# 0 23 26
# 1  0 11
# 
# Accuracy : 0.5667          
# 95% CI : (0.4324, 0.6941)
# No Information Rate : 0.6167          
# P-Value [Acc > NIR] : 0.824           
# 
# Kappa : 0.2449          
# 
# Mcnemar's Test P-Value : 9.443e-07       
#                                           
#             Sensitivity : 1.0000          
#             Specificity : 0.2973          
#          Pos Pred Value : 0.4694          
#          Neg Pred Value : 1.0000          
#              Prevalence : 0.3833          
#          Detection Rate : 0.3833          
#    Detection Prevalence : 0.8167          
#       Balanced Accuracy : 0.6486  