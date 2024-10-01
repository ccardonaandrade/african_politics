## ---------------------------
## Script name: African Coups and Polity IV
## Author: Carlos Cardona
## Date Created: `r paste(Sys.Date())`
## ---------------------------
## set working directory
setwd("C:/Users/ccard/OneDrive/Documentos/GitHub/african_politics")     # Tim's working directory (PC)
## ---------------------------
## load up the packages we will need:  (uncomment as required)

library(tidyverse)
# This package comes from here. Looks cool
# https://github.com/xmarquez/democracyData
library(democracyData)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)
library(readxl)
library(janitor)
library(fixest)
library(broom)



###################
# Creating the map
###################

# Calling the dataset
load("clean/african_politics.R")
african_politics$iso_a3 <- countrycode(african_politics$cown, origin = 'cown', destination = 'iso3c')



# Calling the world dataset
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% select(iso_a3)


# Joining

spatial_join <- st_as_sf(left_join(african_politics,world))
spatial_join$centroid <- st_centroid(spatial_join$geometry)



for (i in 1965:2005) {
  
  
# Filter for coup1 == spatial_join
coup_points <- spatial_join %>%
  filter(coup1 == 1, year==i) %>%
  st_centroid()  # Ensure we get the centroids


proof <- spatial_join  %>% 
  filter(year==i)

proof  %>% 
  ggplot(aes()) + geom_sf(data =  proof, aes(fill = polity2))  +
  geom_sf(data = coup_points, aes(color = "Coup Happened"), fill="green", size = 3, shape = 21) + # Add green points for coup1 == 1
  labs(subtitle ="") +geom_sf(data = world, fill = "transparent") + theme_bw() +  
  coord_sf(xlim = c(-30, 60), ylim = c(-40, 45), expand = FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        title =element_text(size=14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.background = element_rect(fill = "white"),
        #legend.title=element_blank(),
        legend.key = element_rect( fill = "white"),
        legend.position = c(0.2, 0.3),
        legend.key.size = unit(0.6, 'cm'),
        legend.text=element_text(size=14),
        legend.title = element_text(margin = margin(b = 10))  # Adjust the bottom margin
  )+
  scale_fill_gradient(low = "darkred", high = "yellow", 
                      breaks = c(-10,- 5, 0, 5, 10), 
                      limits = c(-10, 10),
                      name = "Polity Score") + # Specify breaks for the color scale
 scale_color_manual(name = "", values = c("Coup Happened" = "darkgreen")) 

ggsave(paste0("figures/couppolity_",i,".png"), width = 10, height = 10, dpi = 900)
}
  
