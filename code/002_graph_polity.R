## ---------------------------
## Script name: Evolution of Polity IV vs Coups by Country
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


# Loading the data
load("clean/african_politics.R")

average_polity <-  african_politics %>%
  group_by(year) %>%
  summarise(polity2 = mean(polity2, na.rm=TRUE))

african_politics %>%
  filter(country_name == "Botswana", year %in% c(1970:2010)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = polity2))+
  geom_vline(data = african_politics %>% filter(country_name == "Botswana", coup1 == 1, year %in% c(1970:2010)),
             aes(xintercept = year, color = "Coup Happened"),
             linetype = "dashed")+
  ylim(-10, 10) +   
  xlim(1970, 2010) +
  labs(x = "Year", 
       y = NULL,
       title = "Botswana",
       subtitle = "Polity V Score") +
  scale_color_manual(name = "", 
                     values = "darkred", 
                     guide = guide_legend(override.aes = list(linetype = "dashed"))) +  # Customizing legend
  theme_minimal()

african_politics %>%
  filter(country_name == "Botswana", year %in% c(1970:2010)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = polity2)) +
  geom_line(data = average_polity, aes(y = polity2, color = "Average Polity Score"), size = 1, linetype = "solid") +  # Add average line
  geom_vline(data = african_politics %>% filter(country_name == "Botswana", coup1 == 1, year %in% c(1970:2010)),
             aes(xintercept = year, color = "Coup Happened"),
             linetype = "dashed",  key_glyph = "path") +
  ylim(-10, 10) +   
  xlim(1970, 2010) +
  labs(x = "Year", 
       y = NULL,
       title = "Botswana",
       subtitle = "Polity V Score") +
  scale_color_manual(name = "", 
                     values = c("gray", "darkred"),  # Add gray for the average line
                     labels = c("Average Polity V", "Coup Happened"),
                     guide = guide_legend(override.aes = list(linetype = c("solid", "dashed")
                                                              ))) +  # Change shape to show legend properly
  theme_minimal()



ggsave(paste0("figures/couppolity_",i,".png"), width = 10, height = 10, dpi = 900)


bostwana <- african_politics %>%
  filter(country_name=="Egypt") 

# Get the unique list of countries with coups
countries_with_coups <- african_politics %>%
  filter(year<2011, coup1 == 1) %>%
  distinct(country_name) %>%
  pull()

# Get the unique list of countries without coups
countries_without_coups <- african_politics %>%
  filter(!country_name %in% countries_with_coups) %>%
  distinct(country_name) %>%
  pull()

# Function to create plots for each country
plot_country <- function(country) {
  african_politics %>%
    filter(country_name == country, year %in% c(1970:2010)) %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = polity2)) +
    geom_line(data = average_polity, aes(y = polity2, color = "Average Polity Score"), size = 1, linetype = "solid") +  # Add average line
    geom_vline(data = african_politics %>% filter(country_name == country, coup1 == 1, year < 2010),
               aes(xintercept = year, color = "Coup Happened"),
               linetype = "dashed",  key_glyph = "path") +
    ylim(-10, 10) +   
    xlim(1970, 2010) +
    labs(x = "Year", 
         y = NULL,
         title = country,
         subtitle = "Polity V Score") +
    scale_color_manual(name = "", 
                       values = c("gray", "darkred"),  # Add gray for the average line
                       labels = c("Average Polity V", "Coup Happened"),
                       guide = guide_legend(override.aes = list(linetype = c("solid", "dashed")))) +  # Change shape to show legend properly
    theme_minimal()
}

# Loop through each country, create and save the plots
walk(countries_with_coups, function(country) {
  # Print the current country being processed
  print(paste("Processing country:", country))
  plot <- plot_country(country)
  # Save each plot using ggsave with custom dimensions and resolution
  ggsave(paste0("figures/polity_evolution/couppolity_", country, ".png"), plot = plot, width = 8, height = 8, dpi = 900, bg = "white")
})



# I have to modify the function for countries with no coup

# Create a custom grob for the legend, centered vertically
coup_line <- linesGrob(x = c(0, 1), y = c(0.5, 0.5), 
                       gp = gpar(col = "darkred", lwd = 1, lty = "dashed"))


# Function to create plots for each country
plot_country <- function(country) {
  african_politics %>%
    filter(country_name == country, year %in% c(1970:2010)) %>%
    ggplot(aes(x = year)) +
    geom_line(aes(y = polity2)) +
    geom_line(data = average_polity, aes(y = polity2, color = "Average Polity Score"), size = 1, linetype = "solid") +
    geom_segment(aes(x = 1970, xend = 1970.1, y = 0, yend = 0, color = "Coup Happened"),
                 linetype = "dashed", show.legend = TRUE) +  # Dummy segment to force legend entry
    ylim(-10, 10) +   
    xlim(1970, 2010) +
    labs(x = "Year", 
         y = NULL,
         title = country,
         subtitle = "Polity V Score") +
    scale_color_manual(name = "", 
                       values = c("Average Polity Score" = "gray", "Coup Happened" = "darkred"),
                       breaks = c("Average Polity Score", "Coup Happened"),
                       labels = c("Average Polity V", "Coup Happened")) +
    theme_minimal() +
    guides(color = guide_legend(
      override.aes = list(
        linetype = c("solid", "dashed"),
        shape = c(NA, NA)
      )
    ))
}


# Loop through each country, create and save the plots
walk(countries_without_coups, function(country) {
  # Print the current country being processed
  print(paste("Processing country:", country))
  plot <- plot_country(country)
  # Save each plot using ggsave with custom dimensions and resolution
  ggsave(paste0("figures/polity_evolution/couppolity_", country, ".png"), plot = plot, width = 8, height = 8, dpi = 900, bg = "white")
})
