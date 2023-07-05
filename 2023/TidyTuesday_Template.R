# Tidy Tuesday - Month XX, 20XX ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("")

## data
tuesdata <- tidytuesdayR::tt_load(20XX, week = XX)

# Data Exploration ####

# Data Visualization ####

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 11, width = 8, unit = "in")