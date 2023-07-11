# Tidy Tuesday - July 11 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
# font_add_google("")

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

# Data Exploration ####

# Data Visualization ####

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-07-11_GlobalSurfaceTemperatures"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 11, width = 8, unit = "in")
