# Tidy Tuesday - January 31, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
# font_add_google("", db_cache = FALSE)
# font_add_google("", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 5)

# Data Exploration ####
predictions <- tuesdata$predictions
groundhogs <- tuesdata$groundhogs

# Data Visualization ####

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-01-31_GroundhogDay/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)

