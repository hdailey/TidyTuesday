# Tidy Tuesday - August 8, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggtext)


## fonts
# font_add_google()
# showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 32)

# Data Exploration ####
sauces <- tuesdata$sauces
seasons <- tuesdata$seasons
episodes <- tuesdata$episodes

saucesData <- sauces %>%
  mutate(sauce_name = case_when(sauce_name == "Da' Bomb Beyond Insanity" ~ "Da' Bomb - Beyond Insanity", .default = sauce_name),
         sauce_name = case_when(sauce_name == "Hot Ones -The Classic (Garlic Fresno Edition)" ~ "Hot Ones – The Classic (Garlic Fresno Edition)", .default = sauce_name),
         abbrev = abbreviate(sauce_name),
         abbrev = case_when(abbrev == "HO–TC(ME" ~ "HO–TC(ME)", .default = abbrev),
         abbrev = case_when(abbrev == "HO–TLD(E" ~ "HO–TLD(E)", .default = abbrev),
         abbrev = case_when(abbrev == "HO–TC(FE" ~ "HO–TC(FE)", .default = abbrev))

# Data Visualization ####
plotFinal <- saucesData %>%
  ggplot() +
  geom_textbox(aes(x = sauce_number, y = season, label = str_wrap(sauce_name), fill = scoville), width = grid::unit(0.73, "npc"))
