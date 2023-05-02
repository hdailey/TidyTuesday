# Libraries
library(tidyverse)
library(showtext)

# Fonts
font_add_google("")
showtext_auto()

# Read in Data
tuesdata <- tidytuesdayR::tt_load(2023, week = 18)
