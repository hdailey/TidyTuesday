# Tidy Tuesday - August 8, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(gt)


## fonts
font_add_google("Passion One", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 32)

# Data Exploration ####
sauces <- tuesdata$sauces
seasons <- tuesdata$seasons
episodes <- tuesdata$episodes

saucesWide <- sauces %>%
  select(-c(sauce_name)) %>%
  #mutate(kiloscoville = scoville/1000) %>%
  #select(-c(scoville)) %>%
  pivot_wider(names_from = sauce_number, values_from = scoville)


# Data Visualization ####
plotFinal <- saucesWide %>%
  gt() %>%
  data_color(palette = "Reds") %>%
  fmt_number(drop_trailing_zeros = TRUE) %>%
  tab_spanner(label = "Sauce Number", columns = 2:11) %>%
  cols_label(season = "Season") %>%
  cols_align(align = "center") %>%
  tab_header(title = md("<span style='color:#FCDC1C'>**Hot Ones Episode Heat Trends**</span>"),
             subtitle = md("Over the course of 21 seasons, hot sauces tested by Hot Ones have trended spicier overall and in frequency.")) %>%
  tab_footnote("Units: Scovilles") %>%
  tab_source_note("Source: Wikipedia and Carl Börstell | #TidyTuesday | Week 32 | @hdailey") %>%
  opt_table_font(font = google_font("Passion One")) %>%
  tab_options(table.background.color = "black",
              )

# Save ####
gtsave(data = plotFinal, filename = paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), path = here::here("2023/2023-08-08_HotOnes"),
       vwidth = 1000, vheight = 1500)
