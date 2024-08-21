# Tidy Tuesday - August 20 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 34)

# Data Exploration ####
monarchsMarriages <- tuesdata$english_monarchs_marriages_df

monarchsMarriagesFiltered <- monarchsMarriages %>%
  mutate(year_of_marriage = as.numeric(year_of_marriage),
         king_age = as.numeric(king_age),
         consort_age = as.numeric(consort_age)) %>%
  na.omit() %>%
  mutate(difference_in_age = king_age - consort_age) %>%
  mutate(label = case_when(difference_in_age < -5 ~ glue::glue("King {king_name} ({king_age})\n",
                                                              "marries\n",
                                                              "{consort_name} ({consort_age})"))) %>%
  mutate(magnitude_age_difference = case_when(abs(difference_in_age) > 20 ~ "Greater Than 20 Years",
                                              abs(difference_in_age) <= 20 & abs(difference_in_age) > 10 ~ "10-20 Years",
                                              abs(difference_in_age) <= 10 & abs(difference_in_age) > 5 ~ "5-10 Years",
                                              .default = "Less Than 5 Years")) %>%
  select(c(year_of_marriage, king_age, consort_age, difference_in_age, magnitude_age_difference)) %>%
  pivot_longer(cols = c(king_age, consort_age), values_to = "age")

# Data Visualization ####
plotFinal <- monarchsMarriagesFiltered %>%
  ggplot() +
  geom_line(aes(x = year_of_marriage, y = age, group = year_of_marriage, colour = magnitude_age_difference), linewidth = 0.5) +
  geom_point(aes(x = year_of_marriage, y = age, fill = magnitude_age_difference), size = 1, pch = 21, stroke = 0.5) +
  scale_colour_discrete(type = c("#F21A00", "#E1AF00", "#78B7C5", "#3B9AB2")) +
  scale_fill_discrete(type = c("#F21A00", "#E1AF00", "#78B7C5", "#3B9AB2")) +
  

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-08-20_EnglishMonarchs/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")

