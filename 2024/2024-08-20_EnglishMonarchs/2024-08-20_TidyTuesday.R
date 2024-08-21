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
  pivot_longer(cols = c(king_age, consort_age), values_to = "age") %>%
  mutate(name = case_when(name == "king_age" ~ "King",
                          .default = "Consort"))

# Data Visualization ####
plotFinal <- monarchsMarriagesFiltered %>%
  ggplot() +
  geom_line(aes(x = year_of_marriage, y = age, group = year_of_marriage, colour = magnitude_age_difference), show.legend = FALSE) +
  geom_point(aes(x = year_of_marriage, y = age, colour = magnitude_age_difference, pch = name)) +
  scale_colour_discrete(type = c("#F21A00", "#E1AF00", "#78B7C5", "#3B9AB2")) +
  scale_fill_discrete(type = c("#F21A00", "#E1AF00", "#78B7C5", "#3B9AB2")) +
  xlim(c(850, 2010)) +
  labs(x = "Year",
       y = "Age of King or Consort",
       title = "Age at Time of Marriage of Kings and Consorts",
       subtitle = glue::glue("A comparison of ages at the time of marriage for monarchs and their consorts from the Mid-7th century to the Mid-20th century. ",
                             "The color of data corresponds to the difference in age between the monarch and their consort."),
       caption = "Source: List of Monarchs by Marriage via .ianvisits.co.uk | #TidyTuesday | Week 34 | @hdailey",
       colour = "Age Difference Magnitude",
       shape = "King or Consort") +
  theme(text = element_text(family = "Roboto Condensed", size = 28, colour = "#000000"),
        plot.title = element_text(family = "Roboto", size = 32, face = "bold", margin = margin(t = 2, b = 2)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.1, margin = margin(b = 5)),
        plot.caption = element_text(size = 14, colour = "grey75", hjust = 0.5),
        plot.caption.position = "plot",
        axis.ticks.x = element_blank(),
        axis.text = element_text(colour = "#000000"),
        legend.text = element_text(size = 18),
        legend.margin = margin(t = -10, b = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, linewidth = 0),
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5, size = 20, margin = margin(t = 5, b = -1)), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.1),
        plot.background = element_rect(fill = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#000000"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-08-20_EnglishMonarchs/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")
