# Tidy Tuesday - January 9, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(cowplot)
library(lubridate)
library(ggstream)

## fonts
# font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Oswald", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 2)

# Data Exploration ####
nhlTeams <- tuesdata$nhl_teams
nhlRosters <- tuesdata$nhl_rosters
nhlBirthdays <- tuesdata$nhl_player_births
canBirthdays <- tuesdata$canada_births_1991_2022

nhlBirthdayposition <- nhlRosters %>%
  select(c(position_type, birth_date, birth_country)) %>%
  mutate(year = year(birth_date)) %>%
  filter(year >= 1970) %>%
  group_by(position_type) %>%
  summarise(n = n())
  

nhlRostersGrouped <- nhlRosters %>%
  select(c(position_type, birth_date, birth_country)) %>%
  mutate(year = year(birth_date)) %>%
  filter(year >= 1970) %>%
  filter(birth_country %in% c("CAN", "USA", "SWE", "FIN", "RUS")) %>%
  mutate(position_type = str_to_title(position_type)) %>%
  group_by(year, position_type, birth_country) %>%
  summarise(n = n()) %>%
  mutate(birth_country = case_when(birth_country == "CAN" ~ "Canada",
                                   birth_country == "USA" ~ "United States of America",
                                   birth_country == "SWE" ~ "Sweden",
                                   birth_country == "FIN" ~ "Finland",
                                   birth_country == "RUS" ~ "Russia"))

logo <- magick::image_read(path = here::here("2024/2024-01-09_NHLBirthdays/NHL_Logo.png"))

# Data Visualization ####
plotNHL <- nhlRostersGrouped %>%
  ggplot() +
  geom_stream(aes(x = year, y = n, fill = birth_country), type = "ridge", show.legend = FALSE) +
  scale_fill_discrete(type = wesanderson::wes_palette("Darjeeling1")) +
  facet_wrap(~position_type, nrow = 3, ncol = 1) +
  labs(x = "",
       y = "Number of Players",
       title = "NHL Birthdays 1970-2005",
       subtitle = glue::glue("This visualization explores NHL player birthdays for players who were born between 1970 and 2005 in ",
                             "<span style='color:#FF0000'>**Canada**</span>, ",
                             "<span style='color:#00A08A'>**Finland**</span>, ",
                             "<span style='color:#F2AD00'>**Russsia**</span>, ",
                             "<span style='color:#F98400'>**Sweden**</span>, or the ",
                             "<span style='color:#5BBCD6'>**United States**.</span> ",
                             "Of 27,543 players in the dataset, 32% are defensemen, 60% are forwards and the remainder are goalies."),
       caption = "Source: Statistics Canada, NHL Team List Endpoint and NHL API | Logo: StickPNG | #TidyTuesday | Week 2 | @hdailey",
       fill = "") +
  scale_x_continuous(expand = c(0,0)) +
  theme_minimal_vgrid(font_family = "Oswald", font_size = 28) +
  theme(plot.title = element_text(hjust = 0, face = "bold", margin = margin(t = 2, b = 10), size = 64),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 36, lineheight = 0.3, width = 0.89, hjust = 0),
        plot.caption = element_text(size = 20, colour = "grey75", margin = margin(t = -20, b = -2)),
        plot.caption.position = "plot",
        axis.title.y = element_text(margin = margin(r = -13)),
        strip.text = element_text(size = 32, face = "bold", hjust = 0, margin = margin(b = -5)),
        panel.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF"),
        plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF"))

plotFinal <- ggdraw() +
  draw_plot(plotNHL) +
  draw_image(logo, scale = 0.16, x = 0.43, y = 0.4)

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-01-09_NHLBirthdays/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)

