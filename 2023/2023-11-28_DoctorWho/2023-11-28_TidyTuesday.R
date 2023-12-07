# Tidy Tuesday - November 28, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggalt)

## fonts
font_add_google("Bricolage Grotesque", db_cache = FALSE)
showtext_auto()


## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 48)

# Data Exploration ####
episodes <- tuesdata$drwho_episodes
directors <- tuesdata$drwho_directors
writers <- tuesdata$drwho_writers

drWhoData <- episodes %>%
  left_join(writers, by = "story_number") %>%
  left_join(directors, by = "story_number") %>%
  group_by(season_number) %>%
  summarise(min = min(rating),
            max = max(rating),
            mean = mean(rating)) %>%
  na.omit() %>%
  mutate(season_number = factor(season_number, levels = 1:13, labels = glue::glue("Season", " ", "{drWhoData$season_number}")))

# Data Visualization ####
plotFinal <- drWhoData %>%
  ggplot() +
  geom_dumbbell(aes(x = min, xend = max, y = season_number),
                colour_x = "#003B6F", colour_xend = "#003B6F", colour = colorspace::lighten("#a6b8c7", 0.8),
                size = 1, size_x = 3.5, size_xend = 3.5) +
  geom_text(data = drWhoData %>% filter(season_number == "Season 13"),
            aes(x = min, y = 13.25, label = "Min"),
            colour = colorspace::lighten("#a6b8c7", 0.8), size = 8, family = "Bricolage Grotesque") +
  geom_text(data = drWhoData %>% filter(season_number == "Season 13"),
            aes(x = max, y = 13.25, label = "Max"),
            colour = colorspace::lighten("#a6b8c7", 0.8), size = 8, family = "Bricolage Grotesque") +
  theme_minimal() +
  labs(x = "Rating (Out of 100)",
       y = "", 
       title = "Doctor Who Ratings from the Revived Era",
       subtitle = glue::glue("The datardis package developed by Jonathan Kitt contains a compiliation of Doctor Who data. ",
                             "This visualization explores the minimum and maximum ratings for each season since Season 1 in 2005. ", 
                             "Over time, the distribution of ratings has condensed and decreased with the highest average rating occuring during Season 4."),
       caption = "Source: {datardis} package | #TidyTuesday | Week 48 | @hdailey") +
  theme(text = element_text(family = "Bricolage Grotesque", colour = colorspace::lighten("#a6b8c7", 0.8), size = 48),
        plot.title = element_text(size = 64, face = "bold", colour = "#00203c"),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.4, size = 30, colour = "#00203c"),
        plot.caption = element_text(size = 28, margin = margin(b = 2), colour = colorspace::lighten("#00203c", 0.3)),
        plot.caption.position = "plot",
        axis.text = element_text(colour = colorspace::lighten("#a6b8c7", 0.8)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25, linetype = 3),
        plot.background = element_rect(colour = colorspace::lighten("#00060B", 0.5),
                                       fill = colorspace::lighten("#00060B", 0.5)),
        panel.background = element_rect(colour = colorspace::lighten("#00060B", 0.5),
                                        fill = colorspace::lighten("#00060B", 0.5)))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-11-28_DoctorWho/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
