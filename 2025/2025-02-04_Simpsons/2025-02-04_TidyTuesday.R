# Tidy Tuesday - February 4, 2025 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(patchwork)

## fonts
font_add_google("Permanent Marker", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 5)
characters<- tuesdata$simpsons_characters
episodes <- tuesdata$simpsons_episodes
locations <- tuesdata$simpsons_locations
lines <- tuesdata$simpsons_script_lines

# Data Exploration ####
linesSpoken <- lines %>%
  filter(speaking_line == TRUE) %>%
  group_by(character_id) %>%
  reframe(totalWords = sum(word_count)) %>%
  na.omit() %>%
  arrange(desc(totalWords)) %>%
  left_join(characters, by = c("character_id" = "id")) %>%
  slice_head(n = 20) %>%
  arrange(totalWords) %>%
  mutate(name = fct_inorder(name))

linesLocation <- lines %>% 
  filter(speaking_line == TRUE) %>%
  group_by(raw_location_text) %>%
  reframe(totalWords = sum(word_count)) %>%
  na.omit() %>%
  arrange(desc(totalWords)) %>%
  mutate(raw_location_text = stringr::str_to_title(raw_location_text)) %>%
  slice_head(n = 20) %>%
  arrange(totalWords) %>%
  mutate(raw_location_text = fct_inorder(raw_location_text))

seasons <- episodes %>%
  group_by(season) %>%
  reframe(averIMDB = mean(imdb_rating), totalViewers = sum(us_viewers_in_millions)) %>%
  na.omit()

pal <- c("#197ec0ff", "#FD7446ff", "#FD8CC1FF", "#46732eff", "#8a9197ff",
         "#197ec0ff", "#FD7446ff", "#FD8CC1FF", "#46732eff", "#8a9197ff",
         "#197ec0ff", "#FD7446ff", "#FD8CC1FF", "#46732eff", "#8a9197ff",
         "#197ec0ff", "#FD7446ff", "#FD8CC1FF", "#46732eff", "#8a9197ff")

pal2 <- c("#197ec0ff", "#FD7446ff", "#FD8CC1FF", "#46732eff", "#8a9197ff",
         "#197ec0ff", "#FD7446ff")

# Data Visualization ####
plotLinesCharacter <- linesSpoken %>%
  ggplot(aes(x = totalWords, y = name, fill = gender)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#FD8CC1FF", "#197ec0ff")) +
  labs(x = "Total Number of Words",
       y = "") +
  theme(text = element_text(colour = "#370335ff", size = 18),
        axis.text = element_text(colour = "#370335ff"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#370335ff", linetype = "dashed", linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25, colour = "#370335ff"))

plotLinesLocation <- linesLocation %>%
  ggplot(aes(x = totalWords, y = raw_location_text, fill = raw_location_text)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  labs(x = "Total Number of Words",
       y = "") +
  theme(text = element_text(colour = "#370335ff", size = 18),
        axis.text = element_text(colour = "#370335ff"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#370335ff", linetype = "dashed", linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25, colour = "#370335ff"))

plotSeasons <- episodes %>%
  ggplot() +
  geom_point(aes(x = imdb_rating, y = us_viewers_in_millions), size = 0.5) +
  labs(x = "IMDB Rating",
       y = "US Viewers (in Millions)") +
  theme(axis.text = element_text(colour = "#370335ff"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#370335ff", linetype = "dashed", linewidth = 0.25),
        panel.grid.major.y = element_line(colour = "#370335ff", linetype = "dashed", linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25, colour = "#370335ff"))

plotText <- episodes %>%
  ggplot() +
  annotate("text", x = -50, y = 4.5, label = str_wrap("Donuts, Data, and D'oh: Data Exploration into The Simpsons", width = 20),
           family = "Permanent Marker", fontface = "bold", size = 16, lineheight = 0.3, hjust = 0.5, colour = "#197ec0ff") +
  annotate("text", x = -50, y = 2, label = str_wrap(glue::glue("This week we are explore the relationships of characters and locations with the number of words, ",
                                                         "as well as what the relationship between IMDB Rating and viewers may be. "),
                                                    width = 30),
           family = "Permanent Marker", size = 8, lineheight = 0.3, hjust = 0.5, colour = "#197ec0ff") +
  annotate("text", x = -50, y = 0.5, label = "Source: Prashant Banerjee via Kaggle | #TidyTuesday | Week 5 | @hdailey",
           family = "Permanent Marker", size = 4, hjust = 0.5, colour = "#197ec0ff") +
  ylim(c(0,6)) +
  xlim(c(-100, 0)) +
  theme_void() +
  theme(plot.margin = margin(l = -10, r = 50))

plotFinal <- (plotLinesCharacter | plotLinesLocation) /
  (free(plotText) | plotSeasons) & theme(text = element_text(family = "Permanent Marker", colour = "#370335ff", size = 20),
                      plot.background = element_rect(fill = colorspace::lighten("#fed439ff", 0.2), colour = NA),
                      panel.background = element_rect(fill = colorspace::lighten("#fed439ff", 0.2), colour = NA),
                      plot.margin = margin(5, 2, 2, -2))
# Save ####
ggsave(plot = plotFinal, path = here::here("2025/2025-02-04_Simpsons/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300)

