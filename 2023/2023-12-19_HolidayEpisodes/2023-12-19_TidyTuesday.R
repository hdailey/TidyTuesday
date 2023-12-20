# Tidy Tuesday - December 12, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(emojifont)

## fonts
# font_add_google("Mountains of Christmas", db_cache = FALSE)
# showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 51)

# Data Exploration ####
genres <- tuesdata$holiday_episode_genres
episodes <- tuesdata$holiday_episodes %>%
  filter(parent_primary_title == "The Great British Baking Show") %>%
  mutate(original_title = case_when(original_title == "The Great British Bake Off (2016) Christmas Special 1" ~ "Christmas Special No. 1",
                                  original_title == "The Great British Bake Off (2016) Christmas Special 2" ~ "Christmas Special No. 2",
                                  .default = original_title)) %>%
  arrange(desc(average_rating)) %>%
  select(c(season_number, original_title, average_rating, parent_average_rating, num_votes)) %>%
  mutate(cakePosition_x = case_when(season_number == 10 ~ -0.5,
                                    season_number == 4 ~ 0.5,
                                    season_number == 6 ~ -0.5,
                                    season_number == 9 ~ -1,
                                    season_number == 11 ~ 0.5,
                                    season_number == 5 ~ 1,
                                    average_rating == 7.4 ~ -0.5,
                                    season_number == 8 ~ -1,
                                    season_number == 12 ~ 0,
                                    season_number == 3 ~ 0.5,
                                    .default = 1)) %>%
  mutate(cakePosition_y = case_when(season_number == 10 ~ 0.0085,
                                    season_number == 4 ~ 0.0085,
                                    season_number == 6 ~ -0.0032,
                                    season_number == 9 ~ -0.0032,
                                    season_number == 11 ~ -0.0032,
                                    season_number == 5 ~ -0.0032,
                                    average_rating == 7.4 ~ -0.0145,
                                    season_number == 8 ~ -0.0145,
                                    season_number == 12 ~ -0.0145,
                                    season_number == 3 ~ -0.0145,
                                    .default = -0.0145))
  



pal <- colorRampPalette(colors = c("#FF0000", "#FF7878", "#FFFFFF", "#74D680", "#178B29"))(25)

# Data Visualization ####
plotStand <- ggplot() +
  geom_segment(mapping = aes(x = -0.75, xend = 0.75, y = 0, yend = 0), linewidth = 8, colour = "grey65") +
  geom_segment(mapping = aes(x = -1.25, xend = 1.25, y = -0.010, yend = -0.010), linewidth = 8, colour = "grey65") +
  geom_segment(mapping = aes(x = -1.25, xend = 1.25, y = -0.020, yend = -0.020), linewidth = 8, colour = "grey65") +
  geom_segment(mapping = aes(x = 0, xend = 0, y = 0.01, yend = -0.030), linewidth = 8, colour = "grey65") +
  geom_point(mapping = aes(x = 0, y = 0.01), size = 12, colour = "grey65") +
  geom_segment(mapping = aes(x = -0.3, xend = 0.3, y = -0.030, yend = -0.030), linewidth = 8, colour = "grey65")

plotFinal <- plotStand +
  geom_text(data = episodes %>% filter(average_rating == 7.9), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes, label = emoji("cake")),
            family = "EmojiOne", size = 46) +
  geom_text(data = episodes %>% filter(average_rating == 7.8), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes, label = emoji("cake")),
            family = "EmojiOne", size = 36) +
  geom_text(data = episodes %>% filter(average_rating == 7.7), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes, label = emoji("cake")),
            family = "EmojiOne", size = 36) +
  geom_text(data = episodes %>% filter(average_rating <= 7.4), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes, label = emoji("cake")),
            family = "EmojiOne", size = 28) +
  ggrepel::geom_label_repel(data = episodes, aes(x = cakePosition_x, y = cakePosition_y, label = original_title), nudge_y = 0.0015)
  

plotFinal <- episodes %>%
  ggplot() +
  geom_text(aes(x = factor(season_number), y = average_rating, colour = num_votes, label = emoji("cake")),
               family = "EmojiOne", size = 45) +
  geom_label(aes(x = factor(season_number), y = average_rating, label = str_to_title(primary_title)),
                          min.segment.length = 10, nudge_y = 0.03, force = 5) +
  scale_y_continuous(limits = c(7, 7.95)) +
  scale_colour_gradient(low = "#FF0000", high = "#178B29", guide = "colourbar") +
  


# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-12-19_HolidayEpisodes/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
