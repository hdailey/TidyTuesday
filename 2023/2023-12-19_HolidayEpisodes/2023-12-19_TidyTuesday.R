# Tidy Tuesday - December 12, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(emojifont)
library(cowplot)

## fonts
font_add_google("Nunito")
showtext_auto()

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
  mutate(cakePosition_y = case_when(season_number == 10 ~ 0.022,
                                    season_number == 4 ~ 0.022,
                                    season_number == 6 ~ 0.007,
                                    season_number == 9 ~ 0.007,
                                    season_number == 11 ~ 0.007,
                                    season_number == 5 ~ 0.007,
                                    average_rating == 7.4 ~ -0.006,
                                    season_number == 8 ~ -0.006,
                                    season_number == 12 ~ -0.006,
                                    season_number == 3 ~ -0.006,
                                    .default = -0.006)) %>%
  mutate(num_votes_category = case_when(num_votes <= 24 ~ "Less Than 24 Votes",
                                        num_votes > 24 & num_votes <= 30 ~ "25-30 Votes",
                                        num_votes > 30 ~ "More Than 30 Votes"),
         num_votes_category = factor(num_votes_category))
  
episodeLegend <- data.frame(x = c(-1, 0, 1),
                            y = rep(-0.04, 3),
                            label = c("Less Than 24 Votes", "25-30 Votes", "More Than 30 Votes")) %>%
  mutate(label = fct_inorder(label),
         colourCode = case_when(label == "More Than 30 Votes" ~ '"#178B29"',
                                label == "25-30 Votes" ~ '"#CCAD07"',
                                label == "Less Than 24 Votes" ~ '"#FF0000"'))

# Data Visualization ####
plotStand <- ggplot() +
  geom_segment(mapping = aes(x = -0.75, xend = 0.75, y = 0, yend = 0), linewidth = 8, colour = "grey75") +
  geom_segment(mapping = aes(x = -1.25, xend = 1.25, y = -0.010, yend = -0.010), linewidth = 8, colour = "grey75") +
  geom_segment(mapping = aes(x = -1.25, xend = 1.25, y = -0.020, yend = -0.020), linewidth = 8, colour = "grey75") +
  geom_segment(mapping = aes(x = 0, xend = 0, y = 0.01, yend = -0.030), linewidth = 8, colour = "grey75") +
  geom_point(mapping = aes(x = 0, y = 0.01), size = 12, colour = "grey75") +
  geom_segment(mapping = aes(x = -0.3, xend = 0.3, y = -0.030, yend = -0.030), linewidth = 8, colour = "grey75")

plotCake <- plotStand +
  geom_text(data = episodes %>% filter(average_rating == 7.9), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes_category, label = emoji("cake")),
            family = "EmojiOne", size = 84) +
  geom_text(data = episodes %>% filter(average_rating == 7.8), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes_category, label = emoji("cake")),
            family = "EmojiOne", size = 64) +
  geom_text(data = episodes %>% filter(average_rating == 7.7), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes_category, label = emoji("cake")),
            family = "EmojiOne", size = 64) +
  geom_text(data = episodes %>% filter(average_rating <= 7.4), aes(x = cakePosition_x, y = cakePosition_y, colour = num_votes_category, label = emoji("cake")),
            family = "EmojiOne", size = 52) +
  scale_colour_manual(values = c("More Than 30 Votes" = "#178B29",
                                 "25-30 Votes" = "#CCAD07",
                                 "Less Than 24 Votes" = "#FF0000")) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))
  

plotFinal <- plotCake +
  geom_text(data = episodeLegend, mapping = aes(x = x, y = y, colour = label, label = emoji("cake")),
            family = "EmojiOne", size = 52, lineheight = 0.05, fontface = "bold") +
  geom_text(data = episodeLegend, mapping = aes(x = x, y = y - 0.002, colour = label, label = label),
            size = 24, hjust = 0.5, family = "Nunito") +
  scale_colour_manual(values = c("More Than 30 Votes" = "#178B29",
                                 "25-30 Votes" = "#CCAD07",
                                 "Less Than 24 Votes" = "#FF0000")) +
  labs(title = "Great British Bakeoff Christmas Episodes",
       subtitle = "Of the 11 Christmas themed Bakeoff episodes, two episodes have the highest average rating of 7.9 and five episodes having an average rating of less than 7.4.",
       caption = "Source: IMDb | #TidyTuesday | Week 51 | @hdailey | Inspired by @nrennie") +
  theme(text = element_text(family = "Nunito"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 84),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, halign = 0.5, size = 64, margin = margin(b = -100)),
        plot.caption = element_text(size = 32, hjust = 0.5, colour = "grey35", margin = margin(t = 0, b = 2)))
  


# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-12-19_HolidayEpisodes/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")

