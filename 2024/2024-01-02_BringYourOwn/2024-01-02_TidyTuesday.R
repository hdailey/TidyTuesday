# Tidy Tuesday - January 3, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggbump)
library(cowplot)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2020, week = 10)

# Data Exploration ####
gameGoals <- tuesdata$game_goals
seasonGoals <- tuesdata$season_goals
top250 <- tuesdata$top_250

SJS <- seasonGoals %>%
  filter(team == "SJS") %>%
  group_by(player) %>%
  mutate(n_games = 1:n(),
         total_seasons = max(n_games),
         total_goals = sum(goals),
         cumsum_goals = cumsum(goals),
         #r500 = detect_index(cumsum_goals, ~. > 500),
         r300 = detect_index(cumsum_goals, ~. > 300),
         r100 = detect_index(cumsum_goals, ~. > 100),
         r50 = detect_index(cumsum_goals, ~. > 50)) %>%
  ungroup() %>%
  filter(total_goals >= 1) %>%
  distinct(player, total_goals, total_seasons, r300) %>%
  pivot_longer(cols = total_seasons:r300, names_to = "r", values_to = "r_v") %>%
  mutate(line_colour = case_when(player == "Joe Pavelski" ~ "#006D75",
                                 player == "Patrick Marleau" ~ "#EA7200", 
                                 .default = "grey50")) %>%
  filter(total_goals >= 50)

logo <- magick::image_read(path = here::here("2024/2024-01-02_BringYourOwn/san-jose-sharks-logo.png"))

# Data Visualization ####
plotBump <- SJS %>%
  ggplot() +
  geom_bump(data = subset(SJS, line_colour == "grey50"),
            aes(x = r, y = r_v, group = player, colour = line_colour, linewidth = total_goals)) +
  geom_bump(data = subset(SJS, line_colour != "grey50"),
            aes(x = r, y = r_v, group = player, colour = line_colour, linewidth = total_goals)) +
  geom_text(data = subset(SJS, r == "total_seasons"),
            aes(x = 2.3, y = r_v, label = glue::glue("{player}: ", "{total_goals}"), colour = line_colour, size = total_goals),
            family = "Asap Condensed", fontface = "bold") +
  
  geom_segment(aes(x = Inf, xend = Inf, y = 0, yend = 25), linewidth = 0.1, arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(x = 0.9, y = 20, label = str_wrap("Number of Seasons to Reach 300 and Total Career Goals for the San Jose Sharks",
                                                   width = 40)),
           family = "Asap", size = 6, lineheight = 0.3, fontface = "bold", check_overlap = T) +
  geom_text(aes(x = 0.9, y = 22, label = str_wrap("Source: HockeyReference.com | #TidyTuesday | Week 1 | @hdailey | Inspired by @gkaramanis",
                                                  width = 60)),
           family = "Asap", size = 4, lineheight = 0.4, colour = "grey35", check_overlap = T) +
  scale_colour_identity() +
  scale_size_continuous(range = c(1, 7)) +
  scale_y_reverse(breaks = seq(0, 25), position = "right", "# of Seasons Played") +
  scale_x_discrete(labels = c("# of seasons to 300 goals", "# of seasons to total number of goals"),
                   position = "top") +
  coord_cartesian(clip = "off") +
  labs(x = "") +
  theme_minimal(base_family = "Asap Condensed", base_size = 18) +
  theme(legend.position = "none",
        panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, colour = "grey75"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "#000000"),
        axis.text.x = element_text(size = 20),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

plotFinal <- ggdraw() +
  draw_plot(plotBump) +
  draw_image(logo, scale = 0.3, x = -0.28, y = -0.13) 
  
# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-01-02_BringYourOwn/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), width = 4, height = 4, units = "in", dpi = 300)

