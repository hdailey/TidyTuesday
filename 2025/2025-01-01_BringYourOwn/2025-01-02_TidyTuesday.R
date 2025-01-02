# Tidy Tuesday - January 2, 2025 ####

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
tuesdata <- read.csv(here::here("2025/2025-01-01_BringYourOwn/S.J.csv"))

# Data Exploration ####
SJS <- tuesdata %>%
  group_by(season, home_or_away) %>%
  reframe(dZoneGiveawaysFor = sum(dZoneGiveawaysFor), goalsAgainst = sum(goalsAgainst), goalsFor = sum(goalsFor)) %>%
  mutate(goalsAgainst = -(goalsAgainst)) %>%
  pivot_longer(cols = c(goalsAgainst, goalsFor), names_to = "goalCategory", values_to = "goals") %>%
  mutate(colour = case_when(goalCategory == "goalsFor" ~ "#EA7200", .default = "#006D75"))

# logo <- magick::image_read(path = here::here("2024/2024-01-02_BringYourOwn/san-jose-sharks-logo.png"))

# Data Visualization ####
primaryAxis <- c(min(SJS$goals) - 25, max(SJS$goals) + 25)
secondaryAxis <- c(min(SJS$dZoneGiveawaysFor) - 25, max(SJS$dZoneGiveawaysFor) + 25)

b <- diff(primaryAxis)/diff(secondaryAxis)
a <- primaryAxis[1] - b*secondaryAxis[1]

scaleFactor <- max(SJS$goals)/max(SJS$dZoneGiveawaysFor)

plotFinal <- SJS %>%
  ggplot() +
  geom_col(aes(x = season, y = goals, fill = colour)) +
  geom_line(data = SJS %>% distinct(), aes(x = season, y = dZoneGiveawaysFor, colour = "Defensive Zone Giveaways"), linewidth = 0.5) +
  geom_text(data = SJS %>% filter(goalCategory == "goalsAgainst") %>% distinct(), 
            aes(x = season, y = goals + 30, label = -1*goals), colour = "white", size = 8) +
  geom_text(data = SJS %>% filter(goalCategory == "goalsFor") %>% distinct(), 
            aes(x = season, y = goals - 30, label = goals), colour = "white", size = 8) +
  geom_text(data = SJS %>% filter(home_or_away == "AWAY") %>% distinct() %>% filter(season == 2018),
            aes(x = season-2, y = dZoneGiveawaysFor), label = glue::glue("2018:\n486 Giveaways"), lineheight = 0.25, size = 8) +
  geom_text(data = SJS %>% filter(home_or_away == "HOME") %>% distinct() %>% filter(season == 2016),
            aes(x = season-2, y = dZoneGiveawaysFor-225), label = glue::glue("2016:\n634 Giveaways"), lineheight = 0.25, size = 8) +
  geom_segment(data = SJS %>% filter(home_or_away == "AWAY") %>% distinct() %>% filter(season == 2018),
               aes(x = 2016.5, y = 486, xend = 2017.75, yend = 486), linewidth = 0.5,
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(data = SJS %>% filter(home_or_away == "HOME") %>% distinct() %>% filter(season == 2016),
               aes(x = 2014.5, y = 634-225, xend = 2015.75, yend = 634-20), linewidth = 0.5,
               arrow = arrow(length = unit(0.5, "cm"))) +
  scale_y_continuous(name = "Goals", sec.axis = sec_axis(~ .*1, name = "Total Defensive Zone Giveaways")) +
  scale_x_continuous(name = "Season") +
  scale_colour_manual(values = c("black")) + 
  scale_fill_identity(labels = c("Goals For", "Goals Against"), guide = "legend") +
  labs(title = "San Jose Sharks Goals and Defensive Zone Giveaways",
       subtitle = "2008-2020",
       caption = "Source: MoneyPuck.com | #TidyTuesday | Week 1 | @hdailey") +
  coord_flip() +
  facet_wrap(~home_or_away) +
  ggthemes::theme_fivethirtyeight() +
  theme(aspect.ratio = 1,
        text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 48),
        plot.title.position = "plot",
        plot.subtitle = element_text(hjust = 0.5, size = 36),
        plot.caption = element_text(hjust = 1, size = 12, margin = margin(t = -10, r = -10, b = -10)),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold", size = 32),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title = element_text(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(t = -5))

# # Save ####
ggsave(plot = plotFinal, path = here::here("2025/2025-01-01_BringYourOwn/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300)

