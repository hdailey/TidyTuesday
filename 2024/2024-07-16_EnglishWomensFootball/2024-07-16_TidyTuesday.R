# Tidy Tuesday - July 16, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggbump)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 29)
ewfAppearances <- tuesdata$ewf_appearances
ewfMatches <- tuesdata$ewf_matches
ewfStandings <- tuesdata$ewf_standings

# Data Exploration ####
ewfTop5 <- ewfAppearances %>%
  filter(tier == 1) %>%
  mutate(season = str_remove_all(season, "\\-.*"),
         season = as.numeric(season)) %>%
  filter(season >= 2017) %>%
  mutate(team_name = str_remove(team_name, "Ladies| Belles| Women| Lionesses")) %>%
  group_by(team_name) %>%
  summarise(goalsFor = sum(goals_for),
            goalsAgainst = sum(goals_against)) %>%
  arrange(desc(goalsFor)) %>%
  slice_head(n = 5)

ewfStandings_filtered <- ewfStandings %>%
  filter(tier == 1) %>%
  mutate(team_name = str_remove(team_name, "Ladies| Belles| Women| Lionesses")) %>%
  filter(team_name %in% c("Manchester City", "Arsenal", "Chelsea", "Manchester United", "Reading")) %>%
  mutate(season = str_remove_all(season, "\\-.*"),
         season = as.numeric(season)) %>%
  filter(season >= 2017) %>%
  mutate(teamColor = case_when(team_name == "Manchester City" ~ "#6CADBB",
                               team_name == "Arsenal" ~ "#EC0024",
                               team_name == "Chelsea" ~ "#394694",
                               team_name == "Manchester United" ~ "#FBE122",
                               .default = "#004494")) %>%
  mutate(division = str_remove_all(division, "FA | \\(.+\\)"))
# Data Visualization ####
plotFinal <- ewfStandings_filtered %>%
  ggplot() +
  geom_bump(aes(x = season, y = position, colour = teamColor), linewidth = 2, show.legend = FALSE) +
  geom_point(aes(x = season, y = position, fill = colorspace::lighten(teamColor, 0.3), colour = teamColor), 
             pch = 21, size = 8, stroke = 1, show.legend = FALSE) +
  annotate("text", x = ewfStandings_filtered$season, y = ewfStandings_filtered$position, label = ewfStandings_filtered$position,
           colour = "white", fontface = "bold", size = 12) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
                     labels = c(2017, 2018, 2019, 2020, 2021, 2022, 2023),
                     limits = c(2016.5, 2023.5)) +
  scale_y_continuous(breaks = rev(seq(1:12)),
                     trans = "reverse") +
  labs(x = "", 
       y = "",
       title = "Position of the Top 5 Goal Scoring Teams in English Women's Football (2017-2023)",
       subtitle = glue::glue("From 2017 through 2023 the top five goal scoring teams in English Women's Football were ",
                             "<span style='color:#6CADBB'>**Manchester City**,</span> ",
                             "<span style='color:#EC0024'>**Arsenal**,</span> ",
                             "<span style='color:#394694'>**Chelsea**,</span> ",
                             "<span style='color:#FBE122'>**Manchester United**,</span> and ",
                             "<span style='color:#004494'>**Reading**.</span>",
                             "Between these five teams, over 1,500 goals were scored, with only 631 against. ",
                             "Chelsea has been a dominate powerhouse being the top team since 2019 where they rose from 3rd in 2018 to 1st the following year. ",
                             "Unfortunately for Reading FC, with the last place finish in 2022, Reading FC was relegated to Tier 2 in 2023."),
       caption = "Source: The English Women's Football Database | #TidyTuesday | Week 29") +
  theme(text = element_text(family = "Roboto Condensed", colour = "#FFFFFF", size = 28),
        plot.title = element_text(family = "Roboto", face = "bold", size = 30, hjust = 0.5, margin = margin(t = 5)),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, colour = "#FFFFFF", halign = 0.5, margin = margin(t = 2.5, b = 5)),
        axis.text.x = element_text(face = "bold", angle = 45, colour = "#FFFFFF", hjust = 1, size = 32),
        plot.caption = element_text(colour = "#FFFFFF", hjust = 0.5, size = 16, margin = margin(t = -8, b = 2.5)),
        plot.margin = margin(l = -5, r = 5),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#25A519", colour = "#FFFFFF"),
        panel.background = element_rect(fill = "#25A519", colour = "#FFFFFF"),
        panel.grid.major.x = element_line(linewidth = 0.5, linetype = "longdash"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "#FFFFFF", linewidth = 2),
        axis.ticks = element_blank())
# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-07-16_EnglishWomensFootball/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)
