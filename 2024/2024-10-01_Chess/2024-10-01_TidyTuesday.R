# Tidy Tuesday - October 1 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 40)

# Data Exploration ####
chess <- tuesdata$chess

topStartingMoves <- chess %>%
  filter(rated == TRUE,
         victory_status == "mate") %>%
  group_by(opening_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(opening_name = sub(".+:(.+)", "\\1", opening_name),
         opening_name = trimws(opening_name))

chessFiltered <- chess %>%
  filter(rated == TRUE,
         victory_status == "mate") %>%
  mutate(opening_name = sub(".+:(.+)", "\\1", opening_name),
                  opening_name = trimws(opening_name)) %>%
  filter(opening_name %in% c(topStartingMoves$opening_name)) %>%
  mutate(opening_name = fct_infreq(opening_name))

# Data Visualization ####
plotFinal <- chessFiltered %>%
  ggplot(aes(x = opening_name, y = turns, colour = opening_name, fill = opening_name)) +
  geom_violin(scale = "count", alpha = 0.5, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set3") +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "",
       y = "Number of Moves",
       title = "Does A Player's Starting Move Determine How Many Moves Will It Will Take To Win?",
       subtitle = glue::glue("This week we look at the Chess Game Dataset (Lichess) from @MitchellJ via Kaggle. ",
                             "Here, we are curious if for all ranked matches that ended in checkmate, ",
                             "if the starting move determines how many moves it will take for victory."),
       caption = "Source: Chess Game Dataset (Lichess) via Kaggle.com | #TidyTuesday | Week 40 | @hdailey") +
  ggthemes::theme_fivethirtyeight() +
  theme(text = element_text(family = "Roboto Condensed", size = 24),
        plot.title = element_text(hjust = 0, face = "bold", family = "Roboto"),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3),
        axis.title.y = element_text(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(colour = "grey55", size = 14, margin = margin(t = 10, b = -10)))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-10-01_Chess/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")
