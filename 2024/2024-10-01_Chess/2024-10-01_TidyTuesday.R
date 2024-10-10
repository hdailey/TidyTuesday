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
  geom_violin(alpha = 0.5, show.legend = FALSE) +
  scale_colour_brewer(palette = "Set3") +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "",
       y = "Number of Moves",
       title = "Does A Player's Starting Move Determine How Many Moves Will It Will Take To Win?")
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save ####
# ggsave(plot = plotFinal, path = here::here("2024/2024-10-01_Chess/"),
#         paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")
