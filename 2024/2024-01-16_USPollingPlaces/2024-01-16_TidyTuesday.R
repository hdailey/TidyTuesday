# Tidy Tuesday - January 16, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(lubridate)

## fonts
font_add_google("Newsreader", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 3)

# Data Exploration ####
pollingPlaces <- tuesdata$polling_places %>%
  mutate(electionYear = year(election_date)) %>%
  filter(electionYear %in% c(2012, 2020)) %>%
  group_by(electionYear, state) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = "state",
              names_from = "electionYear",
              values_from = "n") %>%
  na.omit(2012) %>%
  rename("election2012" = "2012",
         "election2020" = "2020") %>%
  mutate(diff = election2020 - election2012,
         diff = as.numeric(diff)) %>%
  arrange(desc(diff)) %>%
  mutate(state = fct_inorder(state)) %>%
  mutate(posNeg = ifelse(diff < 0, "neg", "pos"),
         colourFill = case_when(posNeg == "neg" ~ "#BF0A30",
                                .default = "#002868"))

# Data Visualization ####
plotFinal <- pollingPlaces %>%
  ggplot(aes(x = diff, y = state)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "#FFFFFF") +
  geom_col(aes(fill = colourFill), width = 0.6) +
  scale_fill_identity() +
  coord_cartesian(clip = "off") +
  labs(x = "Difference",
       y = "",
       title = "What are the US States that have had the most significant shift in polling locations for the 2020 election as compared to the 2012 election?",
       subtitle = glue::glue("This visualization explores the difference  ",
                             "(<span style='color:#002868'>**Positive**</span> or <span style='color:#BF0A30'>**Negative**</span>) ",
                             "in polling locations for the 2020 and 2012 elections."),
       caption = "Source: Center for Public Integrity | #TidyTuesday | Week 3 | @hdailey") +
  theme_minimal(base_family = "Newsreader", base_size = 26) +
  theme(text = element_text(colour = "#FFFFFF"),
        plot.title = ggtext::element_textbox_simple(size = 42, hjust = 0, lineheight = 0.3, face = "bold", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 28, lineheight = 0.3, margin = margin(b = 5)),
        plot.caption = element_text(size = 16, margin = margin(b = -5, t = 5)),
        axis.text = element_text(colour = "#FFFFFF"),
        axis.text.y = element_text(face = "bold"),
        plot.background = element_rect(fill = colorspace::lighten(col = "#39407E", amount = 0.4)),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.3),
        panel.grid.minor.x = element_blank(),
        axis.line.x.bottom = element_line(colour = "#FFFFFF", linewidth = 0.5))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-01-16_USPollingPlaces/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)

