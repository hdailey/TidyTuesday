# Tidy Tuesday - October 31, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(tidytext)

## fonts
font_add_google("Creepster", db_cache = FALSE)
font_add_google("Nosifer", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 44)

# Data Exploration ####
horrorArticles <- tuesdata$horror_articles

horrorArticles_claim <- horrorArticles %>%
  filter(rating %in% c("true", "false", "legend")) %>%
  unnest_tokens(word, claim) %>%
  na.omit(word) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(year = lubridate::year(published),
         month = lubridate::month(published, label = TRUE, abbr = FALSE)) %>%
  count(word, month, year, rating)

horrorArticles_SA <- horrorArticles_claim %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(month, year, sentiment, rating) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# Data Visualization ####
plotFinal <- horrorArticles_SA %>%
  ggplot(aes(x = year, y = sentiment, fill = sentiment > 0)) +
  ggchicklet::geom_chicklet(width = 5, radius = grid::unit(0.5, "mm"), colour = "#FBFAF4", position = position_dodge2(), size = 0) +
  geom_hline(aes(yintercept = 0), linewidth = 0.5, colour = "#FFFFFF") +
  facet_grid(rating ~ month, switch = "y") +
  scale_x_continuous(position = "top") +
  scale_y_continuous() +
  scale_fill_manual(values = c("#FF9A00", "#C900FF")) +
  labs(title = "Snopes Horror Claims Sentiment Analysis",
       subtitle = glue::glue("This week we explore the Snopes horror urban legends with a sentiment analysis. ", 
                             "For horror urban legends that have been identified as true, false or still a legend, this visualization summarizes ",
                             "if the sentiment from the claim is either ", 
                             "<span style='color:#C900FF;'><b>positive</b></span> or ", 
                             "<span style='color:#FF9A00;'><b>negative</b></span>."),
       caption = "Source: Snopes Horror Section | #TidyTuesday | Week 44 | Inspired by @jennaschilling | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Creepster", colour = "#FFFFFF", size = 28),
        plot.title = element_text(family = "Nosifer", size = 48, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(halign = 0.5, margin = margin(b = 10), lineheight = 0.4),
        plot.caption = element_text(size = 16, hjust = 0.5,  margin = margin(t = -50, b = 5)),
        plot.caption.position = "plot",
        strip.text = element_text(),
        plot.background = element_rect(fill = colorspace::lighten("#000000")),
        legend.position = "none",
        plot.margin = margin(2, 5, 2, 5))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-10-31_HorrorArticles/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.75, dpi = 300,
       height = 8, width = 11, unit = "in")

