# Tidy Tuesday - January 15, 2025 ####

#libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

# 
## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 2)
conf2023 <- tuesdata$conf2023
conf2024 <- tuesdata$conf2024
 
# Data Exploration ####
title2023 <- unique(conf2023$session_title) %>%
  str_replace_all("[^[:alnum:]]", " ")

conf2023_edited <- conf2023 %>%
  mutate(isUniversity = case_when(str_detect(speaker_affiliation, "university|University|college|College|Universidad") ~ TRUE, 
                                  .default = FALSE)) %>%
  filter(session_type == "regular") %>%
  select(c("speaker_affiliation", "isUniversity")) %>%
  arrange(desc(isUniversity))

## Assign x and y for plotting - via @nrennie
conf2023_edited$x <- rep(1:10, times = 10)[1:nrow(conf2023_edited)]
conf2023_edited$y <- rep(1:10, each = 10)[1:nrow(conf2023_edited)]

# Data Visualization ####
plotFinal <- conf2023_edited %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(aes(fill = isUniversity), colour = "#FFFFFF", pch = 21, size = 5, show.legend = FALSE) +
  annotate("text", x = 9.25, y = 10, label = "Source: posit | #TidyTuesday | Week 2\nGraphic: @hdailey", colour = "grey45", 
           family = "Asap Condensed", size = 8, lineheight = 0.3) +
  labs(title = "posit::conf(2023) | university speakers",
       subtitle = glue::glue("In 2023, RStudio rebranaded to Posit and held the first posit::conf(2023) in Chicago. ",
                             "The 2023 conference highlighted advancements in the open source workspace and featured ",
                             "speakers from many industries including ",
                             "<span style='color:#4AA4DE'>**universities**</span>. ",
                             "The number of regular session talks by speaker with an affiliation to a university is highlighted below.")) +
  scale_fill_manual(values = c("grey10", "#4AA4DE")) +
  theme_void() +
  theme(text = element_text(family = "Asap Condensed", colour = "#FFFFFF"),
        plot.title = element_text(size = 72),
        plot.subtitle = ggtext::element_textbox_simple(size = 48, lineheight = 0.3),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_rect(fill = "grey10", colour = NA))

# Save ####
ggsave(plot = plotFinal, path = here::here("2025/2022-01-14_posit/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300)

