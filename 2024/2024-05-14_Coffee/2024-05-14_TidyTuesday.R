# Tidy Tuesday - Week 20####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 20)
coffeeSurvey <- tuesdata$coffee_survey

# Data Exploration ####
ageGroupIndex <- c(2, 3, 4, 6, 1, 7, 5)

ageGroups <- c(unique(coffeeSurvey$age)) %>%
  na.omit() %>%
  str_remove(" years old")

ageGroups <- ageGroups[order(ageGroupIndex)]

coffeeSurveyFiltered <- coffeeSurvey %>%
  select(c(age, gender, cups, favorite, roast_level, sweetener, additions)) %>%
  na.omit(gender) %>%
  mutate(roast_level = case_when(roast_level == "Italian" ~ "Dark",
                                 roast_level == "French" ~ "Dark",
                                 roast_level == "Blonde" ~ "Light",
                                 roast_level == "Nordic" ~ "Light",
                                 .default = roast_level)) %>%
  count(gender, age, roast_level) %>%
  filter(gender %in% c("Female", "Male", "Non-binary")) %>%
  mutate(age = fct_relevel(str_remove(age, " years old"), ageGroups),
         col = case_when(roast_level == "Dark" ~ "#3e1e04",
                         roast_level == "Medium" ~ "#965015",
                         .default = "#c4923e")) %>%
  mutate(total_GA = sum(n), .by = c(gender, age),
         perc = round(n/total_GA*100)) %>%
  mutate(roast_label = case_when(roast_level == "Dark" ~ paste0(perc, "%"),
                                 roast_level == "Medium" ~ paste0(perc, "%"),
                                 roast_level == "Light" ~ paste0(perc, "%")))

# Data Visualization ####
plotFinal <- coffeeSurveyFiltered %>%
  ggplot(aes(x = age, y = n, fill = col)) +
  geom_bar(position = "fill", stat = "identity", linewidth = 0.2) +
  geom_text(aes(label = roast_label), position = position_fill(vjust = 0.5), 
            color = "white", alpha = 0.3, family = "Roboto Condensed", stat = "unique", 
            fontface = "bold", size = 6) +
  scale_fill_identity() +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), expand = expansion(add = 0.01)) + 
  facet_wrap(~gender, nrow = 3, ncol = 1) +
  labs(x = "",
       y = "",
       title = "How do coffee roast preferences change with age and gender?",
       subtitle = glue::glue("Coffee roast preferences by age and gender as reported by the Great American Coffee Taste Test. ",
                             "Percentages show the proportion of roast preference for each group. ",
                             "<span style='color:#c4923e'>**Light**,</span> ",
                             "<span style='color:#965015'>**Medium**,</span> and ",
                             "<span style='color:#3e1e04'>**Dark**</span> roasts are shown in their respective colors."),
       caption = "Source: The Great American Coffee Taste Test | TidyTuesday | Week 20 | @hdailey") +
  theme(text = element_text(family = "Roboto Condensed", size = 20),
        plot.title = element_text(family = "Roboto", face = "bold", size = 32, colour = "#6a3005",
                                  margin = margin(b = 1)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(colour = "#6a3005", lineheight = 0.3),
        plot.caption = element_text(size = 12, colour = "#6a3005", margin = margin(2, 5, 0 , 3)),
        plot.caption.position = "plot",
        strip.text = element_text(colour = "#6a3005", face = "bold", size = 21),
        plot.background = element_rect(fill = "#cbac85", colour = "#cbac85"),
        panel.background = element_rect(fill = "#cbac85", colour = "#cbac85"),
        strip.background = element_rect(fill = "#cbac85", colour = "#cbac85"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", colour = "#6a3005", size = 21))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-05-14_Coffee/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")
