# Tidy Tuesday - January 23, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggbeeswarm)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 4)

# Data Exploration ####
englishEducation <- tuesdata$english_education %>%
  select(size_flag, income_flag, university_flag, education_score) %>%
  mutate(size_flag = case_when(grepl("london", size_flag, ignore.case = TRUE) ~ "London",
                               grepl("BUA", size_flag, ignore.case = FALSE) ~ "Other",
                               .default = size_flag)) %>%
  mutate(size_flag = factor(size_flag, levels = c("Other", "London", "City", "Large Towns", 
                                                  "Medium Towns", "Small Towns")))

englishEducation_summary <- englishEducation %>%
  group_by(size_flag) %>%
  summarise(mean = mean(education_score))

# Data Visualization ####
plotFinal <- englishEducation %>%
  ggplot(aes(x = size_flag, y = education_score)) +
  geom_beeswarm(colour = "dodgerblue", size = 0.6,
                dodge.width = 0.05, cex = 0.75) +
  annotate("text", y = -5, x = 3.58, label = "Average for group size", size = 8, family = "Roboto Condensed") +
  annotate("segment", x = 3.5, xend = 3.2, y = -4.8, yend = -2.8, arrow = arrow(angle = 45, length = unit(0.1, "cm")), 
           linewidth = 0.2) +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean, geom = "crossbar",
               linewidth = 0.1) +
  labs(x = "",
       y = "Educational Attainment Index Score",
       title = "Educational Attainment Score",
       subtitle = "By Town Size, England",
       caption = "Source: The UK Office for National Statistics | #TidyTuesday | Week 3 | @hdailey | Inspired by Figures @ ons.uk.gov") +
  coord_flip() +
  theme_minimal(base_family = "Roboto Condensed", base_size = 24) +
  theme(plot.title = element_text(family = "Roboto", face = "bold", margin = margin(b = 1), size = 32),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Roboto", size = 28),
        plot.caption = element_text(hjust = 1, colour = "grey55"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey55", linewidth = 0.15),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-01-23_UKEducation/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)

