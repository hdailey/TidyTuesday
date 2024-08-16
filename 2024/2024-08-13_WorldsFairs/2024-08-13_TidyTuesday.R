# Tidy Tuesday - August 13 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggridges)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 33)
worldsFair <- tuesdata$worlds_fairs


# Data Exploration ####
worldsFairGrouped <- worldsFair %>%
  filter(category == "World Expo") %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

calendarMonths <- data.frame(start_month = factor(month.abb, levels = month.abb))

worldsFairFiltered <- worldsFair %>%
  select(c(start_month, start_year, country, visitors, cost, attending_countries)) %>%
  mutate(start_month = factor(start_month, levels = 1:12, labels = month.abb))

worldsFairFinal <- calendarMonths %>%
  left_join(worldsFairFiltered, by = "start_month")

# Data Visualization ####
plotFinal <- worldsFairFiltered %>%
  ggplot(aes(x = attending_countries, y = start_month, fill = start_month)) +
  geom_density_ridges_gradient(aes(fill = after_stat(x)), 
                               linewidth = 0.1, scale = 2, gradient_lwd = 0.75) +
  ylim(c(calendarMonths$start_month)) +
  labs(x = "Number of Attending Countries",
       y = "Start Month of Exposition", 
       title = "Attending Country Distribution by Starting Month at World's Fair Expositions",
       subtitle = glue::glue("Distribution of attending countries across the World's Fair Expositions held from ",
                             "{min(worldsFairFinal$start_year, na.rm = TRUE)} to ",
                             "{max(worldsFairFinal$start_year, na.rm = TRUE)}"),
       caption = "Source: World's Fair (Wikipedia) | #TidyTuesday | Week 33 | @hdailey | Inspired by @poncest") +
  scale_fill_viridis_c(option = "rocket", begin = 1, end = 0,
                       name = "Number of Attending Countries") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  guides(fill = guide_colourbar(position = "top", direction = "horizontal",
                                theme = theme(legend.title.position = "top", 
                                              legend.key.width = unit(5, "cm"),
                                              legend.key.height = unit(0.5, "cm"), 
                                              legend.title = element_text(hjust = 0.5, face = "bold", size = 26),
                                              legend.ticks = element_blank()))) +
  theme(text = element_text(family = "Roboto Condensed", size = 32, colour = "grey20"),
        plot.background = element_rect(fill = colorspace::lighten("#DEDCD1", 0.3), colour = colorspace::lighten("#DEDCD1", 0.3)),
        plot.title = element_text(family = "Roboto", size = 42, face = "bold", hjust = 0,
                                  margin = margin(t = 5, b = 5, l = 2)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(margin = margin(t = 1, b = 5, l = 2)),
        plot.caption = element_text(size = 12, hjust = 0.5, colour = "grey30"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", colour = "grey30", linewidth = 0.1),
        panel.background = element_rect(fill = colorspace::lighten("#DEDCD1", 0.3), colour = colorspace::lighten("#DEDCD1", 0.3)),
        legend.margin = margin(r = -60, b = -50),
        legend.box.background = element_rect(fill = colorspace::lighten("#DEDCD1", 0.3), colour = colorspace::lighten("#DEDCD1", 0.3)),
        axis.text.x = element_text(margin = margin(-10, 0, 0, 0)),
        axis.title = element_text(size = 26, colour = "grey30"))



# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-08-13_WorldsFairs/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")

