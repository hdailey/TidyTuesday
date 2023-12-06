# Tidy Tuesday - November 21, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(lubridate)
library(hrbrthemes)
library(viridis)

## fonts
font_add_google("Barlow Condensed", db_cache = FALSE)
showtext_auto()


## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 47)

# Data Exploration ####
rladies_chapters <- tuesdata$rladies_chapters %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(year, month, location) %>%
  summarise(n = n()) %>%
  mutate(location = case_when(location == "inperson" ~ "In Person",
                              location == "online" ~ "Online",
                              .default = location))

# Data Visualization ####
plotFinal <- rladies_chapters %>%
  ggplot(aes(year, month, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), colour = "grey95", fontface = "bold", size = 10, family = "Barlow Condensed") +
  facet_grid(~location) +
  labs(x = "",
       y = "",
       title = "R-Ladies Chapter Events",
       subtitle = glue::glue("The R-Ladies Global was founded by Gabriela de Queiroz and is a grassroots movement designed to promote gender diversity in the R programming community and a welcoming space.",
                             "Since 2011, there have been 4,268 R-Ladies events with 60% (n = 2533) of events being in person and 40% (n = 1735) hosted online.",
                             "Prior to the COVID-19 pandemic, there were no online hosted events noted in the dataset, with the peak number of in person events hosted in the year prior to the pandemic."),
       caption = "Source: @Fgazzelloni | #TidyTuesday | Week 47 | @hdailey") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_viridis() +
  theme_ft_rc(base_family = "Barlow Condensed", plot_title_family = "Barlow Condensed", strip_text_family = "Barlow Condensed",
              ) +
  theme(plot.title = element_text(colour = "grey95", face = "bold", hjust = 0, size = 56),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(family = "Barlow Condensed", colour = "grey95", lineheight = 0.4, hjust = 0, size = 36),
        plot.caption = element_text(colour = "grey75", size = 20, family = "Barlow Condensed", margin = margin(b = -20)),
        plot.caption.position = "plot",
        axis.text.x = element_text(colour = "grey95", angle = 45, vjust = 1, hjust = 1, size = 32),
        axis.text.y = element_text(colour = "grey95", size = 32),
        strip.text = element_text(colour = "grey95", hjust = 0.5, face = "bold", size = 32),
        panel.spacing = unit(2, "lines"),
        legend.position = "top", 
        legend.direction = "horizontal",
        legend.text = element_text(colour = "grey95", size = 32, margin = margin(t = -8, b = -20)),
        legend.title = element_text(colour = "grey95", size = 32, margin = margin(t = 5, b = -10))) +
  guides(fill = guide_colourbar(title = "Number of Events", title.position = "top",title.hjust = 0.5, barwidth = unit(2, "in")))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-11-21_R-Ladies/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
