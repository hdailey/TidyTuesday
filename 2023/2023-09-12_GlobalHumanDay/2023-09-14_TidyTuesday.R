# Tidy Tuesday - September 14, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(countrycode)
library(treemapify)
library(patchwork)

## fonts
font_add_google("Nunito", db_cache = FALSE)
font_add_google("Merriweather", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 37)

# Data Exploration ####
allCountries <- tuesdata$all_countries
countryRegion <- tuesdata$country_regions
GHD <- tuesdata$global_human_day
economicActivity <- tuesdata$global_economic_activity

countryDataAmericas <- countryRegion %>%
  inner_join(allCountries, by = "country_iso3") %>%
  select(-c(alt_country_name2:other_code2)) %>%
  mutate(continent = countrycode(country_name,
                                 "country.name",
                                 "continent")) %>%
  filter(continent == "Americas",
         country_name == "United States of America") %>%
  select(Category, Subcategory, hoursPerDayCombined) %>%
  mutate(Category = str_to_title(Category),
         Subcategory = str_to_title(Subcategory))

countryDataAmericasGrouped <- countryDataAmericas %>%
  group_by(Category) %>%
  summarise(totalHours = sum(hoursPerDayCombined))

# Data Visualization ####
plotFinal <- countryDataAmericas %>%
  ggplot(aes(area = hoursPerDayCombined, fill = Category,
             subgroup = Category, label = Subcategory)) +
  geom_treemap() +
  geom_treemap_text(place = "center", min.size = 6, reflow = TRUE, family = "Nunito", fontface = "bold", lineheight = 0.3) +
  scale_fill_brewer(palette = "PRGn") +
  labs(title = "The Human Chronome Project in the United States",
       subtitle = glue::glue("Driven by the Anthropocene and that human activities are the dominant drivers of change, ",
                             "the Human Chronome Project is a comprehensive database of all global human activities to ",
                             "understand what's going on in the world. ", 
                             "The Chronome quantifies how populations spend their time on average and how these impact Earth. ", 
                             "This visualization explores the Human Chronome Project in the United States where on average the US population ",
                             "spends approximately 11 hours per day on somatic maintenance, which is defined as caring for the cleanliness, appearance and health of human bodies. ",
                             "Conversely, the US population spent on average approximately 4 minutes on 'Non-Food Provision' which includes providing raw materials ",
                             "to the technosphere."),
       caption = "Source: The Human Chronome Project | #TidyTuesday | Week 37 | @hdailey") +
  theme(text = element_text(family = "Nunito"),
        plot.title = element_text(family = "Merriweather", face = "bold", hjust = 0.5, size = 32),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, hjust = 0.5, size = 18),
        legend.position = "top",
        legend.justification = "center",
        legend.text = element_text(face = "bold", margin = margin(l = -3), size = 10),
        legend.margin = margin(t = 5, b = -5),
        plot.caption = element_text(hjust = 0, size = 11, colour = "grey55"),
        plot.caption.position = "plot") +
  guides(fill = guide_legend(nrow = 1, title = "", keywidth = unit(0.2, "cm"), keyheight = unit(0.2, "cm"), label.hjust = 0))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-09-12_GlobalHumanDay/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")
