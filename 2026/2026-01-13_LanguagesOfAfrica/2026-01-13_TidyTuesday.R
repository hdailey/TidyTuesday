# Tidy Tuesday - January 13, 2026 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(sysfonts)
library(showtext)
library(sf)
library(afrilearndata)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2026, week = 02)
africanLanguage <- tuesdata$africa

# Data Exploration ####
## Load Mapping Data
mapData_Countries <- africountries
mapData_Boundary <- africontinent

## Data Cleaning
africanLanguage_TopLang <- africanLanguage %>%
  group_by(country) %>%
  slice_max(native_speakers, n = 1) %>%
  distinct()
  
africanLanguage_numLangs <- africanLanguage %>%
  group_by(country) %>%
  summarise(numLang = n())

africanLanguage_grouped <- africanLanguage_TopLang %>%
  left_join(africanLanguage_numLangs, by = "country")

africanLanguage_final <- mapData_Countries %>%
  left_join(africanLanguage_grouped, by = c("name" = "country")) %>%
  replace(is.na(.), 0)

africanLanguage_finalLabel <- africanLanguage_final %>%
  drop_na() %>%
  slice_max(pop_est, n = 10)

africanLanguage_finalLabel_North <- africanLanguage_finalLabel %>%
  filter(name %in% c("Morocco", "Algeria", "Egypt"))

africanLanguage_finalLabel_East <- africanLanguage_finalLabel %>%
  filter(name %in% c("Sudan", "Tanzania", "Ethiopia", "Kenya", "Uganda"))

africanLanguage_finalLabel_SouthWest <- africanLanguage_finalLabel %>%
  filter(name %in% c("South Africa", "Nigeria"))

## Plot
pal <- wesanderson::wes_palette("Zissou1", n = 20, type = "continuous")

plotFinal <- africanLanguage_final %>%
  ggplot() +
  geom_sf(data = mapData_Countries, fill = NA) +
  geom_sf(mapping = aes(fill = numLang)) +
  geom_sf_text(mapping = aes(label = numLang), size = 5, family = "Asap Condensed", show.legend = FALSE) +
  ggrepel::geom_label_repel(data = africanLanguage_finalLabel_North,
                            mapping = aes(label = glue::glue("{name}\n{language}: {scales::label_number(scale = 1e-6, suffix = 'M')(native_speakers)}"),
                                          geometry = geometry),
                            arrow = arrow(length = unit(0.005, "npc")),
                            segment.size = 0.2,
                            size = 8,
                            lineheight = 0.3,
                            stat = "sf_coordinates",
                            nudge_y = 15,
                            min.segment.length = 0,
                            family = "Asap Condensed") +
  ggrepel::geom_label_repel(data = africanLanguage_finalLabel_East,
                            mapping = aes(label = glue::glue("{name}\n{language}: {scales::label_number(scale = 1e-6, suffix = 'M')(native_speakers)}"),
                                          geometry = geometry),
                            arrow = arrow(length = unit(0.005, "npc")),
                            segment.size = 0.2,
                            size = 8,
                            lineheight = 0.3,
                            stat = "sf_coordinates",
                            nudge_x = 28,
                            min.segment.length = 0,
                            family = "Asap Condensed") +
  ggrepel::geom_label_repel(data = africanLanguage_finalLabel_SouthWest,
                            mapping = aes(label = glue::glue("{name}\n{language}: {scales::label_number(scale = 1e-6, suffix = 'M')(native_speakers)}"),
                                          geometry = geometry),
                            arrow = arrow(length = unit(0.005, "npc")),
                            segment.size = 0.2,
                            size = 8,
                            lineheight = 0.3,
                            stat = "sf_coordinates",
                            nudge_x = -10,
                            nudge_y = -10,
                            min.segment.length = 0,
                            family = "Asap Condensed") +
  coord_sf(xlim = c(-40, 80), ylim = c(-50, 50), expand = FALSE) +
  scale_fill_gradientn(colours = pal, 
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100),
                       limits = c(0, 100),
                       guide = guide_colorbar(position = "bottom", direction = "horizontal", 
                                              draw.llim = FALSE, draw.ulim = FALSE, frame.colour = "black", frame.linewidth = 0.25)) +
  labs(title = "The Languages of Africa",
       subtitle = glue::glue("This week we explore the Languages of Africa dataset from Wikipedia. ",
                             "This visualization shows the number of languages spoken in each country.", 
                             "Cameroon has the greatest diversity of languages with 96 different languages represented in the dataset. ",
                             "The top ten countries with the greatest populations are called out with the most common language and number of native speakers."),
       caption = "Source: Wikipedia.org | #TidyTuesday | Week 2 | @hdailey",
       fill = "Number of Languages Spoken") +
  theme_void() +
  theme(text = element_text(family = "Asap Condensed", size = 18),
        plot.title = element_text(family = "Asap", face = "bold", hjust = 0.5, margin = margin(5, 0, 5, 0), size = 32),
        plot.subtitle = ggtext::element_textbox_simple(hjust = 0.5, linewidth = 25, lineheight = 0.3, size = 24),
        plot.caption = element_text(hjust = 0.5, colour = "grey75", margin = margin(2.5, 0, 2.5, 0)),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        legend.title.position = "top",
        legend.title.align = 0.5,
        legend.margin = margin(-25, 0, 0, 0),
        legend.ticks = element_line(colour = "black", linewidth = 0.25, lineend = "square"))

# Save ###### 
ggsave(plot = plotFinal, filename = paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"),
       height = 5.5, width = 6, units = "in", dpi = 300)