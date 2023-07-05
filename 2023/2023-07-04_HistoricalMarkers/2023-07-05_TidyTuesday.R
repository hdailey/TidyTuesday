# Tidy Tuesday - July 4, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(tigris)
library(ggrepel)

## fonts
font_add_google("Nanum Gothic")

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 27)

# Data Exploration ####
no_markers <- tuesdata$no_markers
historical_markers <- tuesdata$historical_markers %>%
  filter(state_or_prov == "California",
         str_detect(title, "Post Office"))

historical_markers[1, 3] = "US Post Office (First Federal Building)"
historical_markers[2, 3] = "First Fountain Valley Post Office"
historical_markers[3, 3] = "Former Tehachapi Post Office"
historical_markers[6, 3] = "Walnut Creek Post Office"
historical_markers[8, 3] = "Ojai Post Office Tower"
historical_markers[10, 3] = "First South SF Post Office"

caMap <- map_data("state") %>%
  fortify(region = "region") %>%
  filter(region == "california")

# Data Visualization ####
plotFinal <- ggplot() +
  geom_map(data = caMap, map = usMap, aes(map_id = region, group = group), fill = colorspace::lighten("#DA291C", 0.3),
           colour = colorspace::lighten("#DA291C", 0.1)) +
  geom_point(data = historical_markers, aes(x = longitude_minus_w, y = latitude_minus_s), colour = "white", size = 4) +
  geom_label_repel(data = historical_markers, aes(x = longitude_minus_w, y = latitude_minus_s, label = title),
                   seed = 20230705, force = 70, family = "Nanum Gothic", fontface = "bold", size = 11, 
                   segment.colour = "white", colour = "#004B87", min.segment.length = 1) +
  xlim(min(caMap$long), max(caMap$long)) +
  ylim(min(caMap$lat), max(caMap$lat)) +
  labs(x = "",
       y = "",
       title = "Historic Post Office Markers in California",
       subtitle = "Post Offices in California that are designated as historical markers by the Historical Marker Database.",
       caption = "Source: HMDb USA Index | #TidyTuesday | Week 27") +
  theme_void() +
  theme(text = element_text(family = "Nanum Gothic", colour = "white", size = 48),
        plot.title = element_text(face = "bold", size = 72, hjust = 0.5),
        plot.subtitle = ggtext::element_textbox_simple(halign = 0.5, lineheight = 0.3, size = 48),
        plot.caption = element_text(hjust = 0.5, size = 32, margin = margin(b = 0.25, unit = "lines")),
        plot.caption.position = "plot",
        plot.background = element_rect(colour = colorspace::lighten("#004B87", 0.5), fill = colorspace::lighten("#004B87", 0.5)),
        panel.background = element_rect(colour = colorspace::lighten("#004B87", 0.5), fill = colorspace::lighten("#004B87", 0.5)))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-07-04_HistoricalMarkers/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 9, width = 8, unit = "in")

