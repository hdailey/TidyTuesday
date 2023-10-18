# Tidy Tuesday - October 10, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(tidytext)
library(showtext)
library(osmdata)
library(sf)


## fonts
font_add_google("Special Elite")
font_add_google("Nosifer")
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 41)

# Data Exploration ###
hauntedPlaces <- tuesdata$haunted_places %>%
  filter(state == "California",
         city == "San Francisco",
         longitude != is.na(longitude)) %>%
  unnest_tokens(categories, location) %>%
  filter(!categories %in% stop_words$word)

hauntedPlaces_Categories <- hauntedPlaces %>%
  mutate(categories = case_when(categories == "inn" ~ "hotel", .default = categories)) %>%
  group_by(categories) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(categories != "san") %>%
  slice_head(n = 1)

hauntedPlaces_CACount <- tuesdata$haunted_places %>%
  filter(state == "California") %>%
  group_by(city) %>%
  summarise(n = n()) %>%
  arrange(desc(n))


hauntedPlaces <- hauntedPlaces %>%
  filter(categories %in% c(hauntedPlaces_Categories$categories)) %>%
  distinct(description, .keep_all = TRUE) %>%
  mutate(emoji = case_when(categories == "school" ~ "1f47b")) #%>%
  #st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Data Visualization ####
sfMap_bb <- getbb("San Francisco, United States")
sfStreets <- sfMap_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

sfSideStreets <- sfMap_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residental", "living_streets", "unclassified", "service", "footway")) %>%
  osmdata_sf()


plotFinal <- ggplot() +
  geom_sf(data = sfSideStreets$osm_lines, colour = "#890EF2", size = 0.1, alpha = 0.2) +
  geom_sf(data = sfStreets$osm_lines, colour = "#890EF2", size = 0.2, alpha = 0.8) +
  ggfx::with_outer_glow(colour = "#FF5124", sigma = 10,
                        emoGG::geom_emoji(data = hauntedPlaces, aes(x = longitude, y = latitude, emoji = emoji), 
                                          size = 0.03)) +
  annotate("text", x = -122.82, y = 37.8, label = str_wrap("San Francisco's Haunted Schools", width = 30),
           colour = "grey95", family = "Nosifer", size = 10, lineheight = 0.3) +
  annotate("text", x = -122.82, y = 37.75, label = str_wrap(glue::glue("San Francisco is the third most haunted city in California with 27 haunted locations. ", 
                                                  "Of these 27 locations, {length(hauntedPlaces)} are schools which are shown below."), 
                                                  width = 30),
           colour = "grey95", family = "Special Elite", size = 8, lineheight = 0.3) +
  annotate("text", x = -122.82, y = 37.7, label = "Source: The Shadowlands Haunted Places Index | TidyTuesday | Week 41 | @hdailey",
           colour = "grey35", family = "Special Elite", size = 4) +
  labs(title = ) +
  theme_void() +
  theme(text = element_text(colour = "grey95", family = "Special Elite"),
        plot.background = element_rect(fill = colorspace::lighten(col = "#000000", amount = 0.1)),
        plot.title = element_text(vjust = -100),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, margin = margin(t = 20)),
        plot.margin = margin(0, 0, 0, 0))
  
# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-10-10_HauntedPlaces/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 6, width = 8, unit = "in")

