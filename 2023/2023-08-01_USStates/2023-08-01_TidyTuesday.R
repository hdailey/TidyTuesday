# Tidy Tuesday - August 1, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext) 
library(cartogram)

## fonts
font_add_google("Lato")
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 31)
  
# Data Exploration ####
states <- tuesdata$states
statesEtmology <- tuesdata$state_name_etymology

statesJoin <- states %>%
  left_join(statesEtmology, by = "state") %>%
  filter(!state %in% c("AK","HI","PR","GU","AS","MP","VI","DC"))

usMap <- tigris::states(cb = TRUE) %>%
  filter(!STUSPS %in% c("AK","HI","PR","GU","AS","MP","VI","DC")) %>%
  sf::st_transform(crs = "ESRI:102003")                              #NAD 1983 CONUS CRS

statesCombined <- usMap %>% 
  left_join(statesJoin, by = c("NAME" = "state")) %>%
  cartogram_dorling(weight = "n_representatives", m_weight = 0.02) %>%
  select(-c(language:meaning)) %>%
  mutate(isDuplicate = duplicated(STUSPS)) %>%
  filter(isDuplicate == FALSE) %>%
  arrange(desc(n_representatives))

# Data Visualization ####
plotFinal <- statesCombined %>%
  ggplot(aes(fill = population_2020)) +
  geom_sf(size = 10, colour = "#000000") +
  geom_sf_text(aes(label = glue::glue("{STUSPS}"), size = 2, family = "Lato", )) +
  scale_size_area(max_size = 5, guide = "none") +
  scale_fill_gradientn(colours = cetcolor::cet_pal(name = "d1", n = 3),
                       breaks = c(1e7, 2e7, 3e7), 
                       labels = c("10 Million", "20 Million", "30 Million")) +
  scale_colour_identity() +
  coord_sf() +
  theme_void() +
  labs(title = "A look at the US population and House of Representative officials",
       subtitle = glue::glue("This graphic explores the population distrubition and associated representation in the US House of Representatives (HoR) in the continental United States (CONUS).",
                             "The size of each circle corresponds to the number of representatives a state holds in the US HoR and the color corresponds to the size of the population.",
                             "The Top 5 states with the most representatives are ",
                             "{statesCombined$NAME[1]}, {statesCombined$NAME[2]}, {statesCombined$NAME[3]}, {statesCombined$NAME[4]}, and {statesCombined$NAME[5]} with ",
                             "{statesCombined$n_representatives[1]}, {statesCombined$n_representatives[2]}, {statesCombined$n_representatives[3]}, {statesCombined$n_representatives[4]} and {statesCombined$n_representatives[5]} representatives, respectively."),
       caption = "Source: Wikipedia | @TidyTuesday | Week 31 | @hdailey | Inspired by @leeolney3") +
  theme(text = element_text(family = "Lato"),
        plot.margin = margin(1, 1, 1, 1),
        plot.title = ggtext::element_textbox_simple(face = "bold", hjust = 0.5, halign = 0.5, size = 62, lineheight = 0.25, margin = margin(t = 10, b = 10)),
        plot.subtitle = ggtext::element_textbox_simple(hjust = 0.5, halign = 0.5, size = 32, lineheight = 0.3, margin = margin( r = 5, l = 5)),
        plot.caption = element_text(size = 20, hjust = 1),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        legend.box.margin = margin(b = 5),
        legend.title = element_text(face = "bold", size = 24),
        legend.text = element_text(size = 24),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title = "Population (in 2020)", title.position = "top", title.hjust = 0.5,
                               barwidth = unit(7, "cm")))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-08-01_USStates"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.8, dpi = 300,
       height = 8, width = 8, unit = "in")
