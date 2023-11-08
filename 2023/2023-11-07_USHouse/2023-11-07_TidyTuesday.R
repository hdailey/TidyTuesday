# Tidy Tuesday - November 07, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(geofacet)

## fonts
font_add_google("Roboto Condensed", db_cache = FALSE)
font_add_google("Neuton", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 45)

# Data Exploration ####
otherParties <- unique(houseData$party) %>%
  as.data.frame() %>%
  rename("party" = ".") %>%
  mutate(party = str_to_title(party)) %>%
  filter(party != c("Democrat", "Republican"))

houseData <- tuesdata$house %>%
  mutate(party = str_to_title(party),
         party = case_when(party %in% c(otherParties$party) ~ "Other",
                           .default = party)) %>%
  na.omit(party) %>%
  group_by(year, state, state_po, party) %>%
  summarise(n = n())
# Data Visualization ####

plotUS <- houseData %>%
  ggplot() +
  geom_area(aes(x = year, y = n, fill = party), position = "fill", show.legend = FALSE) +
  geom_text(aes(x = median(range(year)), y = 0.5, label = state_po), family = "Neuton", size = 18,
            colour = "#FFFFFF") +
  scale_fill_manual(values = c("Democrat" = "#0015BC", "Republican" = "#FF0000", "Other" = "grey65")) +
  facet_geo(~state_po) +
  coord_cartesian(expand = FALSE) +
  labs(title = "US House Representation Over the Years by Party",
       subtitle = glue::glue("This graphic explores the proportion of elected US House officials by party. ",
                             "Party data is included for the ", 
                             "<span style='color:#0015BC;'><b>Democratic,</b></span> and ",
                             "<span style='color:#FF0000;'><b>Republican,</b></span> parties, as well as for ",
                             "<span style='color:grey65;'><b>Other Notable,</b></span> parties (i.e., independent)."),
       caption = "Source: MIT Election Data and Science Lab | #TidyTuesday | Week 45 | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_text(family = "Neuton", face = "bold", size = 62),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 42, lineheight = 0.3, 
                                                       width = unit(12, "cm"), hjust = 0,
                                                       margin = margin(t = 2, b = -40)),
        plot.caption = element_text(size = 28, colour = "grey45"),
        plot.background = element_rect(colour = "#FFFFFF", fill = "#FFFFFF"),
        strip.text = element_blank(),
        plot.margin = margin(2, 5, 2, 5))

# Save ####
ggsave(plot = plotUS, path = here::here("2023/2023-11-07_USHouse"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 1, dpi = 300,
       height = 8, width = 11, unit = "in")
