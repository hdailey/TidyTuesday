library(tidyverse)
library(showtext)
library(rnaturalearth)
library(ggtext)

font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

tuesData <- tidytuesdayR::tt_load(2023, week = 16)

crops <- tuesData$founder_crops %>%
  filter(!is.na(edibility),
         source == "ORIGINS") %>%
  group_by(site_name) %>%
  slice_max(prop, n = 1) %>%
  ungroup()

map <- ne_countries(continent = c("africa", "asia"), returnclass = "sf")

plot_03 <- ggplot() +
  geom_sf(data = map, colour = "black", fill = "grey95") +
  geom_point(data = crops,
             mapping = aes(x  = longitude, y = latitude, shape = category,
                           colour = category)) +
  labs(x = "",
       y = "",
       title = "Neolithic Founder Crops",
       subtitle = "Eight **founder crops** — emmer wheat, einkorn wheat, barley, lentil, pea, chickpea, bitter vetch, and flax — have long been thought to have been the bedrock of Neolithic economies. <br>
       The map below shows the site locations noted within the Origins of Agriculture database.",
       caption = "Source: Orgins of Agriculture | #TidyTuesday | Week 16") +
  theme_void() +
  theme(text = element_text(size = 48, family = "Cabin Condensed"),
        plot.background = element_rect(fill ="grey95", color = NA),
        panel.background = element_rect(fill = "grey95", color = NA),
        plot.title = element_text(size = 64, family = "Cabin", face = "bold",
                                  margin = margin(t = 10, b = 10, l = 10)),
        plot.subtitle = element_textbox_simple(lineheight = 0.5, hjust = 0, margin = margin(b = 10)),
        plot.caption = element_text(size = 28),
        legend.position = "top",
        legend.title = element_blank())
  
ggsave(plot = plot_03, path = here::here("2023/2023-04-18_NeolithicFounderCrops/"), "2023-04-19_TT.png", dpi = 320, height = 8, width = 11, unit = "in")
