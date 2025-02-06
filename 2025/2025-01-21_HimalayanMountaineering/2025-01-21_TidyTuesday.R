# Tidy Tuesday - January 2, 2025 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 3)
exped <- tuesdata$exped_tidy
peaks <- tuesdata$peaks_tidy

# Data Exploration ####
expedTop10 <- exped %>%
  select(c(PEAKID, MDEATHS, HDEATHS, TOTMEMBERS)) %>%
  mutate(DEATHS = MDEATHS + HDEATHS) %>%
  reframe(PEAKID, DEATHS, TOTMEMBERS) %>%
  group_by(PEAKID) %>%
  summarise(totDEATH = sum(DEATHS), totMEM = sum(TOTMEMBERS)) %>%
  mutate(perc = totDEATH/totMEM) %>%
  arrange(desc(perc)) %>%
  slice_head(n = 10)

expedJoin <- exped %>%
  inner_join(peaks, by = "PEAKID") %>%
  select(c(EXPID, PEAKID, HIMAL_FACTOR, SEASON, SUCCESS1:SUCCESS4, MDEATHS, HDEATHS, TOTMEMBERS, COMRTE, 
           HEIGHTM)) %>%
  mutate(SUCCESS = SUCCESS1 | SUCCESS2 | SUCCESS3 | SUCCESS4) %>%
  mutate(DEATHS = MDEATHS + HDEATHS) %>%
  select(-c(SUCCESS1:SUCCESS4, MDEATHS, HDEATHS)) %>%
  group_by(HIMAL_FACTOR) %>%
  reframe(COMRTE, HEIGHTM, totDEATH = sum(DEATHS), totMEM = sum(TOTMEMBERS)) %>%
  distinct() %>%
  mutate(perc = totDEATH/totMEM) %>%
  arrange(desc(perc)) %>%
  mutate(HIMAL_FACTOR = fct_inorder(HIMAL_FACTOR))

top10 <- expedJoin %>%
  distinct(HIMAL_FACTOR) %>%
  slice_head(n = 10) 

top10peaks <- top10$HIMAL_FACTOR

expedJoinFiltered <- expedJoin %>%
  filter(HIMAL_FACTOR %in% c(top10peaks),
         COMRTE == FALSE)

# Data Visualization ####
plotFinal <- expedJoinFiltered %>%
  ggplot(aes(x = HEIGHTM)) +
  geom_density(aes(fill = perc), colour = NA) +
  ggpattern::geom_density_pattern(pattern = "gradient", pattern_orientation = "vertical", pattern_fill = NA, colour = NA) +
  facet_wrap(~HIMAL_FACTOR, nrow = 10, strip.position = "left") +
  rcartocolor::scale_fill_carto_c(palette = "Fall") +
  theme_void() +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(title = "Top 10 Dangerous Peaks in the Himalayas (Non-Commercial)",
       subtitle = glue::glue("This week, we explore the Himalayan Database which is a comprehensive archive documenting mountaineering expeditions ",
                             "in the Nepal Himalaya.",
                             "The Himalayan Database continues the work of Elizabeth Hawley, and details the peaks, expeditions, climbing statuses, and ",
                             "geographic information of the many Himalayan summits.",
                             "This visualization explores average peak height and how this relates to <span style='color:#ca562c'>**more fatalities**</span>, ",
                             "or <span style='color:#3d5941'>**less fatalities**</span>."),
       caption = "Source: Himalayan Database | #TidyTuesday | Week 3 | @hdailey",
       x = "Height (m)") +
  theme(text = element_text(family = "Asap Condensed", colour = "#FFFFFF", size = 18),
        plot.title = element_text(family = "Asap", face = "bold", size = 24),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, margin = margin(t = 2, b = -4)),
        plot.caption = element_text(size = 6),
        panel.background = element_rect(fill = colorspace::lighten("#364e5a", 0.2), colour = NA),
        plot.background = element_rect(fill = colorspace::lighten("#364e5a", 0.2), colour = NA),
        plot.margin = margin(2, 10, 5, 2),
        axis.text.x = element_text(margin = margin(t = 5, b = 2)),
        axis.title.x = element_text(),
        legend.position = "none")

# # Save ####
ggsave(plot = plotFinal, path = here::here("2025/2025-01-21_HimalayanMountaineering/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), width = 3, height = 5, units = "in", dpi = 300)

