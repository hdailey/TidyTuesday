# Tidy Tuesday - August 6 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(rnaturalearth)
library(sf)
library(cowplot)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 32)
olympics <- tuesdata$olympics

# Data Exploration ####
olympicGrouped <- olympics %>%
  group_by(event) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

olympicsRugby <- olympics %>%
  mutate(isRugby = str_detect(event, "Rugby")) %>%
  filter(isRugby == TRUE) %>%
  mutate(team = case_when(team == "Australasia" ~ "Australia",
                          team == "Union des Socits Franais de Sports Athletiques" ~ "France",
                          team == "United States" ~ "United States of America",
                          .default = team)) %>%
  filter(team %in% c("Australia", "Fiji", "France", "United States of America", "South Africa")) %>%
  select(c(team, noc, year, season, medal)) %>%
  na.omit(medal) %>%
  group_by(team, medal) %>%
  count(medal) %>%
  mutate(medal = factor(medal, levels = c("Bronze", "Silver", "Gold"))) %>%
  mutate(medalColor = case_when(medal == "Gold" ~ "#D6C372",
                                medal == "Silver" ~ "#C6C7BE",
                                .default = "#987559"))

## Data for Basemap
countries <- unique(olympicsRugby$team)
  
worldBasemap <- ne_countries(scale = 50) %>%
  left_join(olympicsRugby, by = join_by(admin == team))

centroids <- bind_cols(worldBasemap, st_coordinates(st_centroid(worldBasemap, of_largest_polygon = TRUE))) %>%
  st_drop_geometry() %>%
  select(admin, gu_a3, pop_est, gdp_md, lon = X, lat = Y) %>%
  pivot_longer(pop_est:gdp_md)

maxy <- max(worldBasemap$n)

offset <- 10

# Data Visualization ####
basePlot <- worldBasemap %>%
  ggplot() +
  geom_sf(fill = "grey95", show.legend = FALSE) +
  scale_fill_identity() +
  labs(title = "Olympic Rugby Medals (1900-2016)",
       subtitle = glue::glue("This week we explore the Olympic Medals dataset from RGriffin on Kaggle. ",
                             "Since the Summer Olympics are happening now (and the USA Womens Team won bronze), we opted to explore Rugby Sevens olympic medals ",
                             "from various countries since the 1900s. As of 2016, France continued to lead the medal count with ",
                             "17 <span style='color:#D6C372'>**Gold Medals**, </span>",
                             "24 <span style='color:#C6C7BE'>**Silver Medals**, </span> and ",
                             "no <span style='color:#987559'>**Bronze Medals** </span>to date in 2016."),
       caption = "Source: @Rgriffin via Kaggle.com | #TidyTuesday | Week 32 | @hdailey") +
  theme_void() +
  theme(plot.title = element_text(family = "Roboto", face = "bold", size = 32, margin = margin(t = 10, r = 5, b = 10, l = 5)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox(family = "Roboto Condensed", lineheight = 0.3, size = 20, width = 0.85, 
                                                margin = margin(t = -2, r = 5, b = -8, l = 5)),
        plot.caption = element_text(family = "Roboto Condesned", size = 8, colour = "grey55", margin = margin(t = -5, r = 5, b = 5)),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "#FFFFFF", colour = NA),
        panel.background = element_rect(fill= "#FFFFFF", colour = NA))

## Plot each country (Source: @dieghernan via Stack Exchange - https://stackoverflow.com/questions/76422043/bar-charts-over-shapefile-maps)
for (country_i in countries) {
  subData <- centroids %>%
    filter(admin == country_i) %>%
    left_join(olympicsRugby, by = join_by(admin == team))
  
  subPlot <- subData %>%
    ggplot(aes(x = medal, y = n, fill = medalColor)) +
    geom_col(alpha = 0.6, width = 0.7, show.legend = FALSE) +
    geom_text(aes(x = medal, y = n-2, label = n), fontface = "bold", family = "Roboto Condensed") +
    scale_fill_identity() +
    labs(x = country_i,
         y = "") +
    ylim(c(0, maxy)) +
    theme_void(base_family = "Roboto Condensed") +
    theme(axis.title.x = element_text(face = "bold"))
  
  basePlot <- basePlot  +     
    annotation_custom(ggplotGrob(subPlot),
                      # Position of the annotation, based on the centroid info
                      xmin = subData$lon[1] - offset,
                      xmax = subData$lon[1] + offset,
                      ymin = subData$lat[1] - offset,
                      ymax = subData$lat[1] + offset
  )
    
}

plotFinal <- ggdraw() + 
  draw_plot(basePlot, x = 0, y = 0, height = 1) +
  draw_image("olympicrings.png", x = 0.86, y = 0.71, width = 0.1, height = 0.1)

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-08-06_Olympics/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in")

