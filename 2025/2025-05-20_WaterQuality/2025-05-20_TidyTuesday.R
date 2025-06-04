# Tidy Tuesday - May 20, 2025 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(sysfonts)
library(showtext)
library(sf)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 20)
wq <- tuesdata$water_quality
weather <- tuesdata$weather

# Data Exploration ####
## Generate site index
siteIndex <- wq %>%
  select(c("swim_site", "latitude", "longitude")) %>%
  distinct()

## Join datasets and drop unused columns
ausWQ <- wq %>%
  left_join(weather, by = "date") %>%
  select(-c("region", "council", "time", "max_temp_C", "min_temp_C", 
            "latitude.y", "longitude.y"))

## Statistical Test - Mann Kendall 
pVal <- 0.05

### Split columns for MK test
ausWQ_Split <- split(ausWQ$enterococci_cfu_100ml, ausWQ$swim_site)

### Perform MK test on each site and identify failed MK tests/errors
eColi_Trend <- lapply(ausWQ_Split, Kendall::MannKendall) 
testFailed_eColi <- sapply(eColi_Trend, inherits, "error")

### Convert to dataframe and join lat/long
eColi_Trend_df <- do.call(rbind.data.frame, eColi_Trend)
eColi_Trend_df <- cbind(swim_site = rownames(eColi_Trend_df), eColi_Trend_df)
rownames(eColi_Trend_df) <- NULL

eColi_Trend_df_Final <- eColi_Trend_df %>%
  left_join(siteIndex, by = "swim_site") %>%
  mutate(pValSig = case_when(as.numeric(sl) < 0.05 ~ "Statistically Significant",
                             .default = "No Significance"),
         posNeg = case_when(tau < 0 ~ "Negative",
                            tau > 0 ~ "Positive",
                            .default = "Zero")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 3577) %>%
  arrange(desc(tau)) %>%
  filter(pValSig == "Statistically Significant")

# Data Visualization ####
## Get Australia Basemap from @gkaramanis
australia <- rnaturalearth::ne_countries(country = "Australia", scale = 10) %>%
  st_transform(crs = 3577)

bbox_wkt <- "POLYGON((150 -34.2, 151.6 -34.2, 151.6 -33.5, 150 -33.5, 150 -34.2))"

bbox_wkt_transform <- st_as_sfc(bbox_wkt, crs = 4326) %>%
  st_transform(bbox_sf, crs = 3577) %>%
  st_as_text()

### Download waterbodies: https://data.dea.ga.gov.au/derivative/dea_waterbodies/3-0-0/shapefile/ga_ls_wb_3_v3.zip
australiaWater <- st_read(here::here("2025/2025-05-20_WaterQuality/")) %>%
  st_transform(crs = 3577)

## Plot
plotFinal <- ggplot() +
  geom_sf(data = australia, colour = NA, fill = "#808080") +
  geom_sf(data = australiaWater, colour = "#74ccf4", fill = "#74ccf4") +
  geom_sf(data = eColi_Trend_df_Final, aes(colour = posNeg), size = 0.25, show.legend = TRUE) +
  ggrepel::geom_label_repel(data = eColi_Trend_df_Final %>% slice_head(n = 5),
                           aes(geometry = geometry, label = glue::glue("{swim_site}","\n", "\u03c4=", "{round(tau, digits = 2)}")), min.segment.length = 0, segment.size = 0.25,
                           stat = "sf_coordinates", seed = 12345, lineheight = 0.3, fill = alpha(c("white"), 0.5)) +
  ggrepel::geom_label_repel(data = eColi_Trend_df_Final %>% slice_tail(n = 5),
                           aes(geometry = geometry, label = glue::glue("{swim_site}","\n", "\u03c4=", "{round(tau, digits = 2)}")), min.segment.length = 0, segment.size = 0.25,
                           stat = "sf_coordinates", seed = 12345, lineheight = 0.3, fill = alpha(c("white"), 0.5)) +
  scale_colour_manual(name = "Trend",
                      labels = c("Negative Trend",
                                 "Positive Trend"),
                      values = c("#fa7035", "#f3e399")) +
  coord_sf(xlim = c(149.9, 151.7), ylim = c(-34.15, -33.5), default_crs = 4326, expand = FALSE) +
  labs(title = "*Enterococci* Concentration Trends at Sydney Beaches (NSW)",
       subtitle = glue::glue("Mann Kendall trend analysis on *Enterococci* bacteria concentrations observed at Sydney area beaches. ",
                             "Most beaches along the eastern coast were found to have moderately statistically significant increasing trends at the 95-percent confidence interval. ",
                             "Inland beaches were found to have *Enterococci* concentrations exhibiting declining trends over the sampling period."),
       caption = "Source: Beachwatch.nsw.gov.au (NSW Government) | #TidyTuesday | Week 20 | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Asap Condensed", size = 18),
        plot.title = ggtext::element_markdown(family = "Asap", face = "bold", size = 32),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3),
        plot.caption = element_text(size = 12),
        plot.background = element_rect(fill = "#FFFFFF", colour = NA),
        panel.background = element_rect(fill = "#74ccf4", colour = NA),
        plot.margin = margin(1, 2, 1, 2),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.width = unit(0.25, "cm")) +
  guides(colour = guide_legend(override.aes = list(size = 2)))

# Save ###### Save #####TRUE
ggsave(plot = plotFinal, path = here::here("2025/2025-05-20_WaterQuality/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), width = 5, height = 2.95, 
       units = "in", dpi = 300)

