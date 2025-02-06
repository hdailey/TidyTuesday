# Tidy Tuesday - January 28, 2025 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(urbnmapr)
library(ggpmisc)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 4)
insecurity2022 <- tuesdata$water_insecurity_2022
insecurity2023 <- tuesdata$water_insecurity_2023

# Data Exploration ####
counties <- get_urbn_map("territories_counties", sf = TRUE)
states <- get_urbn_map(sf = TRUE)

insecurity <- insecurity2022 %>%
  rbind(insecurity2023) %>%
  pivot_wider(id_cols = c("geoid":"name"), names_from = "year", values_from = "percent_lacking_plumbing") %>%
  mutate(percDiff = `2023`-`2022`) %>%
  mutate(incDec = case_when(percDiff < 0 ~ "Decrease Lacking Plumbing",
                            percDiff > 0 ~ "Increase Lacking Plumbing",
                            .default = "Unknown or No Change")) %>%
  filter(`2023` != 0,
         `2022` != 0) %>%
  # Split name into County Name and State Name via @NelsonGon on StackExchange - https://stackoverflow.com/questions/55886633/split-a-data-frame-column-based-on-a-comma
  mutate(state_name = unlist(lapply(strsplit(name, ", "), function(x) x[2])),
         county_name = gsub(",.*", "", name)) %>%
  inner_join(counties, by = c("geoid" = "county_fips"))

top5 <- insecurity %>%
  arrange(percDiff) %>%
  slice_head(n = 5) %>%
  select(c("geoid", "name", "2022", "2023", "percDiff")) %>%
  mutate(`2022` = scales::percent(`2022`, accuracy = 0.1),
         `2023` = scales::percent(`2023`),
         percDiff = -percDiff,
         percDiff = scales::percent(percDiff)) %>%
  mutate(name = stringr::str_to_title(name)) %>%
  rename("Difference" = "percDiff",
         "County Name" = "name")

# Data Visualization ####
plotMap <- insecurity %>%
  ggplot() +
  geom_sf(data = counties,
          mapping = aes(),
          fill = "#FFFFFF", colour = "#000000") +
  geom_sf(mapping = aes(geometry = geometry, fill = percDiff), colour = "black", show.legend = FALSE) +
  geom_sf(data = insecurity %>% filter(geoid %in% c(top5$geoid)), 
          mapping = aes(geometry = geometry), fill = "#84fb4e", colour = "black", size = 3) +
  rcartocolor::scale_fill_carto_c(palette = "Geyser", direction = -1) +
  labs(title = "US Water Insecurity (2022-2023)",
       subtitle = glue::glue("This week we are examining water insecurity in the US via Niha Pereira's dataset. ",
                             "Examining 2023 data as compared to 2022, we find 302 counties reported an ",
                             "<span style='color:#008080'>**increase in water accessibility**</span> ", 
                             "whereas 286 reported a <span style='color:#ca562c'>**decrease in water accessibility**</span>. ",
                             "260 counties did not have reportable data, or there was no change in water accessibility.\n", 
                             "\n",
                             "Summarized in the table below are the <span style='color:#84fb4e'>**Top 5 Counties**</span> with the greatest ",
                             "increase in water accessibility."),
       caption = "Source: Niha Pereira | #TidyTuesday | Week 4 | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Asap Condensed", size = 28),
        plot.title = element_text(family = "Asap", margin = margin(10, 0, 2, 2), size = 32, face = "bold"),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, margin = margin(5, 2, 0, 2)),
        plot.caption = element_text(hjust = 0.5, colour = "grey35", margin = margin(-5, 0, 2, 0), size = 12),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))


plotFinal <- plotMap + annotate(geom = "text", x = -1450000, y = -2450000, label = "Top 5 Counties", 
                                family = "Asap Condensed", fontface = "bold", size = 8) + 
  annotate(geom = "table", x = -2500000, y = -3400000, label = list(top5 %>% select(-c(geoid))), 
           family = "Asap Condensed", size = 6)

# # Save ####
ggsave(plot = plotFinal, path = here::here("2025/2025-01-28_WaterInsecurity/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300)

