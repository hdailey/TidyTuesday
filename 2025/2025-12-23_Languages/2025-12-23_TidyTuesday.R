# Tidy Tuesday - December 23, 2025 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(sysfonts)
library(showtext)
library(ggalluvial)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 51)
endangeredStatus <- tuesdata$endangered_status
families <- tuesdata$families
languages <- tuesdata$languages

# Data Exploration ####
country = "US"
languageJoin <- languages %>%
  left_join(endangeredStatus, by = "id") %>%
  left_join(families, by = c("family_id" = "id")) %>%
  filter(is_isolate == FALSE) %>%
  select(c(name, macroarea, latitude, longitude, countries, status_label, status_code, family)) %>%
  drop_na()

familyGrouped <- languageJoin %>%
  drop_na(family) %>%
  group_by(family) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

familyFilter <- familyGrouped$family

languageJoinFiltered <- languageJoin %>%
  filter(str_detect(countries, country)) %>%
  filter(family %in% c(familyFilter)) %>%
  select(c(family, name, status_label, status_code)) %>%
  arrange(desc(status_code)) %>%
  mutate(status_label = str_to_title(status_label))

languageJoinFiltered <- languageJoinFiltered %>%
  mutate(freq = str_count(name)/str_count(name),
         status_label = factor(status_label, levels = unique(languageJoinFiltered$status_label)),
         name = factor(name, levels = name)) %>%
  arrange(desc(status_label))

## Plot
pal <- wesanderson::wes_palette("Zissou1", n = 5, type = "discrete")

plotFinal <- ggplot(data = languageJoinFiltered,
                    mapping
                    = aes(axis1 = family, axis2 = name, axis3 = status_label, y = freq)) +
  geom_alluvium(aes(fill = status_label)) +
  # after stat code from @nrennie - https://github.com/nrennie/tidytuesday/blob/main/2025/2025-12-23/20251223.R
  geom_stratum(mapping = aes(width = case_when(after_stat(x) == 1 ~ 0.4,
                                               after_stat(x) == 2 ~ 0,
                                               after_stat(x) == 3 ~ 0.35),
                             colour = as.character(after_stat(x)),
                             fill = status_label)) +
  geom_text(stat = "stratum",
            mapping = aes(label = after_stat(stratum),
                          x = stage(after_stat(x), after_scale = case_when(x == 1 ~ x - 0.2 + 0.02,
                                                                           x == 2 ~ x - 0.05,
                                                                           x == 3 ~ x - 0.175 + 0.02))),
            family = "Asap Condensed",
            size = 8,
            colour = "#000000",
            hjust = 0) +
  scale_colour_manual(values = c("grey70", "transparent", "#FFFFFF")) +
  scale_fill_manual(na.value = "#FFFFFF",
                    values = c(rev(pal[1:5]), "#FFFFFF", "transparent", "#FFFFFF")) +
  labs(title = "Languages of the World Found in the US",
       subtitle = glue::glue("This week, we explore the Languages of the World dataset from Glottolog, which is an open-access database in linguistics, maintained by the Max Planck Institute. ",
                             "Here, we look at world languages found in the United States and their global status. ",
                             "277 languages are listed as being spoken in the United States, with only 25 resulting from the top 5 most common linguistic families. "),
       caption = "Source: Glottolog 5.2.1 | #TidyTuesday | Week 51 | @hdailey inspired by @nrennie") +
  theme_void() +
  theme(plot.title = element_text(family = "Asap", face = "bold", size = 32, margin = margin(t = 10, r = 0, b = 5, l = 10)),
        plot.subtitle = ggtext::element_textbox_simple(family = "Asap Condensed", size = 18, lineheight = 0.5, margin = margin(t = 0, r = 5, b = -10, l = 10)),
        plot.caption = element_text(family = "Asap Condensed", size = 14, hjust = 0.5, colour = "grey40", margin = margin(t = -10, b = 10)),
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        legend.position = "none")

# Save ###### Save #####TRUE
ggsave(plot = plotFinal, path = here::here("2025/2025-12-23_Languages"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), width = 5, height = 5, 
       units = "in", dpi = 300)