# Tidy Tuesday - December 5, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(viridis)


## fonts
font_add_google("Nunito", db_cache = FALSE)
font_add_google("Merriweather", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 49)

# Data Exploration ####
globalCountries <- ne_countries(returnclass = "sf")

lifeExpectancy <- tuesdata$life_expectancy
gender <- tuesdata$life_expectancy_female_male

age <- tuesdata$life_expectancy_different_ages %>%
  group_by(Entity) %>%
  filter(Year >= 1990) %>%
  na.omit(Code) %>%
  pivot_wider(names_from = Year, values_from = c(LifeExpectancy0:LifeExpectancy80)) %>%
  summarise(Code, 
            `At Age 0` = LifeExpectancy0_2021 - LifeExpectancy0_1990,
            `At Age 10` = LifeExpectancy10_2021 - LifeExpectancy10_1990,
            `At Age 25` = LifeExpectancy25_2021 - LifeExpectancy25_1990,
            `At Age 45` = LifeExpectancy45_2021 - LifeExpectancy45_1990,
            `At Age 65` = LifeExpectancy65_2021 - LifeExpectancy65_1990,
            `At Age 80` = LifeExpectancy80_2021 - LifeExpectancy80_1990) %>%
  pivot_longer(`At Age 0`:`At Age 80`, names_to = "age", values_to = "Change in Life Expectancy") %>%
  mutate(age = fct_inorder(age)) %>%
  left_join(globalCountries, by = c("Code" = "adm0_a3"))

# Data Visualization ####


plotFinal <- age %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = `Change in Life Expectancy`), alpha = 0.5, linewidth = 0.1, colour = "#000000") +
  scale_fill_binned(type = "viridis", alpha = 0.5) +
  facet_wrap(~age, ncol = 2) +
  labs(title = "Change in Life Expectancy (1990-2021)",
       caption = "Source: OWID.org | #TidyTuesday | Week 49 | @hdailey",
       fill = "Δ in Life Expectancy") +
  cowplot::theme_map() +
  guides(fill = guide_colorsteps(title.position = "top", show.limits = TRUE)) +
  theme(text = element_text(family = "Nunito", size = 32),
        plot.title = element_text(family = "Merriweather", hjust = 0.5, face = "bold", size = 48),
        plot.title.position = "plot",
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(hjust = 0.5, margin = margin(b = -10)),
        legend.text = element_text(margin = margin(t = -10)),
        legend.key.width = unit(2, "lines"),
        legend.key.height = unit(0.7, "lines"),
        strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0.5, colour = "grey65", size = 20),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.background = element_rect(fill = "grey95", colour = "grey95"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-12-05_LifeExpectancy/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
