# Tidy Tuesday - July 11, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 28)

# Data Exploration ####
global_temps <- tuesdata$global_temps %>%
  select(c(Year, DJF:SON)) %>%
  mutate(ID = "global_temps")
nh_temps <- tuesdata$nh_temps %>%
  select(c(Year, DJF:SON)) %>%
  mutate(ID = "nh_temps")
sh_temps <- tuesdata$sh_temps %>%
  select(Year, c(DJF:SON)) %>%
  mutate(ID = "sh_temps")
zoann_temps <- tuesdata$zonann_temps 

surface_temps <- rbind(global_temps, nh_temps, sh_temps) %>%
  mutate(year_range = case_when(Year < 1900 ~ "1800s",
                                Year < 2000 & Year >= 1900 ~ "1900s",
                                Year >= 2000 & Year != 2023 ~ "2000s",
                                Year == 2023 ~ "2023")) %>%
  pivot_longer(cols = c(DJF:SON), names_to = "months", values_to = "monthly_mean") %>%
  na.omit() %>%
  mutate(months = factor(months, levels = c("DJF", "MAM", "JJA", "SON")),
         ID = case_when(ID == "global_temps" ~ "Global",
                        ID == "nh_temps" ~ "Northern Hemisphere",
                        ID == "sh_temps" ~ "Southern Hemisphere"))

# Data Visualization ####
plotFinal <- surface_temps %>%
  ggplot(aes(x = months, y = monthly_mean, group = Year)) +
  geom_line(aes(colour = year_range, alpha = year_range, linewidth = year_range)) +
  annotate(geom = 'segment', y = Inf, yend = Inf, color = '#eeeeee', x = -Inf, xend = Inf, linewidth = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-1, 1.5, 0.5), expand = c(0, 0)) +
  scale_linewidth_manual(values = c(0.25, 0.25, 0.25, 1)) +
  scale_alpha_manual(values = c(0.4, 0.4, 0.5, 1)) +
  scale_colour_manual(values = wesanderson::wes_palette("Darjeeling1", n = 4)) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ID) +
  cowplot::theme_minimal_vgrid(colour = "#eeeeee") +
  labs(title = "Global Surface Temperatures",
       subtitle = glue::glue("This visualization explores quarterly global and hemispheric means from 1880 to May 2023. ",
                             "Land-surface, air and sea-surface water temperature anomalies are combined to estimate global surface temperature changes. ",
                             "The values on the y-axis correspond to deviations from the base period of 1950 through 1980 and the color of each line corresponds to the century ", 
                             "(<span style='color:#FF0000'>**1800s**</span>, ", "<span style='color:#00A08A'>**1900s**</span>, ",
                             "<span style='color:#F2AD00'>**2000s**</span>, ", "or <span style='color:#F98400'>**2023**</span>). ",
                             "The meteorical year runs from December 1 to November 30 of the next year."),
       caption = glue::glue("Source: NASA GISTEMP | #TidyTuesday | Week 28", 
                            "<br>", 
                            "Citation: GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies.")) +
  theme(text = element_text(family = "Roboto Condensed", colour = "#eeeeee", size = 40),
        plot.title = element_text(family = "Roboto", face = "bold", size = 48),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3),
        plot.caption = ggtext::element_textbox_simple(lineheight = 0.3, size = 24),
        plot.caption.position = "plot",
        axis.ticks = element_blank(),
        axis.line.x.bottom = element_line(colour = "#eeeeee"),
        axis.text = element_text(colour = "#eeeeee", size = 28),
        axis.text.x = element_text(hjust = 1, angle = 45),
        axis.title = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(hjust = 0.5, colour = "#eeeeee", face = "bold"),
        legend.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        plot.background = element_rect(fill = "#0f101d", colour = "#0f101d"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-07-11_GlobalSurfaceTemperatures/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 8, width = 11, unit = "in")
