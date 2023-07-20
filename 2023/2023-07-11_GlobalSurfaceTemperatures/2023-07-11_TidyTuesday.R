# Tidy Tuesday - July 11, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)

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
## Line Plot -- Quarterly
plotFinal_lines <- surface_temps %>%
  filter(ID == "Global") %>%
  ggplot(aes(x = months, y = monthly_mean, group = Year)) +
  geom_line(aes(colour = year_range, alpha = year_range, linewidth = year_range)) +
  annotate(geom = 'segment', y = Inf, yend = Inf, color = '#eeeeee', x = -Inf, xend = Inf, linewidth = 0.5) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-1, 1.5, 0.5), expand = c(0, 0)) +
  scale_linewidth_manual(values = c(0.25, 0.25, 0.25, 1)) +
  scale_alpha_manual(values = c(0.4, 0.4, 0.5, 1)) +
  scale_colour_manual(values = wesanderson::wes_palette("Darjeeling1", n = 4)) +
  coord_cartesian(clip = "off") +
  cowplot::theme_minimal_vgrid(colour = "#eeeeee") +
  theme(text = element_text(family = "Roboto Condensed", colour = "#eeeeee"),
        plot.caption = element_textbox_simple(hjust = 1),
        axis.ticks = element_blank(),
        axis.line.x.bottom = element_line(colour = "#eeeeee"),
        axis.text = element_text(colour = "#eeeeee", size = 18),
        axis.text.x = element_text(hjust = 1, angle = 45),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        plot.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        plot.margin = margin(20, 20, 0, 0, "pt"))

## Heat Map -- Monthly and Annotation
surface_temps_circ <- tuesdata$global_temps %>%
  pivot_longer(Jan:Dec, names_to = "month") %>% 
  mutate(month = fct_inorder(month))

plotFinal_text <- ggplot() +
  geom_textbox(aes(x = 10, y = 10, label = str_wrap(glue::glue("<span style='color:#eeeeee;font-weight:bold;font-family:Roboto;font-size:32pt'>Global Surface Temperatures</span>",
                                                               "<br><br>",
                                                               "This visualization explores monthly and quarterly global surface temperature means from 1880 to May 2023 (as the meteoric year (Dec. 1 - Nov. 30). ",
                                                               "Land-surface, air and sea-surface water temperature anomalies are combined to estimate global surface temperature changes. ",
                                                               "Centuries correspond to their respective color ",
                                                               "(<span style='color:#FF0000'>**1800s**</span>, ", "<span style='color:#00A08A'>**1900s**</span>, ",
                                                               "<span style='color:#F2AD00'>**2000s**</span>, ", "or <span style='color:#F98400'>**2023**</span>). ",
                                                               "The figure below explores the monthly deviation in degrees celsius during the same time period with overall increases in average monthly global surface temperature observed from the late 1880s to present." ))),
               width = unit(10, "cm"), fill = NA, box.colour = NA, halign = 0.5, colour = "#eeeeee", family = "Roboto Condensed", size = 4.85) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        plot.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        plot.margin = margin(0, 0, 0, 0, "pt"))

plotFinal_circ <- surface_temps_circ %>%
  ggplot() +
  geom_tile(aes(month, Year, fill = value, colour = value)) +
  labs(x = "",
       y = "", 
       fill = "°C",
       colour = "°C") +
  scale_y_continuous(labels = NULL) +
  scale_fill_gradientn(colours = cetcolor::cet_pal(name = "d1a", n = 7)) +
  scale_colour_gradientn(colours = cetcolor::cet_pal(name = "d1a", n = 7)) +
  coord_polar(start = -pi/12) +
  theme_void() +
  theme(text = element_text(colour = "#eeeeee", family = "Roboto Condensed", size = 18),
        panel.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        plot.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        axis.text.x = element_text(colour = "#eeeeee"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.background = element_rect(fill = "#0f101d", colour = "#0f101d"),
        legend.margin = margin(t = -10),
        plot.margin = margin(0, 0, 0, 0, "pt")) +
  guides(fill = guide_colourbar(title.vjust = 0.75, barwidth = unit(5, "cm")))

plotFinal <- ((plotFinal_text / plotFinal_circ) | plotFinal_lines) 
plotFinal <- plotFinal + plot_layout(widths = c(3, 11)) + plot_annotation(caption = glue::glue("Source: NASA GISTEMP,  GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies. | #TidyTuesday | Week 28"))
                                                                                              
# Save Plot Manually