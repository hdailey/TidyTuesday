# Tidy Tuesday - July 31, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext) 
library(emojifont)
library(patchwork)

## fonts
font_add_google("Merriweather")
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 30)

# Data Exploration ####
scurvy <- tuesdata$scurvy

scurvyGrouped <- scurvy %>%
  group_by(fit_for_duty_d6) %>%
  summarise(n = n())

wafflePlotData <- expand.grid(x = 1:4, y = 1:3) %>%
  as.data.frame() %>%
  mutate(col = case_when(x == 1 & y == 3 ~ "#49BD17", .default = "#F27B12"))

# Data Visualization ####
plotWaffle <- ggplot() +
  geom_text(data = wafflePlotData,
            mapping = aes(x = x, y = y, colour = col, label = "\uf13d"),
            family = "fontawesome-webfont", size = 48,
            show.legend = FALSE) +
  scale_x_discrete(expand = c(0.5, 0)) +
  scale_y_discrete(expand = c(0.2, 0)) +
  scale_colour_identity() +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = "#092F58", colour = "#092F58"),
        panel.background = element_rect(fill = "#092F58", colour = "#092F58"))

plotText <- ggplot() +
  ggtext::geom_textbox(aes(x = 10, y = 10, label = str_wrap(glue::glue("<span style='color:grey95;font-weight:bold;font-family:Merriweather;font-size:60pt'>James Lind's Scurvy Trial</span>",
                                                           "<br><br>",
                                                           "<span style='font-size:28pt'>In 1757, the root causes of Scurvy were unknown. Charles Lind conducted the first study to attempt to further understand Scurvy and treatment options.
                                                           He conducted a randomised control trial with 12 sailors, where 11 of the 12 were found not fit for duty after six days of treatment.</span>",
                                                           "<br><br>",
                                                           "<span style='font-size:10pt'>Source: medicalData R Package | #TidyTuesday | Week 30 | @hdailey</span>",
                                                           "<br>",
                                                           "<span style='font-size:10pt'>Inspired by @nrennie</span>"
                                                           ))),
                           width = unit(13, "cm"), fill = NA, box.colour = NA, halign = 0.5, colour = "grey95", family = "Merriweather", size = 6) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = "#092F58", colour = "#092F58"),
        panel.background = element_rect(fill = "#092F58", colour = "#092F58"))

plotFinal <- plotText + plotWaffle
plotFinal <- plotFinal + plot_layout(widths = c(1, 2))

# Save ####
## save plot manually
