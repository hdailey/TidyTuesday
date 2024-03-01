# -*- coding: utf-8 -*-
"""
Tidy Tuesday - February 27, 2024
"""

import pandas as pd
import os
import numpy as np
import textwrap
from plotnine import *
from matplotlib import rc


rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

events = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/events.csv')
births = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')

births = births.dropna()
birthsBaseball = births.loc[births["description"].str.contains("baseball")]
birthsBaseball["person"] = np.select(
        [
        birthsBaseball["person"] == "Steve Mingori",
        birthsBaseball["person"] == "Pepper Martin",
        birthsBaseball["person"] == "Al Rosen",
        birthsBaseball["person"] == "Dickey Pearce",
        ],
        ["Steve Mingori\nKansas City Royals",
        "Pepper Martin\nSt. Louis Cardinals",
        "Al Rosen\nCleveland Indians",
        "Dickey Pearce\nBrooklyn Atlantics"]
        )
birthsBaseball["person"] = birthsBaseball["person"].astype("category")
birthsBaseball["person"] = birthsBaseball["person"].cat.reorder_categories(["Dickey Pearce\nBrooklyn Atlantics", "Pepper Martin\nSt. Louis Cardinals", "Al Rosen\nCleveland Indians", "Steve Mingori\nKansas City Royals"])



# Plot Visualization
plotFinal = (ggplot(birthsBaseball)
              + aes(y = "person")
              + geom_segment(aes(x = "year_birth", xend = "year_death",
                                y = "person", yend = "person"), linetype = "dashed", colour = "#FAFAFA")
              + geom_point(aes(x = "year_birth"), colour = "#FFFFFF", size = 6)
              + geom_point(aes(x = "year_death"), colour = "#CE1141", size = 6)
              + geom_text(aes(x = 1872, y = 1.2), label = "~72 Years", colour = "#FAFAFA")
              + geom_text(aes(x = 1934, y = 2.2), label = "~61 Years", colour = "#FAFAFA")
              + geom_text(aes(x = 1969, y = 3.2), label = "~91 Years", colour = "#FAFAFA")
              + geom_text(aes(x = 1976, y = 4.2), label = "~64 Years", colour = "#FAFAFA")
              + labs(x = "",
                     y = "",
                     title = "Professional Baseball Players Born on February 29",
                     subtitle = textwrap.fill("In honor of MLB Spring Training, this week we're looking at MLB players born on February 29! This visualization shows a players birth year and death year, which teams they played for and their age when they died."),
                     caption = "Source: Wikipedia.org | #TidyTuesday | Week 9 | @hdailey")
              + theme_void(base_family = "Calibri", base_size = 12)
              + theme(text = element_text(colour = "#FFFFFF"),
                      plot_title = element_text(size = 16, hjust = 0, face = "bold"),
                      plot_subtitle = element_text(hjust = 0, size = 11),
                      plot_caption = element_text(hjust = 0.5, size = 6),
                      axis_text_y = element_text(hjust = 1, face = "bold"),
                      axis_text_x = element_text(vjust = 1, face = "bold"),
                      plot_background = element_rect(fill = "#13274F"),
                      panel_grid_major_y = element_blank(),
                      panel_grid_major_x = element_line(linetype = "dotted"),
                      panel_grid_minor_x = element_blank())
              )

plotFinal
