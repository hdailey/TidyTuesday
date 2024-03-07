# -*- coding: utf-8 -*-
"""
Tidy Tuesday - March 5, 2024
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
import textwrap
import imageio as im
from matplotlib import *
from plotnine import *

## Fonts
rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

## Data
trashWheel = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv")

# Data Exploration
trashWheelGrouped = trashWheel.groupby(by = ["Name", "Year"], as_index = False)["Weight"].sum()
trashWheelGrouped = trashWheelGrouped.sort_values(by = "Year")

trashWheelSum = trashWheel.groupby(by = "Year", as_index = False)["Weight"].sum().apply(lambda x: x.astype(int))
trashWheelSum["Yval"] = trashWheelSum["Weight"] + 10

# Plot Visualization
pal = ["#899DA4", "#C93312", "#DC863B", "#FAEFD1"]

plotFinal = (ggplot(trashWheelGrouped)
             + geom_col(aes("Year", "Weight", fill = "Name"))
             + geom_text(trashWheelSum, aes("Year", "Yval", label = "Weight"), colour = "#FFFFFF", fontweight = "bold")
             + scale_fill_manual(values = pal)
             + labs(x = "", 
                    y = "Weight of Trash (Tons)",
                    title = "Mr. Trash Wheel",
                    subtitle = textwrap.fill("This week, we're looking at data from Mr. Trash Wheel from the Baltimore Healthy Harbor Initiative. Starting in 2014 with Mr. Trash Wheel, three more trash wheels have joined the family and have collected more than 2,900 tons of trash since inception.", 95),
                    caption = "Source: Mr. Trash Wheel via BHHI | #TidyTuesday | Week 10 | @hdailey")
             + theme_tufte()
             + theme(text = element_text(family = "Calibri", colour = "#FFFFFF", size = 10),
                     plot_title = element_text(hjust = 0.5, face = "bold", size = 16),
                     plot_subtitle = element_text(size = 10, hjust = 0.5),
                     plot_caption = element_text(size = 6),
                     legend_position = "top",
                     legend_direction = "horizontal",
                     legend_text = element_text(size = 8),
                     legend_key_size = 8,
                     legend_title = element_blank(),
                     axis_ticks = element_blank(),
                     panel_grid = element_blank(),
                     panel_background = element_rect(fill = "#000000"),
                     plot_background = element_rect(fill = "#000000"))
             )
plotFinal