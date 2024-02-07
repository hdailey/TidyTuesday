# -*- coding: utf-8 -*-
"""
Tidy Tuesday - January 31, 2024
"""

import pandas as pd
import os
import numpy as np
import matplotlib as plt
from plotnine import *
import googlefonts_installer

groundhogs = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/groundhogs.csv")
predictions = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-30/predictions.csv")

groundhogPredictions = pd.merge(groundhogs, predictions, on = "id")
groundhogPredictions = groundhogPredictions[(groundhogPredictions.year == 2023)].dropna(subset = ["shadow"])
groundhogPredictions["shadow"] = np.select([groundhogPredictions["shadow"] == True,
                                           groundhogPredictions["shadow"] == False,
                                           ],
                                           ["Winter", "Spring"],
                                           default = "Unknown")
groundhogPredictions["fillValue"] = np.select([groundhogPredictions["shadow"] == "Winter",
                                             groundhogPredictions["shadow"] == "Spring",
                                             ],
                                            ["Blue", "Green"])

x = np.arange(1, 11, 1)
y = np.arange(1, 8, 1)
grid = [(x, y) for x in range(11) for y in range(8)]
grid = pd.DataFrame(grid)
grid["id"] = grid.index.values
grid = grid.rename(columns = {0:"x", 1:"y"})

groundhogPredictions = pd.merge(groundhogPredictions, grid, on = "id")

plotGroundhog = (ggplot(groundhogPredictions, aes("x", "y")) 
                 + geom_tile(aes(fill = "shadow", width = 0.95, height = 0.95))
                 + geom_tile(aes(fill = "country", width = 0.5, height = 0.5))
                 + scale_fill_manual(["#c6d7b9", "#d6fffb", "#ee6f68", "#5a8ff6"])
                 + labs(title = "Groundhog Day Predictions (2023)",
                        subtitle = "This week, we explore Groundhog's Day predictions for 2023.This\nvisualization shows Spring/Winterpredictions from Groundhogs\nreporting in both Canada and the USA.",
                        caption = "Source: groundhog-day.com | #TidyTuesday | Week 5 | @hdailey",
                        x = "",
                        y = "")
                 + theme_538()
                 + theme(plot_title = element_text(hjust = 0.5),
                         plot_subtitle = element_text(ha = "center", linespacing = 1.5),
                         axis_text = element_blank(),
                         panel_grid = element_blank(),
                         legend_box = element_text(margin = {"b":-100}),
                         legend_direction = "horizontal",
                         legend_position = "top",
                         legend_title = element_blank()))
plotGroundhog
ggplot.save(plotGroundhog, filename = "2024-02-06_TT.png")
