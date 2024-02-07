# -*- coding: utf-8 -*-
"""
Tidy Tuesday - February 6 2024
"""

import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
from plotnine import *
from matplotlib import rc
import seaborn

rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

heritage = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv")
heritagePivot = pd.pivot_table(heritage, values = ["2004", "2022"], columns = "country")
heritagePivot["year"] = ["2004", "2022"]

# Plot Visualization
plotFinal = heritagePivot.plot(x = "year", kind = "bar", stacked = True,
                               color = ["#042E45", "#F54A3F", "#0A75AD"], fontsize = 18)
## Add Labels to Top of Each Bar
plotFinal.bar_label(plotFinal.containers[-1], fmt = "%.0f", size = 18, weight = "bold")
plotFinal.text(1.4, 25, "Sweden", fontsize = 22, color = "#0A75AD", bbox = {"facecolor": "white"})
plotFinal.text(1.4, 13, "Norway", fontsize = 22, color = "#F54A3F", bbox = {"facecolor": "white"})
plotFinal.text(1.4, 4, "Denmark", fontsize = 22, color = "#042E45", bbox = {"facecolor": "white"})
plotFinal.spines["top"].set_visible(False)
plotFinal.spines["right"].set_visible(False)
plotFinal.spines["bottom"].set_visible(False)
plotFinal.tick_params(axis = "x", rotation = 0)
plotFinal.set(xlabel = None)
plotFinal.get_legend().remove()
plotFinal.yaxis.grid()
plotFinal.text(0, -7, "Source: 1 Dataset, 100 Visualizations Project | #TidyTuesday | Week 6 | @hdailey inspired by #1",
               wrap = False, size = 8, color = "#BCBCBC")

## Add Labels to Each Box
for i in plotFinal.containers:
   
    # remove the labels parameter if it's not needed for customized labels
    plotFinal.bar_label(i, fmt = lambda x: f'{x:.0f}' if x > 0 else " ", label_type='center', color = "white", size = 18)
    
plotFinal

