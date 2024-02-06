# -*- coding: utf-8 -*-
"""
Tidy Tuesday - February 6 2024
"""

import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
from plotnine import *

heritage = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv")
heritagePivot = pd.pivot_table(heritage, values = ["2004", "2022"], columns = "country")
heritagePivot["year"] = ["2004", "2022"]

plotFinal = heritagePivot.plot(x = "year", kind = "bar", stacked = True, title = "Number of World Heritage Sites",
                               color = ["#042E45", "#F54A3F", "#0A75AD"])

for i in plotFinal.containers:

    # Optional: if the segment is small or 0, customize the labels
    labels = [v.get_height() if v.get_height() > 0 else '' for v in i]
    
    # remove the labels parameter if it's not needed for customized labels
    plotFinal.bar_label(i, labels=labels, label_type='center', color = "white")