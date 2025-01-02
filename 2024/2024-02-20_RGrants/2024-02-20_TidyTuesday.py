# -*- coding: utf-8 -*-
"""
Tidy Tuesday - February 20, 2024
"""

import pandas as pd
import os
import numpy as np
from plotnine import *
from matplotlib import rc
from mizani.formatters import label_dollar

rc("font", **{"family":"serif", "serif":["Georgia"]})

isc_grants = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

isc_grantsByYear = isc_grants[["year", "funded"]].groupby(["year"]).sum().reset_index()
isc_grantsMin = isc_grantsByYear.query("year == 2023")
isc_grantsMax = isc_grantsByYear.query("year == 2018")

# Plot Visualization
plotFinal = (ggplot(isc_grantsByYear)
             + aes(x = "year", y = "funded")
             + geom_line(color = "white")
             + geom_point(isc_grantsMin, aes(x = "year", y = "funded"), color = "yellow")
             + geom_point(isc_grantsMax, aes(x = "year", y = "funded"), color = "yellow")
             + geom_text(aes(x = 2018, y = 310000), label = "Maximum Funding (2018)\n$289,972", color = "yellow")
             + geom_text(aes(x = 2021, y = 52000), label = "Minimum Funding (2023)\n$51,015", color = "yellow")
             + geom_text(aes(x = 2021.5, y = 200000), label = "Funding is on\nthe decline", color = "yellow", size = 18)
             + scale_y_continuous(labels = label_dollar())
             + labs(x = "",
                    y = "Total R Consortium ISC Grant\nFunding Awarded",
                    title = "R Consortium ISC Grant Funding",
                    subtitle = "(2016-2023)",
                    caption = "Source: R Consortium ISC | #TidyTuesday | Week 8 | @hdailey | Inspired by @curatedmess")
             + theme_tufte()
             + theme(text = element_text(family = "Georgia", color = "white", size = 14),
                     plot_background = element_rect(fill = "black"),
                     plot_title = element_text(hjust = -1, size = 16, face = "bold"),
                     plot_subtitle = element_text(ha = "center"),
                     plot_caption = element_text(ha = "center", colour = "white", size = 6),
                     axis_line = element_line(color = "white"),
                     axis_title_y = element_text(size = 14, hjust = 0.5, lineheight = 1.5)))
plotFinal
