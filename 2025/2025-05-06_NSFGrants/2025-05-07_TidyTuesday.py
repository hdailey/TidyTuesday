# -*- coding: utf-8 -*-
"""
Tidy Tuesday - May 7, 2025
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
from datetime import datetime
from plotnine import *
from matplotlib import rc

## Fonts
rc("font", **{"family":"serif", "serif":["Georgia"]})

## Data
tmp = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')
nsfTerminations = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')

# Data Cleaning & Exploration
## Filter for April 25 and convert from string to datetime
nsfTerminations = nsfTerminations.query("termination_letter_date == '2025-04-25'")
colsConvert = ["nsf_expected_end_date", "termination_letter_date", "nsf_startdate"]
nsfTerminations[colsConvert] = nsfTerminations[colsConvert].apply(pd.to_datetime)

for i in colsConvert:
    nsfTerminations[i] = nsfTerminations[i].astype("datetime64[ms]")
    
## Difference between dates
nsfTerminations["researchLost"] = nsfTerminations["nsf_expected_end_date"] - nsfTerminations["termination_letter_date"]
nsfTerminations["researchLost"] = nsfTerminations["researchLost"].dt.components.days

nsfTerminations["researchComplete"] = nsfTerminations["termination_letter_date"] - nsfTerminations["nsf_startdate"]
nsfTerminations["researchComplete"] = nsfTerminations["researchComplete"].dt.components.days

## Removing grants that completed full grant period
nsfTerminations = nsfTerminations.query("researchLost > 0")

## Creating ID
nsfTerminations["ID"] = nsfTerminations.index

## Convert datetime to int
for i in colsConvert:
    nsfTerminations[i] = nsfTerminations[i].apply(lambda x:x.toordinal())

## Select Columns
nsfTerminations_Selection = nsfTerminations[["ID", "in_cruz_list", "termination_letter_date", "usaspending_obligated", "nsf_startdate", "nsf_expected_end_date", "researchLost", "researchComplete"]]
nsfTerminations_Selection = nsfTerminations_Selection.sort_values(by = ["researchLost"], ascending = False)

nsfTerminationsCruz = nsfTerminations_Selection.query("in_cruz_list == True")

# Plot Visualization
## define breaks
breaks = (datetime(2020, 1, 1).toordinal(), datetime(2022, 1, 1).toordinal(), datetime(2024, 1, 1).toordinal(), datetime(2026, 1, 1).toordinal(), datetime(2028, 1, 1).toordinal())

## Plot
plotFinal = (ggplot(nsfTerminations_Selection)
              + geom_tile(aes(x = "reorder(ID, researchLost)", y = "termination_letter_date - (researchComplete/2)", height = "-researchComplete", width = 1, fill = "in_cruz_list"),
                          alpha = 0.5, size = 0)
              + geom_tile(aes(x = "reorder(ID, researchLost)", y = "termination_letter_date + (researchLost/2)", height = "researchLost", width = 1, fill = "in_cruz_list"),
                          size = 0)
              + annotate("text", x = 100, y = 740000, label = "Loss of Research Days", fontweight = "bold")
              + annotate("text", x = 540, y = 737500, label = "Completed Research Days", fontweight = "bold")
              + annotate("path", x = [100, 200], y = [739900, 739700], arrow = arrow(length = 0.05, type = "closed", angle = 45))
              + annotate("path", x = [550, 450], y = [737600, 738000], arrow = arrow(length = 0.05, type = "closed", angle = 45))
              + scale_fill_manual(("#666666", "#AD3232"))
              + scale_y_continuous(breaks = breaks, labels = ("2020", "2022", "2024", "2026", "2028"))
              + coord_cartesian(expand = True)
              + labs(title = "NSF Grants Terminated Under the Trump Administration (2025)",
                     subtitle = "On April 25, 2025, 340,226 days of research under 648 different grants was lost due to NSF grants terminated by the Trump\nAdministration. Of these 648, 23% or 152 were identified by Senator Ted Cruz for promoting DEI or advanced neo-Marxist\nclass warfare propaganda (identified in red).",
                     caption = "Source: Grant Watch | #TidyTuesday | Week 18 | @hdailey inspired by @nrennie",
                     x = "",
                     y = "")
              + theme_void()
              + theme(text = element_text(family = "Georgia", size = 12),
                      plot_title = element_text(face = "bold", size = 18),
                      plot_subtitle = element_text(size = 14, linespacing = 1.5),
                      plot_caption = element_text(size = 10, color = "#848484"),
                      plot_background = element_rect(fill = "#C0C0C0", color = None),
                      panel_background = element_rect(fill = "#C0C0C0", color = None),
                      axis_text_y = element_text(),
                      axis_text_x = element_blank(),
                      figure_size = (12, 6),
                      plot_margin = 0.01,
                      legend_position="none")
              )              
plotFinal

# Save and make final edits in Inkscape
plotFinal.save(filename='2025-05-07_TT.svg', dpi=1000)
