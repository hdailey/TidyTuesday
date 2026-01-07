# -*- coding: utf-8 -*-
"""
Tidy Tuesday - January 6, 2026
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
from plotnine import *
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import os

## Fonts
projDir = os.getcwd()
font_path = os.path.join(projDir, "fonts/AsapCondensed-Black.ttf")
prop = fm.FontProperties(fname = font_path)

## Data
athleteEvents = pd.read_csv('data/athlete_events.csv')
nocRegions = pd.read_csv('data/noc_regions.csv')

# Data Cleaning & Exploration
seasonFilter = "Winter"                                             #

## Movie edit
athleteEventsSeason = athleteEvents[athleteEvents["Season"].str.contains(seasonFilter)]    
athleteEventsSeason = athleteEventsSeason.dropna()
athleteEventsSeasonMedalTotal = athleteEventsSeason.groupby(["NOC"]).agg(medals = ("Medal", "count")).reset_index()
athleteEventsSeasonMedalTotal = athleteEventsSeasonMedalTotal.sort_values(by = ["medals"], ascending = False)
athleteEventsSeasonMedalTotal = athleteEventsSeasonMedalTotal.head(1)       

topMedalCountry = athleteEventsSeasonMedalTotal.iloc[0].NOC

athleteEventsCountry = athleteEventsSeason[athleteEventsSeason["NOC"].str.contains(topMedalCountry)]
athleteEventsCountry = athleteEventsCountry.groupby(["Year", "Sport"]).agg(Medals = ("Medal", "count")).reset_index()

events = list(set(athleteEventsCountry["Sport"]))

athleteEventsFinal = athleteEvents[athleteEvents["NOC"].str.contains(topMedalCountry)]
athleteEventsFinal = athleteEventsFinal[athleteEventsFinal["Sport"].isin(events)]
athleteEventsFinal = athleteEventsFinal.groupby(["Year", "Sport"]).agg(Medals = ("Medal", "count")).reset_index()
athleteEventsFinal = athleteEventsFinal.sort_values(by = "Sport")

# Reindex dataframe to backfill with zeros
all_years = athleteEventsFinal["Year"].unique()
all_sports = athleteEventsFinal["Sport"].unique()
idx = pd.MultiIndex.from_product([all_years, all_sports])
athleteEventsFinal = athleteEventsFinal.set_index(["Year", "Sport"]).reindex(idx, fill_value = 0).reset_index()
athleteEventsFinal = athleteEventsFinal.rename(columns = {"level_0":"Year", "level_1":"Sport"})
athleteEventsFinal["labelFill"] = pd.cut(athleteEventsFinal["Medals"], (-1, 30, 50), labels = ("low", "high"))
athleteEventsFinal["MedalsMasked"] = np.ma.masked_where(athleteEventsFinal["Medals"] == 0, athleteEventsFinal["Medals"])

# Data Visualization
plotFinal = (
    ggplot(athleteEventsFinal, aes(x = "factor(Year)", y = "Sport", fill = "MedalsMasked"))
    + geom_tile(aes(width = 0.95, height = 0.95))
    + geom_text(aes(label = "Medals", color = "labelFill"), size = 8, show_legend = False)
    + scale_color_manual(["white", "purple"])
    + scale_fill_cmap(cmap_name = "plasma", na_value = "black")
    + labs(x = "",
           y = "",
           title = "Canadian Winter Olympic Medals",
           fill = "Medals",
           caption = "Source: @RGRIFFIN via kaggle | #TidyTuesday | Week 1: BYO Data | @hdailey")
    + theme_void()
    + theme(text = element_text(fontproperties = prop),
            plot_caption = element_text(ha = "center", size = 4),
            axis_text_x = element_text(angle = 45),
            legend_direction = "horizontal",
            legend_position = "bottom",
            legend_title_position = "top")
    )

plotFinal

# Save
plotFinal.save("2026-01-07.svg", height = 5, width = 7, units = "in", dpi = 300)

# Make Edits in Inkscape

