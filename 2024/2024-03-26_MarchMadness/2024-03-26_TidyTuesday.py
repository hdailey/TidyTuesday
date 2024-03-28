# -*- coding: utf-8 -*-
"""
Tidy Tuesday - March 26, 2024
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
import janitor
import textwrap
from matplotlib import *
from plotnine import *

## Fonts
rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

## Data
team_results = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/team-results.csv")
public_picks = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-26/public-picks.csv")

# Data Exploration
public_picksPivot = public_picks.loc[:, "TEAM":"FINALS"]
public_picksPivot = public_picksPivot.sort_values("R64")

## Preserve sorting order for categorical factor via variable
teams = public_picksPivot["TEAM"]

public_picksPivot = public_picksPivot.pivot_longer("TEAM", sort_by_appearance = True)
public_picksPivot["TEAM"] = pd.Categorical(public_picksPivot["TEAM"], categories = teams, ordered = True)
public_picksPivot["value"] = public_picksPivot["value"].str.strip("%").astype("float")
public_picksPivot["variable"] = public_picksPivot["variable"].astype("category")
public_picksPivot["variable"] = public_picksPivot["variable"].cat.reorder_categories(["R64", "R32", "S16", "E8", "F4", "FINALS"])
public_picksPivot["ROUND"] = np.select(
    [
     public_picksPivot["variable"] == "R64",
     public_picksPivot["variable"] == "R32",
     public_picksPivot["variable"] == "S16",
     public_picksPivot["variable"] == "E8",
     public_picksPivot["variable"] == "F4",
     public_picksPivot["variable"] == "FINALS"
    ],
    [
     "Round of 64",
     "Round of 32",
     "Sweet 16",
     "Elite 8",
     "Final 4",
     "Finals"
     ],
    default = "Unknown"
    )
public_picksPivot["ROUND"] = public_picksPivot["ROUND"].astype("category")
public_picksPivot["ROUND"] = public_picksPivot["ROUND"].cat.reorder_categories(["Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final 4", "Finals"])

# Plot Visualization
plotFinal = (ggplot(public_picksPivot, aes(x = "TEAM", y = "ROUND", fill = "value"))
              + geom_tile()
              + scale_fill_cmap(cmap_name = "YlOrRd")
              + labs(x = "", 
                     y = "", 
                     title = "2024 NCAA Men's March Madness",
                     subtitle = textwrap.fill("March Madness is the premier NCAA Division I basketball tournament featuring both Men's and Women's teams. 68 teams face single elimination rounds where the public frequently make predictions as to who will be this year's champions as well as who wins each round of the tournament. For the Men's tournament, best bets are placed for UConn.",
                                              width = 97),
                     caption = "Source: Kaggle.com | #TidyTuesday | Week 13 | @hdailey | Inspired by @nrennie")
              + theme_minimal()
              + theme(text = element_text(family = "Calibri"),
                      plot_title = element_text(face = "bold", size = 12),
                      plot_subtitle = element_text(size = 10),
                      plot_caption = element_text(size = 6, colour = "grey"),
                      axis_text_y = element_text(face = "bold"),
                      axis_text_x = element_text(angle = 90, size = 6),
                      panel_background = element_rect(fill = "#FFFFFF"),
                      plot_background = element_rect(fill = "#FFFFFF"),
                      legend_position = "none"))

plotFinal.save("2024-03-28_TT.png", dpi = 1200)
               