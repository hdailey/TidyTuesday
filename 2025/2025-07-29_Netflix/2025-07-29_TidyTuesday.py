# -*- coding: utf-8 -*-
"""
Tidy Tuesday - July 29, 2025
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import pydytuesday as pdt
from plotnine import *
from pyfonts import load_google_font
import mizani.labels as ml

## Fonts
prop = load_google_font("Bebas Neue")

## Data
pdt.get_date('2025-07-29')
movies = pd.read_csv('movies.csv')
shows = pd.read_csv('shows.csv')

# Data Cleaning & Exploration
reportYear = "2025Jan-Jun"                                                     # Set report year

## Movie edit
moviesEdit = movies[movies['report'].str.contains(reportYear)]                 # Filter for movies in that report year
moviesEdit = moviesEdit.sort_values(by = ["views"], ascending = False)         # Sort descending by views
moviesEdit = moviesEdit.head(10)                                               # Slice top ten movies
moviesEdit['viewsLabel'] = moviesEdit["views"].apply(lambda x: f'{x/1e6:,.1f}M')
moviesEdit['viewsYaxis'] = moviesEdit["views"] - 2e7
moviesEdit = moviesEdit.sort_values(by = ["views"], ascending = True)
moviesEdit["title"] = pd.Categorical(moviesEdit["title"], categories = moviesEdit["title"],
                                     ordered = True)# Convert names to factor

# Data Visulization
## Inspired by @gkaramanis - https://github.com/gkaramanis/tidytuesday/tree/master/2025/2025-week_31
plotFinal = (
    ggplot(moviesEdit, aes(x = "title", y = "views"))
    + geom_col(color = "red", fill = None)
    + geom_text(aes(x = "title", y = "viewsYaxis", label = "viewsLabel"))
    + scale_y_continuous(limits = (0, max(moviesEdit["viewsYaxis"]) + 8e7),
                         labels = ml.label_comma())
    + coord_flip()
    + labs(x = "",
           y = "Number of Views")
    + theme_void()
    + theme(text = element_text(fontproperties = prop))
    )

# Save
plotFinal.save("2025-01-02.svg", width = 317, height = 151, units = "mm")

# Make Edits in Inkscape

