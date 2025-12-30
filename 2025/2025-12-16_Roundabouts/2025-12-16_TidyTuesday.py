# -*- coding: utf-8 -*-
"""
Tidy Tuesday - December 16, 2025
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
from matplotlib import rc
import pydytuesday as pdt
import folium
import imageio as iio


## Fonts
rc("font", **{"family":"serif", "serif":["Georgia"]})

## Data
pdt.get_date('2025-12-16')
roundaboutClean = pd.read_csv('roundabouts_clean.csv')

# Data Cleaning & Exploration
## Filter for Icelandic roundabouts
icelandRoundabout = roundaboutClean[roundaboutClean['country'].str.contains("Iceland")]

# Data Visulization
## Setup Folium Map
icelandMap = folium.Map(location = (64.9631, -19.0208), zoom_start = 7, tiles = "OpenStreetMap")

## Setting up icon
icon_image = iio.imread("RoundaboutSign.png")
shadow_image = iio.imread("RoundaboutSign.png")

icon = folium.CustomIcon(
    icon_image,
    icon_size=(38, 38),
    icon_anchor=(0, 0),
    popup_anchor=(-3, -76),
)

title = "Roundabouts in Iceland - Source: roundabouts package | #TidyTuesday | Week 50 | @hdailey"

title_html = '''
             <h3 align="center" style="font-family:Georgia; font-size:16px"><b>{}</b></h3>
             '''.format(title)  

# From https://caellwyn.medium.com/quick-and-beautiful-interactive-maps-in-folium-c6fd535c3d3b
# Loop through the dataframe to add markers according to the 
# latitude and longitude columns in each row.
# Add tooltips displaying the contents of the name column of each row.
for index, row in icelandRoundabout.iterrows():
    info = row["name"], row["county_area"], row["country"], row["lane_type"]
    info = list(info)
    text = f'<strong> {info[0]} - {info[3]} Roundabout </strong> <br> {info[1]}, {info[2]}'
    folium.Marker(
        [row['long'],row['lat']],
        icon = icon,
        tooltip = text).add_to(icelandMap)
        
icelandMap.get_root().html.add_child(folium.Element(title_html))

# Save
icelandMap.save('icelandMap.html')