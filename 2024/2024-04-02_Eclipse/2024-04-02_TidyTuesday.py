# -*- coding: utf-8 -*-
"""
Tidy Tuesday - April 2, 2024
"""
# Libraries, Fonts, Data
## Libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as FuncFormatter
import matplotlib.patches as mpatches
from mpl_toolkits.basemap import Basemap
import geopandas as gpd
from shapely.geometry import Polygon
import wget
from matplotlib import *
import time
import math

## Fonts
rc("font", **{"family":"sans-serif", "sans-serif":["Calibri"]})

## Data
### Eclipse Data
eclipse2024 = pd.read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-09/eclipse_total_2024.csv")

### US Map
#wget.download("https://www2.census.gov/geo/tiger/TIGER2023/STATE/tl_2023_us_state.zip")
usMap = gpd.read_file("C:/Users/hdailey/OneDrive - Water Boards/Reference/DataViz/TidyTuesday/2024/2024-04-02_Eclipse/tl_2023_us_state/tl_2023_us_state.shp")

# Data Exploration
## Filter out non-CONUS States/Territories
usMapFiltered = usMap.query("NAME != ['Alaska', 'Hawaii', 'United States Virgin Islands', \
                            'Commonwealth of the Northern Mariana Islands', 'American Samoa', \
                                'Puerto Rico']")
                            
## Convert eclipse3 column to datetime
eclipse2024["eclipse3_dt"] = pd.to_datetime(eclipse2024["eclipse_3"], format = "%H:%M:%S").astype("int64")
eclipse2024[["HH", "MM", "SS"]] = eclipse2024["eclipse_3"].str.split(":", expand = True)
eclipse2024["HHMM"] = eclipse2024["HH"].astype(str) + eclipse2024["MM"].astype(str)
eclipse2024["HHMM"] = eclipse2024["HHMM"].astype("int64")
conditions = [
    (eclipse2024["HHMM"] <= 1830),
    (eclipse2024["HHMM"] > 1830) & (eclipse2024["HHMM"] <= 1845),
    (eclipse2024["HHMM"] > 1845) & (eclipse2024["HHMM"] <= 1900),
    (eclipse2024["HHMM"] > 1900) & (eclipse2024["HHMM"] <= 1915),
    (eclipse2024["HHMM"] > 1915) & (eclipse2024["HHMM"] <= 1930),
    (eclipse2024["HHMM"]> 1930)
    ]

values = [1830, 1845, 1900, 1915, 1930, 1945]

eclipse2024["category"] = np.select(conditions, values)

# Plot Visualization
## create figure and axes for with Matplotlib for main map
fig, ax = plt.subplots(figsize = (18, 14))
fig.patch.set_facecolor("#9898B4")

## remove the axis box from the main map
ax.axis('off')
               
## plot us map
usMapFiltered[~usMapFiltered.STUSPS.isin(['HI','AK'])].plot(color='#4A3B4B', linewidth=0.8, ax=ax, edgecolor='0.8')

## plot eclipse
for i in range(0, len(values)):
    data = eclipse2024.loc[eclipse2024["HHMM"] == values[i]]
    plt.scatter(x = eclipse2024["lon"], y = eclipse2024["lat"], c = eclipse2024["HHMM"], cmap = "plasma", 
            alpha = 0.8, label = values[i])

## customize plot
ax.set_xlim(-127, -65)
ax.set_ylim(23, 50)
cbar = plt.colorbar(location = "bottom", orientation = "horizontal", shrink = 0.5, pad = 0.025)
cbar.ax.set_title("Total Phase Start Time (in UTC)", size = 24)
cbar.ax.set_xticklabels(["18:30", "18:45", "19:00", "19:15", "19:30", "19:45"])
cbar.ax.tick_params(labelsize = 18)
plt.title("2024 Total Solar Eclipse", loc = "center", size = 32, fontweight = "bold")
plt.figtext(0.33, 0.17, "Source: NASA.gov | #TidyTuesday | Week 15 | @hdailey inspired by @gkaramanis", size = 14)

# Save
plt.savefig("2024-04-09_TT.png", dpi = 600)