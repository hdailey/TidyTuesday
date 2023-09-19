# Tidy Tuesday - September 19, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggraph)
library(tidygraph)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 38)

# Data Exploration ####
cran <- tuesdata$cran_20230905
package <- tuesdata$package_authors
nodes <- tuesdata$cran_graph_nodes
edges <- tuesdata$cran_graph_edges

graphData <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
nodesDist2HW1 <- nodes %>%
  filter(dist2HW == 1) %>%
  summarise(n = n())

# Data Visualization ####
plotFinal <- ggraph(graphData, layout = "manual", x = x, y = y) +
  geom_edge_link0(edge_width = 0.2, edge_colour = "#F9F9F9", alpha = 0.5) +
  geom_node_point(data = nodes %>% filter(dist2HW != 1 | dist2HW != 0),
                  fill = "#F9F9F9", color = "#F9F9F9",
                  shape = 21, stroke = 0.1, size = 0.2) +
  geom_node_point(data = nodes %>% filter(dist2HW == 1),
                  fill = "#56B4E9", color = "#F9F9F9",
                  shape = 21, stroke = 0.1, size = 0.3) +
  geom_node_point(data = nodes %>% filter(dist2HW == 0),
                  fill = "black", color = "#F9F9F9",
                  shape = 21, stroke = 0.1, size = 0.5) +
  labs(title = "CRAN Collaboration Graph",
       subtitle = glue::glue("This visualization recreates the CRAN Collaboration Graph by @schochastics which explores the connection of ",
                             "R package developers who are connected if they appear with one another as authors of an R package in the ", 
                             "description file. Highlighted in blue are the R developers that have a Hadley number of 1 (n = {nodesDist2HW1$n}) ", 
                             "and Hadley himself with a Hadley number of 0 is highlighted in black."),
       caption = "Source: CRAN Collaboration Graph via @schochastics | #TidyTuesday | Week 28 | Inspired by @schochastics | @hdailey") +
  theme_graph(background = "black") +
  theme(plot.title = element_text(family = "Roboto", face = "bold", colour = "#F9F9F9", size = 48),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(family = "Roboto Condensed", colour = "#F9F9F9", lineheight = 0.3, size = 20, 
                                                       margin = margin(t = -8, r = 265, b = -50)),
        plot.caption = element_text(family = "Roboto Condensed", face = "plain", colour = "#F9F9F9", hjust = 0.98, size = 12,  
                                    margin = margin(b = 1)),
        plot.caption.position = "plot",
        plot.margin = margin(3, 0, 1, 3))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-09-19_CRAN/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")
