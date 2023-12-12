# Tidy Tuesday - December 12, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggstream)
library(darknerdthemes)

## fonts
font_add_google("Mountains of Christmas", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 50)

# Data Exploration ####
genres <- tuesdata$holiday_movie_genres
movies <- tuesdata$holiday_movies %>%
  left_join(genres, by = c("tconst" = "tconst")) %>%
  select(c(year, genres.y)) %>%
  group_by(year, genres.y) %>%
  summarise(n = n()) %>%
  filter(year >= 1990) %>%
  mutate(label = case_when(genres.y == "Drama" ~ "Drama",
                           genres.y == "Romance" ~ "Romance",
                           genres.y == "Comedy" ~ "Comedy",
                           genres.y == "Family" ~ "Family",
                           .default = NA))

pal <- colorRampPalette(colors = c("#FF0000", "#FF7878", "#FFFFFF", "#74D680", "#178B29"))(25)

# Data Visualization ####
plotFinal <- movies %>%
  ggplot(aes(year, n, fill = genres.y)) +
  geom_stream(colour = "white", linewidth = 0.055, type = "ridge") +
  annotate("text", x = 2020, y = 65, label = "Romance", colour = "white", fontface = "bold", size = 18, family = "Mountains of Christmas") +
  annotate("text", x = 2020, y = 215, label = "Drama", colour = "white", fontface = "bold", size = 18, family = "Mountains of Christmas") +
  annotate("text", x = 2020, y = 295, label = "Comedy", colour = "white", fontface = "bold", size = 18, family = "Mountains of Christmas") +
  annotate("text", x = 2020, y = 165, label = "Family", colour = "white", fontface = "bold", size = 18, family = "Mountains of Christmas") +
  nerd_theme_gruv() +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "# of Holiday Movies",
       title = "Distribution of Holiday Movies by Year and Genre",
       caption = "Source: IMDb.com | #TidyTuesday | Week 50 | @hdailey",
       fill = "") +
  theme(text = element_text(family = "Mountains of Christmas", size = 36),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 50),
        legend.justification = "center",
        legend.text = element_text(size = 32, margin = margin(l = -10)),
        legend.margin = margin(b = 10),
        plot.caption = element_text(size = 28, margin = margin(t = -20)),
        axis.text = element_text(size = 48),
        panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.5, colour = "#EBDBB2"),
        panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.5)) +
  guides(fill = guide_legend(nrow = 3))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-12-12_HolidayMovies/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
