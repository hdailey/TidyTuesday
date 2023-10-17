# Tidy Tuesday - October 03, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(wesanderson)

## fonts
font_add_google("IM Fell DW Pica", db_cache = FALSE) #folklore, evermore
font_add_google("UnifrakturCook", regular.wt = 700, db_cache = FALSE) #reputation
font_add_google("Satisfy", db_cache = FALSE) #Lover
font_add_google("Permanent Marker", db_cache = FALSE) #1989
font_add_google("Mohave", db_cache = FALSE) #Red
font_add_google("Archivo", db_cache = FALSE) #Fearless
font_add_google("Dancing Script", db_cache = FALSE) #Taylor Swift
font_add_google("Parisienne", db_cache = FALSE) #Speak Now
font_add_google("IBM Plex Serif") #Midnights
font_add_google("Roboto Condensed")
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 42)
albumSongs <- tuesdata$taylor_album_songs
allSongs <- tuesdata$taylor_all_songs
albums <- tuesdata$taylor_albums

fonts <- c("Dancing Script", "Parisienne", "Permanent Marker", "UnifrakturCook",  
           "Satisfy", "IM Fell DW Pica", "IM Fell DW Pica", "Archivo", "Mohave", "IBM Plex Serif")
# Data Exploration ####
swiftData <- albumSongs %>%
  left_join(albums, by = "album_name") %>%
  mutate(label = glue::glue("<strong>{album_name}</strong>", "<br>", "Metacritic Score: ", "{metacritic_score}")) %>%
  arrange(desc(album_release.x)) %>%
  mutate(label = fct_inorder(label)) %>%
  mutate(font = case_when(album_name == "Midnights" ~ "IBM Plex Serif",
                          album_name == "Red (Taylor's Version)" ~ "Mohave",
                          album_name == "Fearless (Taylor's Version)" ~ "Archivo",
                          album_name == "evermore" ~ "IM Fell DW Pica",
                          album_name == "folklore" ~ "IM Fell DW Pica",
                          album_name == "Lover" ~ "Satisfy",
                          album_name == "reputation" ~ "UnifrakturCook",
                          album_name == "1989" ~ "Permanent Marker",
                          album_name == "Speak Now" ~ "Parisienne",
                          album_name == "Taylor Swift" ~ "Dancing Script",
                          .default = "Roboto")) %>%
  group_by(key_mode, label) %>%
  summarise(n = n(), font) %>%
  filter(!is.na(key_mode)) %>%
  mutate(key_mode = str_to_title(key_mode),
           label = fct_rev(label))

# Data Visualization ####
plotFinal <- swiftData %>%
  ggplot(aes(x = key_mode, y = label)) +
  geom_tile(aes(fill = n), height = 0.8, width = 0.8, show.legend = FALSE, ) +
  geom_text(aes(label = n), colour = "grey17", size = 8, fontface = "bold", family = "Roboto Condensed") +
  scale_fill_gradientn(colours = wes_palette(name = "Moonrise3", type = "continuous")) +
  labs(y = "",
       title = "Taylor Swift Songs (2006-2022)",
       subtitle = glue::glue("This week, we explore Taylor Swift's discography from 2006 through 2022 ", 
                             "looking at each album and the number of songs written in each key.", 
                             "Over the years, Taylor Swift has utilized more keys when writing songs than early on ", 
                             "in her career with the total number of songs on each album increasing as well."),
       caption = "Source: {taylor} via W. Jake Thompson | #TidyTuesday | Week 42 | @hdailey") +
  theme_void() +
  theme(text = element_text(family = "Roboto Condensed", colour = "grey97"),
        plot.title = element_text(size = 40, family = "Permanent Marker", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 24, lineheight = 0.3, 
                                                       margin = margin(t = 2, b = 5)),
        plot.caption = element_text(size = 12, colour = "grey55"),
        plot.caption.position = "plot",
        axis.text.y = ggtext::element_markdown(size = 18, hjust = 0.5, lineheight = 0.4,
                                               family = fonts, colour = "grey97"),
        axis.text.x = element_text(size = 14, hjust = 0.5, colour = "grey97", angle = 45),
        panel.grid.major.y = element_line(colour = "grey97"),
        plot.background = element_rect(fill = colorspace::lighten("grey15", amount = 0.2)),
        plot.margin = margin(2, 5, 5, 5))
  
# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-10-17_TaylorSwift/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")

