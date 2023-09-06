# Tidy Tuesday - September 5, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Archivo", db_cache = FALSE)
font_add_google("Archivo Narrow", db_cache = FALSE)
showtext_auto()

## data
demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')


# Data Exploration ####
wagesDemographics <- wages %>%
  pivot_longer(cols = c(union_wage, nonunion_wage, wage),
               names_to = "category",
               values_to = "wageValue") %>%
  filter(grepl("less than college|college or more|demographics: male|demographics: female|public sector: all|private sector: all", facet)) %>%
  mutate(facet = case_when(facet == "demographics: less than college" ~ "Less Than College",
                           facet == "demographics: college or more" ~ "College or More",
                           facet == "demographics: male" ~ "Male",
                           facet == "demographics: female" ~ "Female",
                           facet == "public sector: all" ~ "Public Sector",
                           facet == "private sector: all" ~ "Private Sector",
                           .default = facet)) %>%
  mutate(facet = factor(facet, 
                        levels = c("Public Sector", "College or More", "Female",
                                   "Private Sector", "Less Than College", "Male"),
                        labels = c("Public Sector", "College or More", "Female",
                                   "Private Sector", "Less Than College", "Male")))

# Data Visualization ####
plotFinal <- wagesDemographics %>%
  ggplot() +
  geom_line(aes(year, union_wage_premium_adjusted, group = facet, colour = facet)) +
  gghighlight::gghighlight(use_direct_label = FALSE,
                           unhighlighted_params = list(linewidth = 0.5)) +
  facet_wrap(~facet) +
  scale_y_continuous(labels = scales::label_percent(prefix = "+")) +
  labs(x = "",
       y = "Salary of Union Member vs. Non-Union Members",
       title = "Union Wages Over Time",
       subtitle = glue::glue("The Union Membership and Coverage Database provides data related to private and public sector labor ",
                             "union membership, coverage and density estimates which are compiled from the household Current Population Survey. ",
                             "Estimates are provided by state, detailed industry and occupation starting in 1973. ",
                             "Overall, for education and gender demographics as well as by private vs public sector, union members earn ",
                             "higher wages than their non-union counterparts. ",
                             "This trend is on the decline though for many demographics and sectors except for those that are not college educated."),
       caption = glue::glue("Source: Hirsch, Barry T., and David A. Macpherson. 2003. “Union Membership and Coverage Database from the Current Population Survey: Note.” Industrial and Labor Relations Review 56(2): 349–54 (updated annually at Unionstats.com).",
                            "#TidyTuesday | Week 36 | @hdailey")) +
  MetBrewer::scale_colour_met_d(name = "Moreau") +
  geom_smooth(aes(x = year, y = union_wage_premium_adjusted, colour = facet), method = "lm") +
  cowplot::theme_minimal_grid() +
  theme(text = element_text(family = "Archivo Narrow", size = 18),
        plot.title = element_text(family = "Archivo", face = "bold", size = 48),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, size = 20),
        legend.position = "none",
        axis.text = element_text(family = "Archivo Narrow", size = 18),
        strip.text = element_text(family = "Archivo", face = "bold", size = 18),
        plot.caption = ggtext::element_textbox_simple(hjust = 0, colour = "grey45", size = 10),
        plot.caption.position = "plot",
        plot.background = element_rect(color = "#fafafa", fill = "#fafafa"),
        panel.background = element_rect(color = "#fafafa", fill = "#fafafa"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-09-05_Unions/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")
