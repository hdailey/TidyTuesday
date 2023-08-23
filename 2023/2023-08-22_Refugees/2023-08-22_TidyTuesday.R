# Tidy Tuesday - August 22, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggtext)

## fonts
font_add_google("Nova Flat", db_cache = FALSE)
font_add_google("Roboto", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 34)

# Data Exploration ####
population <- tuesdata$population 

populationUkraine <- population %>%
  filter(coo_name == "Ukraine") %>%
  select(year, refugees, idps, ooc) %>%
  group_by(year) %>%
  summarise(refugeesSum = sum(refugees), idpsSum = sum(idps), oocSum = sum(ooc)) %>%
  pivot_longer(!year, names_to = "category", values_to = "count") %>%
  mutate(category = as.factor(category))

# Data Visualization ####
plotUkraine <- populationUkraine %>%
  ggplot(aes(factor(year), count, fill = category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  annotate("label", x = 8, y = 7500000, label = "Forcibly Displaced Persons Originating From Ukraine", family = "Nova Flat", size = 12.5, colour = "grey95", fill = "grey25", label.size = NA) +
  annotate("richtext", x = 6, y = 7500000, label = glue::glue("The United Nations High Commissioner for Refugees (UNHCR) Data Finder database <br> covers forcibly displaced populations ",
                                                              "including <span style='color:#0057B7'>refugees</span>, <span style='color:#ABABAB'>individuals of concern</span> and <br>",
                                                              "<span style='color:#FFDD00'>internaly displaced persons</span>. ",
                                                              "This graphic explores the forcibly displaced populations <br> originating from Ukraine in the UNHCR database with a significant increase observed <br> in 2022."),
          family = "Roboto", size = 7, colour = "grey95", fill = "grey25", label.size = NA, lineheight = 0.4) +
  coord_flip() +
  scale_fill_discrete(type = c("#FFDD00", "#ABABAB", "#0057B7")) +
  labs(y = "",
       x = "",
       caption = "{refugees} R package | #TidyTuesday | Week 34 | @hdailey") +
  ggthemes::theme_clean() +
  theme(text = element_text(family = "Roboto", colour = "grey95"),
        plot.caption = element_text(family = "Roboto", size = 14),
        plot.caption.position = "plot",
        axis.line.x = element_line(colour = "grey95"),
        axis.line.y = element_line(colour = "grey95"),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Nova Flat", colour = "grey95", size = 16),
        plot.background = element_rect(fill = "grey25", colour = "grey25"),
        panel.background = element_rect(fill = "grey25", colour = "grey25"),
        plot.margin = margin(10, 10, 2, 0))

# Save ####
ggsave(plot = plotUkraine, path = here::here("2023/2023-08-22_Refugees/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")

