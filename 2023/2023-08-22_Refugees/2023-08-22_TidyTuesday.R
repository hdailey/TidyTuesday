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
population <- tuesdata$population #%>%
  # mutate(coo_name = case_when(coo_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
  #                             .default = coo_name))

# population2022_destination <- population %>%
#   filter(year == 2022) %>%
#   group_by(coa_name) %>%
#   summarise(n = sum(asylum_seekers)) %>%
#   arrange(desc(n)) %>%
#   slice_head(n = 5)
# 
# destination <- population2022_destination[1]
# 
# population2022_origin <- population %>%
#   filter(year == 2022) %>%
#   group_by(coo_name) %>%
#   summarise(n = sum(asylum_seekers)) %>%
#   arrange(desc(n)) %>%
#   slice_head(n = 5) %>%
#   mutate(coo_name = case_when(coo_name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
#                               .default = coo_name)) %>%
#   slice_head(n = 5)
# 
# origin <- population2022_origin[1]
# 
# populationDestination <- population %>% 
#   filter(coa_name %in% c(destination$coa_name)) %>%
#   group_by(year, coa_name) %>%
#   summarise(n = sum(asylum_seekers))

populationOrigin <- population %>%
  filter(coo_name == "Ukraine") %>%
  select(year, refugees, idps, ooc) %>%
  group_by(year) %>%
  summarise(refugeesSum = sum(refugees), idpsSum = sum(idps), oocSum = sum(ooc)) %>%
  pivot_longer(!year, names_to = "category", values_to = "count") %>%
  mutate(category = as.factor(category))

# Data Visualization ####
plotUkraine <- populationOrigin %>%
  ggplot(aes(factor(year), count, fill = category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  annotate("label", x = 8, y = 7500000, label = "Forcibly Displaced Persons Originating From Ukraine", family = "Nova Flat", size = 18, colour = "grey95", fill = "grey25", label.size = NA) +
  annotate("richtext", x = 5.75, y = 7500000, label = glue::glue("The United Nations High Commissioner for Refugees (UNHCR) Data Finder database <br> covers forcibly displaced populations ",
                                                              "including <span style='color:#0057B7'>refugees</span>, <span style='color:#ABABAB'>individuals of concern</span> and <br>",
                                                              "<span style='color:#FFDD00'>internaly displaced persons</span>. ",
                                                              "This graphic explores the forcibly displaced populations <br> originating from Ukraine in the UNHCR database with a significant increase observed <br> in 2022."),
          family = "Roboto", size = 8, colour = "grey95", fill = "grey25", label.size = NA, lineheight = 0.3) +
  coord_flip() +
  scale_fill_discrete(type = c("#FFDD00", "#ABABAB", "#0057B7")) +
  labs(y = "",
       x = "",
       caption = "{refugees} R package | #TidyTuesday | Week 34 | @hdailey") +
  ggthemes::theme_clean() +
  theme(text = element_text(family = "Roboto", colour = "grey95"),
        plot.title = element_text(family = "Nova Flat"),
        plot.subtitle = ggtext::element_textbox_simple(),
        plot.caption = element_text(family = "Roboto", size = 12),
        plot.caption.position = "plot",
        axis.line.x = element_line(colour = "grey95"),
        axis.line.y = element_line(colour = "grey95"),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Nova Flat", colour = "grey95", size = 12),
        plot.background = element_rect(fill = "grey25", colour = "grey25"),
        panel.background = element_rect(fill = "grey25", colour = "grey25"))

plotFinal <- plotDestination / plotOrigin

# Save ####
ggsave(plot = plotUkraine, path = here::here("2023/2023-08-22_Refugees/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")

