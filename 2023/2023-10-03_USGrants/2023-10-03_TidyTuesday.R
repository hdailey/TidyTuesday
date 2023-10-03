# Tidy Tuesday - October 03, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("IBM Plex Serif", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 40)

# Data Exploration ####
grants <- tuesdata$grants
detail <- tuesdata$grant_opportunity_details

byAgency <- grants %>%
  filter(year(posted_date) == 2023) %>%
  group_by(agency_name) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(agency_name = fct_reorder(agency_name, n))

grantsNIH <- grants %>%
  filter(agency_name == byAgency$agency_name[1]) %>%
  mutate(year = lubridate::year(posted_date)) %>%
  filter(!is.na(estimated_funding)) %>%
  group_by(year) %>%
  summarise(totalFunding = sum(estimated_funding)) %>%
  mutate(colourPal = case_when(year == 2006 ~ "#025590", 
                            year == 2009 ~ "#A60900",
                            .default = "#000000"))

grantsNIH_minmax <- grantsNIH %>%
  slice(which.min(totalFunding), which.max(totalFunding))

percMinMax = signif(grantsNIH_minmax$totalFunding[2] / grantsNIH_minmax$totalFunding[1], 3)

# Data Visualization ####
agencyPlot <- byAgency %>%
  ggplot(aes(x = n, y = agency_name)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal()

plotFinal <- grantsNIH %>%
  ggplot(aes(x = year, y = totalFunding)) +
  geom_step(linewidth = 0.2) +
  ggrepel::geom_label_repel(data = grantsNIH_minmax %>% filter(year == 2006), aes(label = scales::dollar_format()(totalFunding), 
                                                                                  colour = colourPal), 
                            size = 6, family = "IBM Plex Serif", fontface = "bold", nudge_x = -1, nudge_y = 4e7) +
  ggrepel::geom_label_repel(data = grantsNIH_minmax %>% filter(year == 2009), aes(label = scales::dollar_format()(totalFunding), 
                                                                                  colour = colourPal), 
                            size = 6, family = "IBM Plex Serif", fontface = "bold", nudge_x = -2) +
  geom_point(aes(fill = case_when(year == c(2006, 2009) ~ colourPal, .default = "#000000")), shape = 21) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_continuous(breaks = seq(2005, 2023, by = 1)) +
  scale_y_continuous(breaks = c(0, 1e9, 2e9, 3e9), labels = c("$0", "$1 Billion", "$2 Billion", "$3 Billion")) +
  labs(x = "",
       y = "",
       title = "National Institute of Health (NIH) Grants",
       subtitle = glue::glue("This visualization explores the total amount of grant funding issued by the NIH and affliated agencies ", 
                             "each year from 2006 through 2023. ", 
                             "The maximum and minimum total amounts of grant funding were issued in the early-2000s with the greatest ", 
                             "amount of grant funding issued in {grantsNIH_minmax[2, 1]} was {scales::dollar_format()(grantsNIH_minmax$totalFunding[2])} ", 
                             "whereas only {scales::dollar_format()(grantsNIH_minmax$totalFunding[1])} was issued in {grantsNIH_minmax[1, 1]}. ", 
                             "This represents a {percMinMax}% increase in {grantsNIH_minmax[2, 1]} as compared to {grantsNIH_minmax[1, 1]}."),
       caption = "Source: Grants.gov | #TidyTuesday | Week 40 | @hdailey") +
  cowplot::theme_half_open(line_size = 0.2) +
  cowplot::background_grid(size.major = 0.1) +
  theme(text = element_text(size = 20, family = "IBM Plex Serif", colour = "#000000"),
        plot.title = element_text(size = 32, family = "IBM Plex Serif", face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, margin = margin(t = -5, b = 5)),
        plot.caption = element_text(size = 10, hjust = 0, colour = "grey45"),
        plot.caption.position = "plot",
        plot.margin = margin(5, 5, 2, 5),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 0.8),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#000000"))
  
# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-10-03_USGrants"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")
