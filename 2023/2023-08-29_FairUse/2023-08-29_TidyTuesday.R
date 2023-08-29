# Tidy Tuesday - August 29, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggrepel)
## fonts
font_add_google("Amita", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 35)
findings <- tuesdata$fair_use_findings
cases <- tuesdata$fair_use_cases



# Data Exploration ####
cases <- cases %>%
  separate_rows(categories, sep = ";|,|/") %>%
  mutate(categories = str_squish(categories)) %>%
  filter(categories != "") %>%
  mutate(categories = case_when(categories == "Photograph" ~ "Photography",
                                categories == "News Reporting Photograph" ~ "Photography",
                                categories == "satire" ~ "Satire",
                                categories == "Films" ~ "Film",
                                categories == "Educational" ~ "Education",
                                 .default = categories)) %>%
  filter(!str_detect(categories, "Circuit")) %>%
  mutate(categories = str_to_title(categories))

categoryCount <- cases %>%
  count(categories, sort = TRUE) %>%
  mutate(perc_count = n/sum(n))

categoryFUF <- cases %>%
  select(categories, fair_use_found) %>%
  count(categories, fair_use_found, sort = TRUE) %>%
  group_by(categories) %>%
  reframe(categories, fair_use_found, n, perc_fuf = n/sum(n)) %>%
  left_join(categoryCount, by = join_by(categories == categories)) %>%
  mutate(categories = factor(categories, levels = categoryCount$categories)) %>%
  rename(n_fuf = n.x,
         n_count = n.y) %>%
  mutate(label = case_when(n_count > 1 ~ glue::glue("{n_count} cases"), .default = glue::glue("{n_count} case")),
         label_fuf = case_when(n_fuf > 1 ~ glue::glue("{n_fuf} cases"), .default = glue::glue("{n_fuf} case"))) %>%
  filter(n_count >= 50) %>%
  mutate(countLabel = glue::glue("n = {n_count}"))

# Data Visualization ####
ggthemr::ggthemr("light")

plotFinal <- categoryFUF %>%
  ggplot(aes(x = perc_fuf, y = reorder(categories, n_count), fill = fair_use_found)) +
  geom_col() +
  geom_text_repel(data = categoryFUF %>% filter(fair_use_found == TRUE, categories %in% c("Digitization", "Internet", "Research", 
                                                                                          "Photography", "Film", "Textual Work")), seed = 1,
                  aes(label = label_fuf), force = 1, direction = "x", xlim = c(0.12, 0.12),
                  arrow = arrow(length = (unit(0, "npc"))), colour = "grey25", min.segment.length = 100,
                  family = "Amita", size = 8) +
  geom_text_repel(data = categoryFUF %>% filter(fair_use_found == FALSE, categories %in% c("Digitization", "Internet", "Research", 
                                                                                         "Photography", "Film", "Textual Work")), seed = 1, 
                  aes(label = label_fuf), force = 1, direction = "x", xlim = c(0.75, 0.75),
                  arrow = arrow(length = (unit(0, "npc"))), colour = "#FAFAFA", min.segment.length = 100,
                  family = "Amita", size = 8) +
  scale_fill_manual(values = c("#FF564D", "#9EEC1B")) +
  facet_wrap(~countLabel, nrow = 9, ncol = 1, scales = "free_y", as.table = TRUE) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, .25), labels = scales::percent) +
  labs(x = "",
       y = "",
       title = "Percentages of Fair Use Court Cases Determined By Courts",
       subtitle = glue::glue("The data for this week comes from the US Copyright Office Fair Use Index. ", 
                             "As a principle, 'fair use' is a cornerstone of US copyright law and the goal of ",
                             "the index is to make the principles/application of fair use more accessible and understandable.",
                             "<br>",
                             "This visualization explores the percentage of fair use cases that determined <span style='color:#9EEC1B'>**fair use was found**</span> ",
                             "as well as the percentage of cases that were determined that <span style='color:#FF564D'>**fair use was not found**</span>."), 
       caption = "Source: US Copyright Office Fair Use Index | #TidyTuesday | Week 35 | @hdailey") +
  cowplot::theme_minimal_hgrid() +
  theme(text = element_text(family = "Amita", colour = "#FAFAFA"),
        plot.title = element_text(face = "bold", size = 32, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 20, halign = 0.5, lineheight = 0.4),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 12),
        legend.position = "none",
        axis.text = element_text(colour = "#FAFAFA", size = 16),
        strip.text = element_text(face = "bold", size = 18),
        plot.margin = margin(5, 10, 2, 2),
        plot.background = element_rect(fill = colorspace::lighten("grey25", amount = 0.3), colour = colorspace::lighten("grey25", amount = 0.3)))


# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-08-29_FairUse/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")

