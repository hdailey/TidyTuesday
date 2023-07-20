# Tidy Tuesday - July 20, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(ggridges)

## fonts
font_add_google("Cabin")
font_add_google("Cabin Condensed")
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 29)

# Data Exploration ####
detectors <- tuesdata$detectors 

human_written <- detectors %>%
  mutate(human_or_AI = case_when(.pred_class == "Human" & kind == "Human" ~ 1, .default = 0)) %>%
  filter(name %in% c("Real TOEFL", "US 8th grade essay", "Fake TOEFL - GPT4 - PE", "US 8th grade essay - GPT simplify"))

# Data Visualization ####
plotFinal <- human_written %>%
  ggplot(aes(y = detector, x = .pred_AI, fill = detector)) +
  geom_density_ridges(scale = 2) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent) +
  scale_fill_brewer(palette = "BuPu", direction = -1) +
  facet_wrap(~factor(name, levels = c("Real TOEFL", "US 8th grade essay", 
                                      "Fake TOEFL - GPT4 - PE", "US 8th grade essay - GPT simplify"),
                     labels = c("Real TOEFL", "Real US 8th Grade", "Enhanced TOEFL", "Simplified US 8th Grade")),
             nrow = 2) +
  labs(x = "", 
       y = "",
       title = "GPT Detectors and Bias Against Non-Native English Writers",
       subtitle = paste0("This visualization explores the data from Simon Couch's {detectors} package which is based on ",
                         "the pre-print of GPT Detectors Are Biased Against Non-Native English Writers, arXiv: 2304.02819. ",
                         "When examining TOEFL (Test of English as a Foreign Language) essay detection data, ",
                         "the TOEFL essays were more frequently identified as 'AI-generated' as compared to their US 8th Grade Essay counterparts. ",
                          "To more appropriately emulate the native-speaker use, TOEFL essays were enhanced using ChatGPT which found a reduction ",
                          "in TOEFL essays identified as 'AI-generated'. ",
                          "In contrast, when simplifying the US 8th Grade essay to mirror non-native writing, the essays were more frequently classified as 'AI-generated'."),
       caption = "Source: Simon Couch's {detectors} Package | #TidyTuesday | Week 29") +
  theme_ridges() +
  theme(text = element_text(family = "Cabin Condensed"),
        plot.title.position = "plot",
        plot.title = element_text(family = "Cabin", hjust = 0.5, face = "bold", size = 64),
        plot.subtitle = ggtext::element_textbox_simple(hjust = 0.5, halign = 0.5, lineheight = 0.5, size = 36),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = "bold", size = 32, margin = margin(t = 10)),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(-0.5, "lines"),
        axis.text.y = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(size = 28), 
        panel.grid.major = element_line(colour = "grey75"),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1, size = 26),
        legend.position = "none",
        panel.background = element_rect(fill = "grey95", colour = "grey95"),
        plot.background = element_rect(fill = "grey95", colour = "grey95"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-07-18_GPTDetection"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 8, width = 11, unit = "in")
