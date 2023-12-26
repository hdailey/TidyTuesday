# Tidy Tuesday - December 26, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(cowplot)
library(lubridate)

## fonts
font_add_google("Lexend Deca", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 52)

# Data Exploration ####
cran <- tuesdata$cran_20221122
internal <- tuesdata$internal_calls
external <- tuesdata$external_calls

cranLOC <- cran %>%
  select(c(package, date, license, files_R, loc_R)) %>%
  filter(loc_R != 0)

cranLOC_label_max <- cranLOC %>%
  arrange(desc(loc_R)) %>%
  slice_head(n = 1)

cranLOC_label_other <- cranLOC %>%
  filter(package == "RGtk2.10")

# Data Visualization ####
plotFinal <- cranLOC %>%
  ggplot(aes(x = date, y = loc_R)) +
  geom_line(colour = "dodgerblue", linewidth = 0.25) +
  ggrepel::geom_label_repel(data = cranLOC_label_max, aes(label = package), family = "Lexend Deca", size = 18, segment.colour = "grey55", segment.size = 0.25, nudge_x = -50000000, seed = 1234) +
  ggrepel::geom_label_repel(data = cranLOC_label_other, aes(label = package), family = "Lexend Deca", size = 18, segment.colour = "grey55", segment.size = 0.25, nudge_y = 5000, seed = 1234) +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma_format()) +
  labs(x = "",
       y = "Total Lines of Code",
       title = "Lines of Code Over the Years",
       subtitle = "The final dataset for TidyTuesday 2023 explores R packages. The dataset comes from Mark Padgham and Noam Ross where they used {pkgstats} to analyze the structure of R packages over time. This visualization explores the total number of lines of code in R packages over time.",
       caption = "Source: Padgham & Ross (2022) and {pkgstats} | #TidyTuesday | Week 52 | @hdailey") +
  theme_half_open(font_family = "Lexend Deca", font_size = 32) +
  theme(plot.title = element_text(face = "bold", margin = margin(t = 2, b = 2), size = 64),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, size = 40, margin = margin(t = 2, b = 5)),
        plot.caption = element_text(colour = "grey55", hjust = 0.5, margin = margin(t = -2, b = -2)),
        plot.background = element_rect(fill = "grey95", colour = "grey95"),
        panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.2, colour = "grey55"),
        axis.line = element_line(colour = "grey20"))
  
# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-12-26_RPackageStructure/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
