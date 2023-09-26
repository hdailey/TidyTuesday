# Tidy Tuesday - September 26, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(cowplot)

## fonts
font_add_google("Roboto Slab", db_cache = FALSE)
font_add_google("Jost", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 39)

# Data Exploration ####
royData <- tuesdata$richmondway %>%
  mutate(Dating_flag = case_when(Dating_flag == "Yes" ~ "Dating", .default = "Neither"),
         Coaching_flag = case_when(Coaching_flag == "Yes" ~ "Coaching", .default = "Neither"),
         CoachDate_flag = case_when(Dating_flag == "Dating" & Coaching_flag == "Coaching" ~ "Coaching & Dating", .default = "Neither"),
         Season_Episode = str_remove_all(Season_Episode, "_"),
         Season_Episode = fct_inorder(Season_Episode)) %>%
  mutate(flag = case_when(Dating_flag == "Dating" & Coaching_flag == "Neither" ~ "Dating",
                          Dating_flag == "Neither" & Coaching_flag == "Coaching" ~ "Coaching",
                          Dating_flag == "Neither" & Coaching_flag == "Neither" ~ "Neither",
                          CoachDate_flag == "Coaching & Dating" ~ "Coaching & Dating", 
                          .default = "Neither")) %>%
  pivot_longer(F_count_RK:F_count_total) %>%
  mutate(Season = case_when(Season == 1 ~ "Season 1",
                            Season == 2 ~ "Season 2", 
                            .default = "Season 3"),
         Season = as.factor(Season))

royDataFScore <- tuesdata$richmondway %>%
  select(c(Season, Episode, F_count_RK, F_count_total)) %>%
  group_by(Season) %>%
  summarise(totalRK = sum(F_count_RK), total = sum(F_count_total))

logo <- magick::image_read(path = here::here("2023/2023-09-26_RoyKent/AFCRichmondLogo.png"))

# Data Visualization ####
plotFscore <- royData %>%
  ggplot(aes(x = Episode_order, y = F_score)) +
  geom_col(aes(group = Season, fill = Season), position = position_dodge()) +
  scale_fill_discrete(type = c("#EA0406", "#021E73", "#FFCC00")) +
  facet_wrap(~ Season, strip.position = "bottom", scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "",
       y = "F-Score",
       title = "Roy Kent F-Bombs",
       subtitle = glue::glue("Compiled by @deepshamenghani in the {{richmondway}} package, this weeks visualization ",
                             "explores the number of F*bombs and gestures in the TV series Ted Lasso through the F-Score metric. ", 
                             "An episodes F-Score is defined as the proportion of F*bombs from Roy Kent to the total F*bombs for the episode. ", 
                             "In Seasons 1, 2 and 3 Roy Kent had {royDataFScore$totalRK[1]}, {royDataFScore$totalRK[2]}, {royDataFScore$totalRK[3]} F*bombs, respectively. ", 
                             "Whereas, for Seasons 1, 2 and 3 the total F*bombs observed were {royDataFScore$total[1]},  {royDataFScore$total[2]},  {royDataFScore$total[3]}, respectively."),
       caption = "Source: {richmondway} package | #TidyTuesday | Week 39 | @hdailey") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Slab", size = 20, colour = "#000000"),
        legend.position = "none",
        plot.title = element_text(family = "Jost", face = "bold", size = 42),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.4, hjust = 0, width = 0.85, size = 22, margin = margin(t = 3, b = 10)),
        strip.text = element_text(face = "bold"), 
        plot.caption = element_text(size = 10, margin = margin(t = -10)),
        axis.text.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = -10)),
        axis.text.y = element_text(colour = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F7F6F4"),
        panel.background = element_rect(fill = "#F7F6F4"))

plotFinal <- ggdraw() + 
  draw_plot(plotFscore, x = 0, y = 0, height = 1) +
  draw_image(logo, x = 0.815, y = 0.78, width = 0.2, height = 0.2)

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-09-26_RoyKent/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")
