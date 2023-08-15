# Tidy Tuesday - August 15, 2023 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(elementalist)

## fonts
font_add_google("Caprasimo", db_cache = FALSE)
font_add_google("Corben", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 33)

# Data Exploration ####
spam <- tuesdata$spam %>%
  rename(`Capital Letters` = crl.tot,
         `$` = dollar,
         `!` = bang,
         `000` = n000,
         `spam?` = yesno) %>%
  pivot_longer(cols = c(`Capital Letters`:make), names_to = "character") %>%
  mutate(`spam?` = case_when(`spam?` == "y" ~ "Yes", .default = "No"))

spamStats <- spam %>%
  group_by(character) %>%
  summarise(n = n(), average = mean(value)) %>%
  arrange(desc(average))

# Data Visualization ####

plotFinal <- spam %>%
  ggplot() +
  geom_boxplot(aes(x = value, y = `spam?`, colour = `spam?`, fill = `spam?`), outlier.alpha = 0.5,
               outlier.size = 0.5, show.legend = FALSE) +
  scale_colour_discrete(type = c("#9C686C", "#F8D17E")) +
  scale_fill_discrete(type = c("#9C686C", "#F8D17E")) +
  facet_wrap(~character, scales = "free_x") +
  scale_x_log10() +
  theme_void() +
  labs(x = "",
       y = "",
       title = "SPAM...Emails",
       subtitle = glue::glue("For the Week 33 #TidyTuesday data, this visualization explores the proprotion of emails that <span style='color:#F8D17E'>were reported as spam</span> containing certain characters and words to those that <span style='color:#9C686C'> were not reported as spam</span>. ",
                             "In this dataset, {spamStats$character[1]} were {format(spamStats$average[1]/spamStats$average[2], digits = 1)}x more likely to occur in an email than {spamStats$character[2]}."),
       caption = "Source: {Rdatasets} | #TidyTuesday | Week 33 | @hdailey") +
  theme(text = element_text(family = "Caprasimo", colour = "#FCDC1C", size = 18),
        plot.title = element_text(family = "Corben", face = "bold", hjust = 0.5, size = 48,
                                  margin = margin(t = 5, b = 2)),
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.3, size = 20,
                                                       margin = margin(b = 2)),
        strip.text = element_text(colour = "#FCDC1C", face = "bold", size = 20),
        axis.text.x = element_text(colour = "#FCDC1C"),
        plot.caption.position = "plot", 
        plot.caption = element_text(size = 12),
        panel.grid.major.x = element_line(colour = "black", linetype = 3),
        plot.margin = margin(0, 10, 3, 10),
        panel.background = element_rect_round(fill = "lightpink2", colour = "lightpink2"),
        plot.background = element_rect(fill = "#273752", colour = "#A3ABB7", linewidth = 2))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-08-15_Spam/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.5, dpi = 300,
       height = 8, width = 11, unit = "in")

