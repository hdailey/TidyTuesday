# Tidy Tuesday - February 13, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Roboto", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 7)

# Data Exploration ####
gifts_age <- tuesdata$gifts_age %>%
  pivot_longer(cols = c(SpendingCelebrating:GiftCards), names_to = "Category") %>%
  filter(Category != "SpendingCelebrating") %>%
  mutate(Category = case_when(Category == "GreetingCards" ~ "Greeting Cards",
                              Category == "EveningOut" ~ "Evening Out",
                              Category == "GiftCards" ~ "Gift Cards",
                              .default = Category))
  
# Data Visualization ####
plotFinal <- gifts_age %>%
  ggplot(aes(x = Age, y = Category, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), size = 6) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Valentine's Day Spending by Age and Category",
       subtitle = glue::glue("This week we explore Valentine's Day Survey Data from the United States National Retail Federation. ",
                             "Visualized below is the average amount by age group spent on seven categories from 2010 through 2022. ",
                             "In general, almost all of these categories obseved a decrease in the average amount spent by age group, ",
                             "except for Greeting Cards where people appear to spend more on Greeting Cards for Valentine's Day as they age."),
       caption = "Source: US National Retail Federation | #TidyTuesday | Week 8 | @hdailey",
       fill = "Average Spent ($)") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(text = element_text(size = 24),
        plot.title = element_text(family = "Roboto", face = "bold", size = 40),
        plot.title.position = "plot", 
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.4, size = 20, margin = margin(b = 5)),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, face = "bold", hjust = 1),
        axis.text.y = element_text(face = "bold"),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.text = element_text(size = 18, margin = margin(t = -5)),
        legend.title = element_text(margin = margin(t = -5, b = -5)), 
        plot.caption = element_text(hjust = 0.5, size = 12, colour = "grey65"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, barheight = 0.5, 
                               frame.colour = "black", ticks.colour = "black"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-02-13_ValentinesDay/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)

