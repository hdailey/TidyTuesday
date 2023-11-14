# Tidy Tuesday - November 14, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)
library(patchwork)
library(cowplot)

## fonts
font_add_google("Meow Script", db_cache = FALSE)
font_add_google("Roboto Condensed", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 46)

# Data Exploration ####
diwali <- tuesdata$diwali_sales_data %>%
  na.omit()

diwaliEDA_genderage <- diwali %>%
  group_by(`Age Group`, Gender) %>%
  summarise(averageAmountSpent = mean(Amount)) %>%
  mutate(`Age Group` = as.factor(`Age Group`))

diwaliEDA_state <- diwali %>%
  group_by(State) %>%
  summarise(averageAmountSpent = mean(Amount)) %>%
  arrange(desc(averageAmountSpent))

diwaliEDA_category <- diwali %>%
  group_by(Product_Category) %>%
  summarise(averageAmountSpent = mean(Amount)) %>%
  arrange(desc(averageAmountSpent))

diwaliEDA_agezones <- diwali %>%
  group_by(Zone, `Age Group`) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Data Visualization ####
pal <- c("#891E1B", "#AB2722", "#E53C26", "#F79E1F", "#97221E")
palzones <- c("#B63277", "#751F83", "#C85549", 
              "#E53C26", "#E27F32", "#EDBB4D", "#F5D58A")

plotGenderAge <- diwaliEDA_genderage %>%
  ggplot() +
  geom_col(aes(x = `Age Group`, y = averageAmountSpent, fill = Gender),
           position = position_dodge2()) +
  labs(x = "",
       y = "Average Spent (₹)") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "₹")) +
  scale_fill_manual(values = c("#E53C26", "#F79E1F")) +
  theme_cowplot() +
  theme(text = element_text(colour = "#F79E1F"),
        axis.text = element_text(colour = "#F79E1F"),
        axis.line = element_line(colour = "#F79E1F"),
        axis.ticks = element_line(colour = "#F79E1F"))

plotState <- diwaliEDA_state %>%
  ggplot() +
  geom_col(aes(x = averageAmountSpent, 
               y = reorder(State,averageAmountSpent), fill = State),
           show.legend = FALSE) +
  labs(x = "", 
       y = "") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "₹")) +
  scale_fill_manual(values = rep(pal, 4)) +
  theme_cowplot() +
  theme(text = element_text(colour = "#F79E1F"),
        axis.text = element_text(colour = "#F79E1F"),
        axis.line = element_line(colour = "#F79E1F"),
        axis.ticks = element_line(colour = "#F79E1F"))

plotCategories <- diwaliEDA_category %>%
  ggplot() +
  geom_col(aes(x = averageAmountSpent, y = reorder(Product_Category, averageAmountSpent),
               fill = Product_Category),
           show.legend = FALSE) +
  labs(x = "", 
       y = "") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "₹")) +
  scale_fill_manual(values = rep(pal, 4)) +
  theme_cowplot() +
  theme(text = element_text(colour = "#F79E1F"),
        axis.text = element_text(colour = "#F79E1F"),
        axis.line = element_line(colour = "#F79E1F"),
        axis.ticks = element_line(colour = "#F79E1F"))

plotAgeZones <- diwaliEDA_agezones %>%
  ggplot() +
  geom_col(aes(x = Zone, y = n, fill = `Age Group`),
           position = position_dodge2()) +
  labs(x = "",
       y = "Number of Individuals") +
  scale_fill_manual(values = palzones) +
  theme_cowplot() +
  theme(text = element_text(colour = "#F79E1F"),
        axis.text = element_text(colour = "#F79E1F"),
        axis.line = element_line(colour = "#F79E1F"),
        axis.ticks = element_line(colour = "#F79E1F"))

plotFinal <- plotGenderAge / (plotState + plotCategories) / (plotAgeZones) +
  plot_annotation(title = "Diwali Exploratory Data Analysis",
                  caption = "Source: Kaggle | #TidyTuesday | Week 46 | @hdailey") &
  theme(text = element_text(colour = "#F79E1F", family = "Roboto Condensed", size = 32),
        plot.title = element_text(family = "Meow Script", hjust = 0.5, 
                                  size = 72),
        axis.text = element_text(size = 30),
        axis.title.y = element_text(margin = margin(r = -10)),
        plot.caption = element_text(hjust = 0.5, size = 20, colour = "grey45"),
        plot.background = element_rect(fill = colorspace::lighten("grey5", amount = 0.1),
                                       colour = colorspace::lighten("grey5", amount = 0.1)))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-11-14_Diwali/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300,
       height = 8, width = 11, unit = "in")
