# Tidy Tuesday - October 24, 2023 ####
# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Noto Sans", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2023, week = 43)

# Data Exploration ####
patientRisk <- tuesdata$patient_risk_profiles
patientRisk <- patientRisk %>%
  select(c(`age group:  10 -  14`:`age group:  90 -  94`, `predicted risk of Pulmonary Embolism`:`predicted risk of  Treatment resistant depression (TRD)`)) %>%
  pivot_longer(cols = -c(`predicted risk of Pulmonary Embolism`:`predicted risk of  Treatment resistant depression (TRD)`), names_to = "ageGroup", values_to = "values") %>%
  filter(values == 1) %>%
  mutate(ageGroup = str_to_title(ageGroup),
         ageGroup = gsub("\\s+", " ", ageGroup)) %>%
  group_by(ageGroup) %>%
  reframe(n = n(), 
          meanRiskMS = mean(`predicted risk of Multiple Sclerosis`),
          meanRiskUC = mean(`predicted risk of Ulcerative colitis`),
          meanRiskM = mean(`predicted risk of Migraine`),
          meanRiskD = mean(`predicted risk of Dementia`),
          meanRiskDep = mean(`predicted risk of  Treatment resistant depression (TRD)`)) %>%
  mutate(ageGroup = factor(ageGroup, labels = c("Age Group: 0 - 4", "Age Group: 5 - 9", "Age Group: 10 - 14",
                                                "Age Group: 15 - 19", "Age Group: 20 - 24", "Age Group: 25 - 29",
                                                "Age Group: 30 - 34", "Age Group: 35 - 39", "Age Group: 40 - 44",
                                                "Age Group: 45 - 49", "Age Group: 50 - 54", "Age Group: 55 - 59",
                                                "Age Group: 60 - 64", "Age Group: 65 - 69", "Age Group: 70 - 74",
                                                "Age Group: 75 - 79", "Age Group: 80 - 84", "Age Group: 85 - 89",
                                                "Age Group: 90 - 94"))) %>%
  arrange(desc(ageGroup)) %>%
  pivot_longer(cols = c(meanRiskMS:meanRiskDep), names_to = "meanRiskCategory", values_to = "meanRisk")


# Data Visualization ####
plotFinal <- patientRisk %>%
  ggplot() +
  ggforce::geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 2*pi), size = 2, color = "#000000") +
  ggforce::geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 2*pi*meanRisk, colour = meanRiskCategory), size = 2) +
  scale_colour_discrete(type = wesanderson::wes_palette("Darjeeling1", n = 5)) +
  facet_wrap(~ageGroup, ncol = 4) +
  coord_equal() +
  theme_void() +
  labs(title = "Patient Risk by Age Group",
       subtitle = glue::glue("This week to celebrate the R/Pharma conference, we explore health outcome risks for 100 simulated patients.", 
                             "The average across one year predictibility risk of ", 
                             "<span style='color:#F98400;'><b>Multiple Sclerosis diagnosis,</b></span> ", 
                             "<span style='color:#5BBCD6;'><b>Ulcerative Colitis diagnosis,</b></span> ", 
                             "<span style='color:#F2AD00;'><b>Migrane Occurance,</b></span> ", 
                             "<span style='color:#FF0000;'><b>Dementia diagnosis</b></span> and ", 
                             "<span style='color:#00A08A;'><b>Treatment Resistant Depression diagnosis.</b></span>"),
       caption = "Source: @jreps | #TidyTuesday | Week 43 | Inspired by @curatedmess | @hdailey") +
  theme(text = element_text(family = "Noto Sans", size = 20),
        plot.title = element_text(face = "bold", size = 40, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.4, hjust = 0.5, 
                                                       margin = margin(t = 2, b = 2)),
        plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(b = 1)),
        plot.caption.position = "plot",
        axis.text.y = element_blank(),
        panel.spacing = unit(1, "lines"),
        strip.text = element_text(face = "bold", margin = margin(b = 2)),
        legend.position = "none",
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"))

# Save ####
ggsave(plot = plotFinal, path = here::here("2023/2023-10-24_PatientRiskProfiles/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), scale = 0.75, dpi = 300,
       height = 11, width = 8, unit = "in")