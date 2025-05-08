# Tidy Tuesday - March 04, 2025 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(sysfonts)
library(showtext)
library(lubridate)
library(ggtext)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Asap", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 9)
lbAnimals <- tuesdata$longbeach

# Data Exploration ####
lbAnimals_strays <- lbAnimals %>%
  filter(intake_type == "stray",
         outcome_type %in% c("adoption", "foster_to_adopt", "rescue", "foster"),
         animal_type == "dog") %>%
  mutate(intake_year = year(intake_date),
         intake_month = month(intake_date, label = TRUE),
         intake_day = day(intake_date)) %>%
  select(c(intake_date, intake_year, intake_month, intake_day))
  
lbAnimals_straysDate <- lbAnimals_strays %>%
  group_by(intake_date) %>%
  summarise(total = n())

lbAnimals_straysMonth <- lbAnimals_strays %>%
  group_by(intake_year, intake_month) %>%
  summarise(total = n()) %>%
  mutate(startDate = make_date(year = intake_year, month = intake_month, day = 01))

lbAnimals_straysYear <- lbAnimals_strays %>%
  group_by(intake_year) %>%
  summarise(total = n()) %>%
  mutate(startRect_x = glue::glue("{intake_year}", "-01-01"),
         startRect_x = as.Date(startRect_x),
         endRect_x = glue::glue("{intake_year}", "-12-31"),
         endRect_x = as.Date(endRect_x),
         startRect_y = 0,
         endRect_y = 125,
         textDate = glue::glue("{intake_year}", "-07-01"),
         textDate = as.Date(textDate))


# Data Visualization ####
plotFinal <- ggplot() +
  geom_rect(data = lbAnimals_straysYear,
            mapping = aes(xmin = startRect_x, 
                          xmax = endRect_x, 
                          ymin = startRect_y, 
                          ymax = endRect_y, 
                          fill = total),
            alpha = 0.8, show.legend = FALSE) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  geom_segment(data = lbAnimals_straysMonth,
               mapping = aes(x = startDate,
                             xend = startDate,
                             y = 0,
                             yend = total),
               linewidth = 0.1) +
  geom_point(data = lbAnimals_straysMonth,
             mapping = aes(x = startDate,
                           y = total),
             size = 2) +
  geom_text(data = lbAnimals_straysMonth,
            aes(x = startDate,
                y = total,
                label = total), color = "white", hjust = 0.5, fontface = "bold") +
  geom_textbox(data = lbAnimals_straysYear,
               aes(x = textDate,
                   y = 115,
                   label = glue::glue("**{intake_year}**","<br>", "{total} Dogs")),
               fill = NA, size = 8, lineheight = 0.25, hjust = 0.5, halign = 0.5, width = 0.1) +
  coord_cartesian(expand = TRUE) +
  labs(x = "",
       y = "Total Monthly Adopted Dogs",
       title = "2,961 Dogs Adopted in the City of Long Beach",
       subtitle = glue::glue("Since 2017, Long Beach ACS has adopted (e.g., adopted, fostered, foster to adopt, rescued) out over 2,900 dogs. ",
                             "Long Beach ACS provides care for a broad range of animals including dogs, but also reptiles, birds, cats, guinea pigs, and rabbits. ",
                             "Dog adoptions <span style='color:#1A9641'>**increase**</span> during the Spring/Summer months (e.g., school not being in session) with ",
                             "<span style='color:#D7191C'>**fewer**</span> adoptions observed during the Fall/Winter months."),
       caption = "Source: City of Long Beach ACS | #TidyTuesday | Week 9 | @hdailey inspired by @nrennie") +
  theme_void() +
  theme(text = element_text(family = "Asap Condensed"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(family = "Asap", face = "bold", size = 64),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(size = 32, lineheight = 0.3),
        plot.caption = element_text(size = 24),
        plot.caption.position = "plot",
        plot.margin = margin(2, 5, 2, 5))

# Save #####
ggsave(plot = plotFinal, path = here::here("2025/2025-03-04_LBAnimalShelter/"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), dpi = 300)

