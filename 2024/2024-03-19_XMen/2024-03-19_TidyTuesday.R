# Tidy Tuesday - March 19, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(showtext)

## fonts
font_add_google("Marvel", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 12)

# Data Exploration ####
mutantMoneyball <- tuesdata$mutant_moneyball %>%
  filter(TotalIssues60s != 0) %>%
  mutate(Member = as.factor(Member)) %>%
  select(c(Member, TotalValue60s_heritage:TotalValue90s_ebay, TotalValue60s_wiz:TotalValue90s_oStreet)) %>%
  mutate(TotalValue60s_wiz = parse_number(TotalValue60s_wiz),
         TotalValue70s_wiz = parse_number(TotalValue70s_wiz),
         TotalValue80s_wiz = parse_number(TotalValue80s_wiz),
         TotalValue90s_wiz = parse_number(TotalValue90s_wiz),
         TotalValue60s_oStreet = parse_number(TotalValue60s_oStreet),
         TotalValue70s_oStreet = parse_number(TotalValue70s_oStreet),
         TotalValue80s_oStreet = parse_number(TotalValue80s_oStreet),
         TotalValue90s_oStreet = parse_number(TotalValue90s_oStreet)) %>%
  pivot_longer(cols = c(TotalValue60s_heritage:TotalValue90s_oStreet)) %>%
  mutate(Character = case_when(Member == "warrenWorthington" ~ "Angel/Archangel",
                               Member == "hankMcCoy" ~ "Beast",
                               Member == "scottSummers" ~ "Cyclops",
                               Member == "bobbyDrake" ~ "Iceman",
                               Member == "jeanGrey" ~ "Marvel Girl/Phoenix",
                               Member == "alexSummers" ~ "Havok",
                               Member == "lornaDane" ~ "Polaris",
                               Member == "seanCassidy" ~ "Banshee",
                               Member == "ericMagnus" ~ "Magneto",
                               Member == "charlesXavier" ~ "Professor X")) %>%
  mutate(Character = as.factor(Character)) %>%
  filter(Character %in% c("Professor X", "Cyclops", "Iceman", "Beast", "Angel/Archangel", "Marvel Girl/Phoenix")) %>%
  separate_wider_delim(cols = name,
                       delim = "_",
                       names_sep = "-") %>%
  mutate(`name-2` = case_when(`name-2` == "heritage" ~ "Heritage Auctions",
                              `name-2` == "ebay" ~ "eBay",
                              `name-2` == "oStreet" ~ "Overstreet Price Guide",
                              `name-2` == "wiz" ~ "Wizard Price Guide")) %>%
  mutate(Year = case_when(`name-1` == "TotalValue60s" ~ 1960,
                          `name-1` == "TotalValue70s" ~ 1970,
                          `name-1` == "TotalValue80s" ~ 1980,
                          `name-1` == "TotalValue90s" ~ 1990)) %>%
  filter(Year %in% c(1960, 1990))

lineData <- mutantMoneyball %>%
  group_by(Character, `name-2`) %>%
  summarise(min = min(value),
            max = max(value))

# Data Visualization ####
plotFinal <- mutantMoneyball %>%
  ggplot() +
  geom_segment(data = lineData,
               mapping = aes(x = min, xend = max, y = Character, yend = Character), colour = "grey95",
               size = 0.1) +
  geom_point(data = mutantMoneyball %>% filter(Year == 1960),
             mapping = aes(x = value, y = Character), colour = "#f1c232", size = 1) +
  geom_point(data = mutantMoneyball %>% filter(Year == 1990),
             mapping = aes(x = value, y = Character), colour = "#660000", size = 1) +
  scale_x_continuous(labels = scales::dollar_format()) + 
  coord_cartesian(clip = "off", expand = TRUE) +
  labs(title = "The Original X-Men Valuation (1960-1990)",
       subtitle = glue::glue("This week we explore the Rally X-Men Mutant dataset by applying the methods from <I> Moneyball </I> by Michael Lewis.",
                             "Here, we are looking at the valuation of the total number of issues the original X-Men Characters appeared in starting in ",
                             "<span style='color:#f1c232'>1960</span> and compared to the valuation in <span style='color:#660000'>1990</span>. ",
                             "Over time, all original characters have had a decrease in valuation in 1990 as compared to the valuation of the original X-Men comics published during the 1960s."),
       caption = "Source: Rally's Mutant Moneyball | TidyTuesday | Week 12 | @hdailey") +
  facet_wrap(~`name-2`, scales = "free_x", nrow = 2, ncol = 2) +
  ggthemes::theme_fivethirtyeight() +
  theme(text = element_text(family = "Marvel", colour = "grey75", size = 24),
        plot.title = element_text(face = "bold", size = 32),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.5, size = 18),
        plot.caption = element_text(size = 12, margin = margin(t = 5, b = -10)),
        plot.background = element_rect(fill = colorspace::lighten("#052542")),
        panel.background = element_rect(fill = colorspace::lighten("#052542")),
        strip.background = element_rect(fill = colorspace::lighten("#052542")),
        strip.text = element_text(face = "bold", size = 24),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.1))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-03-19_XMen/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)