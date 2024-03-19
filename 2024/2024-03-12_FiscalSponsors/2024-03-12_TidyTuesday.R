# Tidy Tuesday - February 13, 2024 ####

# libraries, fonts, data ####
## libraries
library(tidyverse)
library(tidytext)
library(stopwords)
library(spacyr)
library(showtext)

## fonts
font_add_google("Marcellus", db_cache = FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2024, week = 11)

# Data Exploration ####
# Load Stopwords
stopwords = stopwords()

# Load Language Model
spacy_initialize(model = "en_core_web_sm")

fiscalDirectory <- tuesdata$fiscal_sponsor_directory %>%
  tidyr::separate_wider_delim(cols = services,
                              delim = "|",
                              names_sep = "_",
                              too_few = "align_end") %>%
  pivot_longer(cols = c(services_1:services_16),
               names_to = "names",
               values_to = "serviceType") %>%
  na.omit() %>%
  select(c(name, year_501c3, year_fiscal_sponsor, n_sponsored, serviceType)) %>%
  mutate(serviceType = str_to_lower(serviceType))

fiscalWords <- fiscalDirectory %>%
  unnest_tokens(input = serviceType, output = wordTokens, token = "words") %>%
  filter(!grepl('[0-9]', wordTokens)) %>%
  filter(!wordTokens %in% stopwords)

serviceType_count <- fiscalWords %>% 
  count(wordTokens, sort = TRUE)

serviceType_nameCount <- fiscalWords %>%
  count(name, wordTokens, sort = TRUE)
 
text <- fiscalWords$wordTokens

fiscalWords_parced <- spacy_parse(text, lemma = FALSE, entity = TRUE, nounphase = TRUE) %>%
  group_by(token, pos) %>%
  summarise(n = n()) %>%
  filter(pos == "VERB") %>%
  arrange(desc(n))

fiscalWords_slice <- head(fiscalWords_parced, 10) %>%
  mutate(token = str_to_title(token)) %>%
  arrange(n) %>%
  mutate(token = as.factor(token))

# Data Visualization ####
pal <- wesanderson::wes_palette("BottleRocket2")

plotFinal <- fiscalWords_slice %>%
  ggplot(aes(x = n, y = reorder(token, n))) +
  geom_point(colour = pal[2], stroke = 1.5, alpha = 1) +
  geom_point(colour = pal[1], alpha = 1) +
  scale_x_continuous(breaks = seq(0, 200, by = 50), limits = c(0, 200)) +
  scale_y_discrete(expand = c(0.05, 0.05)) +
  coord_cartesian(clip = "off", expand = TRUE) +
  labs(x = "Count",
       y = "",
       title = "Fiscal Sponsors Top 10 Service Type Verbs",
       subtitle = glue::glue("The National Network of Fiscal Sponsors defines a fiscal sponsor as: ",
                             "'Fiscal sponsors are nonprofits that enable the movement of money ",
                             "from funders to projects, ideas, organizations, and activities.' ",
                             "This visualization ",
                             "uses the Spacyr and its Small Core English model to detect verbs ",
                             "in the service type category of the dataset."),
       caption = "Source: Fiscal Sponsor Directory | #TidyTuesday | Week 11 | @hdailey") +
  theme_minimal(base_family = "Marcellus") +
  theme(text = element_text(colour = "#FFFFFF"),
        plot.title = element_text(face = "bold", size = 32, margin = margin(t = 2, b = 2)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_textbox_simple(lineheight = 0.5, size = 14, margin = margin(t = 2, b = 5)),
        plot.caption = element_text(size = 8),
        plot.caption.position = "plot",
        axis.text = element_text(colour = "#FFFFFF", size = 18),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(size = 16),
        panel.grid.major = element_line(linetype = "dotted", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = colorspace::lighten("grey15", 0.2),
                                       colour = colorspace::lighten("grey15", 0.2)),
        panel.border = element_rect(colour = "#FFFFFF", fill = NA, linewidth = 0.1))

# Save ####
ggsave(plot = plotFinal, path = here::here("2024/2024-03-12_FiscalSponsors/"),
        paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), units = "in", dpi = 300)

