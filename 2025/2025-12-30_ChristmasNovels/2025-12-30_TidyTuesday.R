# Tidy Tuesday - December 30, 2025 ####

# libraries, fonts, data, etc ####
## libraries
library(tidyverse)
library(sysfonts)
library(showtext)
library(tidytext)
library(textdata)

## fonts
font_add_google("Asap Condensed", db_cache = FALSE)
font_add_google("Mystery Quest", db_cache= FALSE)
showtext_auto()

## data
tuesdata <- tidytuesdayR::tt_load(2025, week = 52)
novels <- tuesdata$christmas_novels
novelText <- tuesdata$christmas_novel_text
novelAuthors <- tuesdata$christmas_novel_authors

sentiment <- get_sentiments("afinn")


## functions
is_title <- function(x) {
  x == str_to_title(x)
}

is_caps <- function(x) {
  x == str_to_upper(x)
}

# Data Exploration ####
novelEdit <- novels %>%
  mutate(title = case_when(title == "Evenings at Donaldson Manor; Or, The Christmas Guest" ~ "Evenings at Donaldson Manor",
                           title == "The Abbot's Ghost, or Maurice Treherne's Temptation: A Christmas Story" ~ "The Abbot's Ghost",
                           title == "A Christmas Carol in Prose; Being a Ghost Story of Christmas" ~ "A Christmas Carol in Prose",
                           gutenberg_id == 18770 ~ "A New Way to Keep House",
                           gutenberg_id == 1902 ~ "A Christmas Romance of a Country Church",
                           gutenberg_id == 22665 ~ "Christian Gellert's Last Christmas",
                           gutenberg_id == 38983 ~ "The Christmas Adventure at Carver House",
                           gutenberg_id == 52935 ~ "Mr. Blake's Walking-Stick",
                           gutenberg_id == 20251 ~ "Christmas Comes but Once a Year...",
                           gutenberg_id == 23569 ~ "Christmas Holidays at Merryvale",
                          .default = title))

novelTextJoin <- novelText %>%
  left_join(novelEdit, by = "gutenberg_id", relationship = "many-to-many") %>%
  left_join(novelAuthors, by = "gutenberg_author_id", relationship = "many-to-many") %>%
  drop_na(text) %>%
  filter(!is_title(text)) %>%
  filter(!is_caps(text)) %>%
  separate_wider_delim(author, delim = ", ", names = c("lastName", "firstName")) %>%
  mutate(isAuthor = str_detect(text, lastName)) %>%
  filter(isAuthor == FALSE) %>%
  mutate(isChapter = str_detect(text, "Chapter")) %>%
  filter(isChapter == FALSE) %>%
  mutate(isFig = str_detect(text, "Illustration")) %>%
  filter(isFig == FALSE) %>%
  mutate(isStave = str_detect(text, "Stave")) %>%
  filter(isStave == FALSE)

sentimentText <- novelTextJoin %>%
  select(c(text, title)) %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stop_words$word)) 
  
sentimentAFINN <- sentimentText %>% 
  inner_join(sentiment) %>%
  group_by(title) %>%
  summarise(sentiment = sum(value)) %>%
  arrange(sentiment) %>%
  mutate(title = fct_inorder(title))

highSentiment <- sentimentAFINN %>%
  slice_max(sentiment) %>%
  mutate(gutenberg_author_id = 9034) %>%
  left_join(novelAuthors, by = "gutenberg_author_id") %>%
  separate_wider_delim(author, delim = ", ", names = c("lastName", "firstName")) %>%
  mutate(firstName = gsub("\\(.*", "", firstName)) %>%
  mutate(author = glue::glue("{firstName} {lastName}"))

lowSentiment <- sentimentAFINN %>%
  slice_min(sentiment) %>%
  mutate(gutenberg_author_id = 5282) %>%
  left_join(novelAuthors, by = "gutenberg_author_id") %>%
  separate_wider_delim(author, delim = ", ", names = c("lastName", "firstName")) %>%
  mutate(author = glue::glue("{firstName} {lastName}"))
  
## Plot
pal <- rep(c("#B70D00", "#DF8080", "#568d66", "#005C01"), 
           length.out = length(sentimentAFINN$title))

img_url = "https://t4.ftcdn.net/jpg/02/91/81/71/360_F_291817108_dC2kVOfh8JnEAQQYjWYL3ac1iBdlTICx.jpg"

plotFinal <- ggplot() +
  geom_col(data = sentimentAFINN, aes(x = title, y = sentiment, fill = title), width = 0.75) +
  ggtext::geom_textbox(aes(hjust = 0.5, x = length(sentimentAFINN$title)/2, y = max(sentimentAFINN$sentiment)-300, label = "Sentiment Analysis of Christmas Novels", text.colour = "#000000",
                           ),
                      family = "Mystery Quest", fill = NA, colour = NA, size = 20, width = unit (5, "in")) +
  ggtext::geom_textbox(aes(x = length(sentimentAFINN$title)/2, y = max(sentimentAFINN$sentiment) - 1000, text.colour = "#000000",
                           label = glue::glue("This week we explore christmas novels in Project Gutenberg. ",
                                              "Here, we've performed a sentiment analysis on the text of the {length(novels$title)} novels found in the gutenbergr package. ",
                                              "The novel with the highest sentiment score of {highSentiment$sentiment} is '{highSentiment$title}' by {highSentiment$author}. ",
                                              "Conversely, the novel with the lowest sentiment score of {lowSentiment$sentiment} is '{lowSentiment$title}' by {lowSentiment$author}."
                                              )),
                       family = "Mystery Quest", fill = NA, colour = NA, size = 10, width = unit (5, "in"), lineheight = 0.5) +
  labs(y = "Sentiment Score",
       caption = "Source: Project Gutenberg via gutenbergr package | #TidyTuesday | Week 52 | @hdailey") +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(values = pal) +
  theme_minimal() +
  theme(text = element_text(family = "Mystery Quest", size = 24, face = "bold"),
        plot.caption = element_text(size = 18, hjust = 0.5, margin = margin(t = -290)), 
        panel.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        axis.title.y = element_text(margin = margin(r = -25)),
        axis.text.y = element_text(size = 24),
        axis.text.x = element_text(hjust = 1, angle = 45),
        axis.title.x = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.1),
        legend.position = "none")

plotFinalBCKD <- ggimage::ggbackground(plotFinal, img_url)

# Save ###### 
ggsave(plot = plotFinalBCKD, path = here::here("2025/2025-12-30_ChristmasNovels"),
       paste0(format(Sys.Date(), "%Y-%m-%d"), "_TT", ".png"), height = 6, width = 10, units = "in", dpi = 300)

