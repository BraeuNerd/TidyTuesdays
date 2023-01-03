# TidyTuesdays Week 1: Bring Your Own Data!

#Data: Books I've read (those logged in my Goodreads library: https://www.goodreads.com/user/show/108234805-mar-a-braeuner)

# libraries --------------------------------

library(tidyverse)
library(showtext) # for specific fonts; font_add_google
library(ggtext) # package to interpret HTML with element_markdown to color titles
library(glue)
library(waffle)
library(patchwork)


# load data --------------------------------

goodreads_data <- read_csv("2023/2023_01_03_Books/read_braeunerd.csv")

# data wrangling ---------------------------

glimpse(goodreads_data)

# Remove vars I'm not interested in for this Viz:
books <- goodreads_data %>%
  select(c(birthplace, gender, genre))

#Fix data types:
books$birthplace <- as.factor(books$birthplace)
books$gender <- as.factor(books$gender)
books$genre <- as.factor(books$genre)
summary(books)

# VIZ --------------------------------------

countries <- books %>%
  group_by(birthplace) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = fct_reorder(birthplace, n), y = n)) +
  geom_col(fill = "#809848") +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Seems I've read way too many US and UK authors <span style='color:#809848;'>(by birthplace)</span>",
       subtitle = "And way too many male authors (gender according to wiki or goodreads bios)  
       <br>  
       <span style='color:#b05252;'>
       ''What an astonishing thing a book is. It is a flat object made from a tree with  
       flexible parts on which are imprinted lots of funny dark squiggles. But one  
       glance at it and you are inside the mind of another person, maybe somebody  
       dead for thousands of years. Across the millennia, an author is speaking  
       clearly and silently inside your head, directly to you. Writing is perhaps the  
       greatest of human inventions, binding together people who never knew each  
       other, citizens of distant epochs. Books break the shackles of time.  
       A book is proof that humans are capable of working magic.''  
       - Carl Sagan </span>",
#       caption = "DataViz: @braeuNERD",
       y = "Number of books") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        axis.line.y = element_line(color = "grey70", size = 0.5),
        axis.line.x = element_line(color = "grey70", size = 0.5),
        axis.ticks.y = element_line(color = "grey70", size = 0.1),
        axis.ticks.x = element_blank(),
        plot.title = element_markdown(color = "white", 
                                      size = 18, margin = margin(40,0,-50,20)),
        plot.subtitle = element_markdown(color = "white", 
                                         size = 11, margin = margin(50,0,-180,20)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color = "grey70", size = 10, margin = margin(0,20,20,0), angle = 90, hjust = 1, vjust = .1),
        axis.text.y = element_text(color = "grey70", size = 10),
        axis.title.y = element_text(color = "grey70"))

# Gender

books %>%
  group_by(gender) %>%
  summarize(n = n())

forgenderwaffle <- c(
  `Men authors (98)` = 98,
  `Women authors (29)` = 29,
  `Anonymous (1)` = 1
)

genderwaffle <- waffle(parts = forgenderwaffle, rows = 10, 
                 colors = c("grey50","white","grey70"))

gender <- genderwaffle + 
  theme_classic() +
  theme(legend.position = "right",
        text = element_text(color = "white"),
        rect = element_rect(fill = "#171717",
                            color = "#171717"),
        plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        strip.background = element_rect(fill = NA, color = NA),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank())

gender$layers[[1]]$aes_params$colour <- '#171717' # Trick to change waffle background color from here: https://stackoverflow.com/questions/64108314/how-to-change-color-of-tile-grout-in-r-waffle-plot-to-match-background
gender

# Genre

books %>%
  group_by(genre) %>%
  summarize(n = n())

forgenrewaffle <- c(
  `Fiction (59)` = 59,
  `Non fiction (66)` = 66,
  `Poetry (3)` = 3
)

genrewaffle <- waffle(parts = forgenrewaffle, rows = 10, 
                 colors = c("#809848","#b05252","#4f759b"))

genre <- genrewaffle + 
  theme_classic() +
  theme(legend.position = "left",
        text = element_text(color = "white"),
        rect = element_rect(fill = "#171717",
                            color = "#171717"),
        plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        strip.background = element_rect(fill = NA, color = NA),
        axis.title.x = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_blank())

genre$layers[[1]]$aes_params$colour <- '#171717'
genre


# Put the 3 Viz together:

countries / (gender+genre) +
  plot_layout(nrow=2, heights = c(3,1)) &
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) &
  plot_annotation(
    theme = theme(plot.background = element_rect(color = "#171717", size = 2, fill = "#171717"),
                  strip.background = element_rect(color = "#171717"))
  )



# save -----------------------------------

ggsave("2023/2023_01_03_Books/byod_goodreads.png", booksmap, dpi = 300, height = 9.9, width = 16.1)
