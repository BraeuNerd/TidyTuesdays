# TidyTuesdays Week 1: Bring Your Own Data!

#Data: Books I've read (those logged in my Goodreads library: https://www.goodreads.com/user/show/108234805-mar-a-braeuner)

# libraries --------------------------------

library(tidyverse)
library(scatterpie)
library(ggtext)

# load data --------------------------------

goodreads_data <- read_csv("2022/01/goodreads_library_export.csv")

# data wrangling ---------------------------

# Remove vars I'm not interested in for this Viz
books <- goodreads_data %>%
  select(-c(`Book Id`, `Author l-f`, ISBN, ISBN13, `Date Added`, Publisher, Binding, Bookshelves, `Bookshelves with positions`, `Read Count`))

# Fix data types:
# Exclusive Shelf has read/reading/to-read categories but is as chr, turn to factor first to filter
books$`Exclusive Shelf` <- as.factor(books$`Exclusive Shelf`)

# Date read as date
books$`Date Read` <- as.Date(books$`Date Read`, "%d-%m-%y")
summary(books)

#filter for Read books
read <- books %>% 
  filter(`Exclusive Shelf` == "read")

# For Map of authors I've read regarding their birthplace and gender:
## Add birthplace & gender column

# Birthplace 
read$birthplace <- c('United Kingdom', 'United States', 'United States', 'United Kingdom', 'United States',
'United States', 'United Kingdom', 'Czech Republic', 'United Kingdom', 'Canada',
'United Kingdom', 'Brasil', 'United States', 'Germany', 'United Kingdom',
'Canada', 'United States', 'Spain', 'United States', 'United States',
'United Kingdom', 'Guatemala','United States', 'Guatemala', 'Denmark',
'United Kingdom', 'Guatemala', 'Spain', 'Guatemala', 'Malaysia',
'Argentina', 'Brasil', 'United States', 'United States', 'Spain',
'United States', 'United States', 'United States', 'United Kingdom', 'Germany',
'United States', 'United States', 'United Kingdom', 'United States', 'United Kingdom',
'Germany', 'United Kingdom', 'United States', 'Uruguay', 'United Kingdom',
'United Kingdom', 'Spain', 'Spain', 'United States', 'United States',
'United States', 'Spain', 'Greece', 'United States', 'United States',
'United Kingdom', 'Canada', 'United States', 'Canada', 'United Kingdom',
'United States', 'Spain', 'Mexico', 'Colombia', 'Colombia',
'United States', 'United States', 'Colombia', 'United States', 'United States',
'Czech Republic', 'Germany', 'Czech Republic', 'United States', 'Colombia',
'United States', 'United States', 'Panama', 'Mexico', 'Chile',
'Argentina', 'Guatemala', 'Spain', 'Guatemala', 'Guatemala', 'Guatemala',
'Germany', 'Czech Republic', 'United Kingdom', 'United Kingdom', 'France',
'United States', 'United States', 'United Kingdom', 'United Kingdom', 'United Kingdom',
'United Kingdom', 'United Kingdom', 'United Kingdom', 'United States', 'United States',
'United States', 'United States', 'United States', 'United States', 'United States', 'Netherlands')

#Gender (according to their wikipedia pages and/or other online Bios.)
read$gender <- c('male', 'male', 'male', 'male', 'female',
'male', 'male', 'female', 'male', 'male',
'female', 'male', 'female', 'male', 'female',
'female', 'female', 'male', 'male', 'female',
'male', 'female', 'female', 'male', 'female',
'male', 'male', 'male', 'male', 'male',
'male', 'male', 'male', 'male', 'male',
'male', 'female', 'male', 'male', 'male',
'male', 'male', 'male', 'female', 'female',
'male', 'male', 'male', 'male', 'female',
'male', 'male', 'male', 'male', 'male',
'male', 'NA', 'male', 'female', 'male',
'female', 'female', 'male', 'female', 'male',
'male', 'male', 'male', 'male', 'male',
'male', 'male', 'male', 'male', 'male',
'male', 'male', 'male', 'male', 'male',
'male', 'male', 'male', 'male', 'female',
'male', 'male', 'male', 'male', 'male', 'male',
'female', 'male', 'female', 'male', 'male',
'male', 'male', 'male', 'male', 'male',
'male', 'male', 'female', 'female', 'male',
'male', 'male', 'male', 'male', 'male', 'male')

glimpse(read) #check data types
#fix data types
read$birthplace <- as.factor(read$birthplace)
read$gender <- as.factor(read$gender)

# VIZ --------------------------------------

#arrange data for scatterpies (spread gender into f/m cols) and add lat long for birthplaces
pies <- read %>%
  count(birthplace, gender) %>%
  spread(gender, n, fill=0) %>%
  mutate(Lat = as.numeric(case_when(
    birthplace=="United Kingdom" ~ "55.3781",
    birthplace=="United States" ~ "38.0902",
    birthplace=="Czech Republic" ~ "49.8175",
    birthplace=="Canada" ~ "54.1304",
    birthplace=="Brasil" ~ "-14.2350",
    birthplace=="Germany" ~ "51.1657",
    birthplace=="Spain" ~ "40.4637",
    birthplace=="Guatemala" ~ "15.7835",
    birthplace=="Denmark" ~ "56.2639",
    birthplace=="Malaysia" ~ "4.2105",
    birthplace=="Argentina" ~ "-30.4161",
    birthplace=="Uruguay" ~ "-32.5228",
    birthplace=="Greece" ~ "39.0742",
    birthplace=="Mexico" ~ "23.6345",
    birthplace=="Colombia" ~ "4.5709",
    birthplace=="Panama" ~ "8.5380",
    birthplace=="Chile" ~ "-35.6751",
    birthplace=="France" ~ "46.2276",
    birthplace=="Netherlands" ~ "52.1326")), 
    Lon = as.numeric(case_when(
      birthplace=="United Kingdom" ~ "-3.4360",
      birthplace=="United States" ~ "-95.7129",
      birthplace=="Czech Republic" ~ "15.4730",
      birthplace=="Canada" ~ "-106.3468",
      birthplace=="Brasil" ~ "-51.9253",
      birthplace=="Germany" ~ "10.4515",
      birthplace=="Spain" ~ "-3.7492",
      birthplace=="Guatemala" ~ "-90.2308",
      birthplace=="Denmark" ~ "9.5018",
      birthplace=="Malaysia" ~ "101.9758",
      birthplace=="Argentina" ~ "-63.6167",
      birthplace=="Uruguay" ~ "-55.7658",
      birthplace=="Greece" ~ "21.8243",
      birthplace=="Mexico" ~ "-102.5528",
      birthplace=="Colombia" ~ "-74.2973",
      birthplace=="Panama" ~ "-80.7821",
      birthplace=="Chile" ~ "-71.5430",
      birthplace=="France" ~ "2.2137",
      birthplace=="Netherlands" ~ "5.2913")))

#get map
worldmap <- map_data("world")

# calculate radius to use
pies$sum <- pies$male+pies$female+pies$`NA`
pies <- pies %>%
  mutate(radius = as.numeric(case_when(
  sum>25 ~ "10",
  sum >= 15 & sum<=25 ~ "8",
  sum <15 & sum>5 ~ "5",
  sum <=5 ~ "3")))

#values for subtitle:

sum(read$`Number of Pages`, na.rm = T) #29204
length(unique(read$birthplace)) # 19
nrow(read) #112
summary(read$gender) # female 24; male 87; NA 1
(87/(112)*100) #77.68%


# Viz  
booksmap

ggplot(worldmap) +
  geom_map(data = worldmap, map = worldmap, 
           aes(x = long,
               y = lat, 
               map_id=region),
           col="#54642f", fill = "#809848", alpha = 0.8) +
  geom_scatterpie(data = pies, 
                  aes(x = Lon,
                      y = Lat,
                      group=birthplace,
                      r=radius),
                  cols = c("male","female","NA")) + coord_equal() +
  scale_fill_manual(values = c(male = "#893168", female = "#efcefa", `NA` = "#809848")) +
  labs(title = "*Who are we reading?*",
       subtitle = "According to what I have logged in my **Goodreads** Shelf, I've read **112 books**,   \n  by authors from **19 countries** and mostly **(77.68%)** male.",
       caption = "(Gender according to the author's Wikipedia site; 1 NA = anonymous)   \n    
       Data: mostly Goodreads |  Viz: @braeuNERD   |   #TidyTuesday") +
  theme_minimal() +
  theme(plot.title = element_markdown(color = "#e9e5cd", size = 30, hjust = 0.05),
        plot.subtitle = element_markdown(color = "#e9e5cd", size = 14, hjust = 0.1),
        plot.caption = element_text(hjust = 0.5, color = "#e9e5cd", size = 12),
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        plot.background = element_rect(fill = "#4f759e"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(color = "#e9e5cd"),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(color = "white", size = 10, face = "bold"),
        strip.background = element_rect(color = "4f759e", fill = "#4f759e"))


# save -----------------------------------

ggsave("byod_goodreads.png", booksmap, dpi = 300, height = 9.9, width = 16.1)
