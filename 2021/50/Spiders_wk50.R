library(tidyverse)
library(tidytuesdayR)
library(viridis)
library(maps)
library(ggthemes)

# Get data

list <- tt_load("2021-12-07")
spiders_df <- list$spiders

# Split those who are distributed in several countries/regions into a row by country:

spiders <- separate_rows(spiders_df, distribution, sep = ",")
spiders$distribution <- str_trim(spiders$distribution, side = c("both"))

#NOTE: Austrlia's regions were in parenthesis and might now be split wrong or confusing - I'm not using Australia but jic.

#List & filter continental LatAm countries
#NOTE some are written Brasil/Brazil, Guiana/Guyana, French Guiana/French Guyana
#NOTE some are written as e.g. "Guatemala to Colombia"
continental_LatAm <- c("Mexico","Guatemala",
                       "Belize", "El Salvador",
                       "Honduras","Nicaragua",
                       "Costa Rica","Panama",
                       "Argentina","Bolivia",
                       "Brazil","Chile",
                       "Colombia","Ecuador",
                       "Guyana", "Guiana", "French Guiana", "French Guyana",
                       "Paraguay","Peru",
                       "Suriname","Uruguay","Venezuela")

LatAm_spiders <- filter(spiders,
                        distribution %in% continental_LatAm)


#substitute all "Guiana" to "Guyana" and all "French Guyana" to "Frengh Guiana" so they match with the names from maps library
LatAm_spiders$distribution <- as.factor(LatAm_spiders$distribution) #easier to substitute factor labels
##rename to match maps
LatAm_spiders <- LatAm_spiders %>%
  rename(region = distribution)

levels(LatAm_spiders$region) <- sub("Guiana","Guyana",levels(LatAm_spiders$region))
levels(LatAm_spiders$region) <- sub("French Guyana","French Guiana",levels(LatAm_spiders$region))

unique(LatAm_spiders$region) #just checking


#### Plotting starts here ####

# Spider species descriptions in continental Latin America by decade

# maps
#remember: on map_data Brazil with "z", Guyana and French Guiana.
america <- map_data("world", region = continental_LatAm)

formap <- LatAm_spiders %>%
  mutate(decade = year - year%%10) %>%
  group_by(decade, region) %>%
  summarise(n = n())

#check range for subtitle:
range(LatAm_spiders$year)

america %>%
  merge(formap, by = "region", all.x=T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat,
             group = group, 
             fill = n)) +
  geom_polygon(color = "gray50") +
  facet_wrap(~decade, ncol=7) +
  scale_fill_viridis(" ", na.value = "#f5f5f2", alpha = 0.8) +
  theme_tufte() +
  labs(title = "Number of spider species described in continental LatAm by decade",
       subtitle = "(from 1757 - 2021)",
       caption = "Data: World Spider Catalog   |   Viz: @braeuNERD   |   #TidyTuesday") +
  theme(legend.position = "top",
        plot.background = element_rect(fill = "#171717"),
        plot.title = element_text(color = "#809848", size = 18),
        plot.subtitle = element_text(color = "#B05252"),
        plot.caption = element_text(hjust = 0.5, color = "grey90"),
        axis.text = element_text(color = "grey90"),
        axis.text.x = element_text(color = "grey90"),
        axis.text.y = element_text(color = "grey90"),
        axis.title = element_blank(),
        legend.text = element_text(color = "grey80"),
        panel.grid = element_blank(),
        strip.text = element_text(color = "white", size = 10, face = "bold"),
        strip.background = element_rect(color = "black", fill = "#4f759b"))

#Other stuff I tried;

#LatAm_spiders %>%
#  mutate(decade = year - year%%10) %>%
#  group_by(CS, region, decade, year) %>%
#  summarise(n = n()) %>%
#  ggplot() +
#  geom_col(mapping = aes(x = year, 
#                         y = n,
#                         fill = n))

#LatAm_spiders %>%
#  mutate(decade = year - year%%10) %>%
#  group_by(CS, region, decade, year) %>%
#  summarise(n = n()) %>%
#  ggplot() +
#  geom_col(mapping = aes(x = year, 
#                         y = n,
#                         fill = n)) +
#  facet_wrap(~region, nrow = 3, scales = "free") +
#  scale_fill_viridis("Number of species described") +
#  theme_bw() +
#  theme(legend.position = "bottom")
  