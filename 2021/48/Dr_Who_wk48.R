library(tidyverse)
library(datardis)
library(igraph)
#library(ggraph)
#library(scales)

#change colnames in writers & directors df's for merging for nodes (useful later)
writers$code <- "w" #just to check
colnames(writers) <- c("story_number","name","code")
directors$code <- "d" #just to check
colnames(directors) <- c("story_number","name","code")

#merge
who <- merge(directors, writers, by = "story_number")
drwho <- merge(who, episodes, by = "story_number")

#filter for just highest rating (plotting all got too messy, stick to rating>85 for now)
drwhohigh <- drwho %>%
  filter(rating>85)

glimpse(drwhohigh)
#restore names on big df
colnames(drwhohigh)[2] <- "directors"
colnames(drwhohigh)[4] <- "writers"

#check 
summary(drwhohigh)
length(drwhohigh$episode_number) #66
unique(drwhohigh$director) #24 (54 tot)
unique(drwhohigh$writer) #18 (36 tot)

#matrix for network plot
## edges matrix: [to, from, attr, attr, attr...]
reduced <- data.frame(drwhohigh$director, drwhohigh$writer, drwhohigh$rating)

#conditionals for aesthetics (assign ratings to lower values that make sense for lty & label size)
edges <- reduced %>%
  mutate(RatingSize = if_else(
    drwhohigh.rating <= 87, 1, if_else(
      drwhohigh.rating <= 89, 3, 8)
    ))

#create graph object
network <- edges %>%
  graph_from_data_frame(directed = FALSE) %>%
  set_vertex_attr(name = "type", value = names(V(.)) %in% edges$director)

network

#Function to modify label positions later in circled layout:
## I took this function from this post: https://www.r-bloggers.com/2011/02/aligning-labels-in-circular-igraph-layouts/
rad.scale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2*pi), range(x)))
}

lab.locs <- rad.scale(x=1:length(V(network)), direction=-1, start=0)

#drwho Color Palette: "#7126bd", "#00203c", "#003b6f", "#6f8ea9", "#a6b8c7"

#assign attributes (the rest are easier within the plot code)
E(network)$color[E(network)$RatingSize == 1] <- "#7126bd"
E(network)$color[E(network)$RatingSize == 3] <- "#003b6f"
E(network)$color[E(network)$RatingSize == 8] <- "#a6b8c7"
E(network)$lty[E(network)$RatingSize == 1] <- 1
E(network)$lty[E(network)$RatingSize == 3] <- 3
E(network)$lty[E(network)$RatingSize == 8] <- 1

l <- layout_in_circle(network)

# Make the plot
par(bg="#00060b") #background color
plot(network,
     vertex.color="#6f8ea9",
     vertex.shape="sphere",
     vertex.size=10,
     vertex.label.color=c("white"),
     vertex.label.font=2,
     edge.width=E(network)$RatingSize,
     vertex.label.cex=1,
     vertex.label.dist=0.75,
     vertex.label.degree=lab.locs,
     layout=l)
title(main = "Dr.Who Director-Writer Duos & their episode rating", cex.main=1.5, col.main="#a6b8c7")
mtext("only depicting episodes rated >85 (66 episodes)", side=3, cex.main=1, col="#6f8ea9")
# Add a legend
legend("bottomleft", 
       legend = c("Rating <87","Rating [87,88]","Rating >89"),
       col=c("#7126bd","#003b6f","#a6b8c7"),
       lty=c(1,3,1),
       lwd=c(2,3,8),
       bg="#6f8ea9",
       bty="o",
       cex=1,
       horiz=F,
       box.lwd=0,
       text.col = "white")

legend("bottomright", 
       legend = "Data: datardis \n Viz: @BraeuNERD \n #TidyTuesday",
       bty="n",
       text.col = "white")

### END ###


### FAILED ATTEMPT with the edgebundleR package: ###

#using the same edges data.frame as before and creating a new nodes df
library(edgebundleR)

nodes <- rbind(writers,directors)
nodes <- nodes[!duplicated(nodes$name), ]
nodes <- select(nodes, -1)

network2 <- graph_from_data_frame(edges, directed = FALSE, vertices=nodes)

par(bg="#00060b")
V(network2)$color <- c("6f8ea9", "7126bd")
edgebundle(network2)

