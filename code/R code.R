# TidyTuesday 03/11/2020 - IKEA Furniture

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

library(tidyverse)
library(RColorBrewer)


data <- ikea %>%
  group_by(category, designer) %>%
  tally() %>%
  ungroup()


data <- data %>%
  slice(-1:-143) %>%
  filter(!designer == "IKEA of Sweden")


data$designer <- str_replace_all(data$designer, "/IKEA of Sweden", "")
data$designer <- str_replace_all(data$designer, "IKEA of Sweden/", "")


dt <- data %>%
  arrange(desc(n)) %>%
  filter(n>15)


dt <- mutate_at(dt, vars(category), as.factor)
dt <- mutate_at(dt, vars(designer), as.factor)
glimpse(dt)


# Start by setting the plot 
# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(dt))
colnames(to_add) <- colnames(dt)
dt <- rbind(dt, to_add)
dt$id <- seq(1, nrow(dt))

# Get the name and the y position of each label
label_data <- dt
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# palette
pal <- brewer.pal(11, "Spectral")


# Plotting
p <- ggplot(dt)+
  geom_bar(aes(x=as.factor(id), y=n, fill=category), stat="identity") +
  scale_fill_manual(values=pal)+
  ylim(-50,90) +
  theme_bw() + 
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4,), "cm")) +
  annotate(geom = "text",
           x=0,y=-50,
           hjust=.5, vjust=1,
           label="IKEA most popular\n designers",
           size=4.3, lineheight=.8,
           family="Staatliches Regular",
           color="black") +
  annotate(geom = "text",
           x=0,y=200,
           vjust=1,
           hjust=.5,
           label = "Data:  Kaggle & TidyTuesday | Graphic: Cristina Cametti - @CameCry",
           size=2.5,
           color="black") +
  coord_polar(start = 0)  +
  geom_text(data=label_data, aes(x=id, y=n+1, label=designer, hjust=hjust), color="black", 
            fontface="bold",alpha=0.6, size=3.4, angle=label_data$angle, inherit.aes = FALSE ) 

p
