#### Data visualization workshop - NDS
#### Tania Alarcon Falconi
#### tania.alarcon_falconi@tufts.edu
#### Last update: 10/03/2018
#### Based on a FiveThirtyEight story: fivethirtyeight.com/features/you-cant-trust-what-you-read-about-nutrition/

### Set workspace -----------------------------------------------------------

rm(list = ls()) # Removes all objects 
dev.off()       # Deletes all figures
cat("\014")     # Clears the console

setwd('C://Users/Tania/Dropbox/DataViz/FiveThirtyEight/Nutrition_pHacking')

# Install and load required packages
# install.packages("readr")

library(readr)
library(dplyr)
library(ggplot2)

#library(tidyverse)


### Import data -------------------------------------------------------------

RawData <- read.csv("raw_anonymized_data.csv")


### Extract variable information --------------------------------------------

# FFQ variable names (should total 1066)

ffq <- names(RawData)[28:1093]

# Characteristic variable names (should total 26)

characteristics <- names(RawData)[2:27]


### Basics of ggplot --------------------------------------------------------

# One variable bar graph using the graphics package

plot(x = RawData$rash)

# One variable bar graph using the ggplot2 package

ggplot(data    = RawData,
       mapping = aes(x = rash)) +
  geom_bar()

# Save ggplot as an object         

RashPlot <- ggplot(data = RawData, mapping = aes(x = rash)) +
  geom_bar()

RashPlot


### Modify title and axis lables --------------------------------------------

# Add title, subtitle, and axes labels

RashPlot <- RashPlot + labs(y = "Counts",
                            x = "Rash in the past year",
                            title = "Bar graph - one variable",
                            subtitle = "Example using ggplot2")

RashPlot

# Change style of all title elements

RashPlot + theme(title = element_text(colour = "red"))

# Change style of the plot title

RashPlot + theme(plot.title = element_text(face = "bold", hjust = 0.5)) 

# Change style of a specific axis title

RashPlot + theme(axis.title.y = element_text(size = 16)) 

# Remove lables using element_blank()

RashPlot + theme(axis.title.x = element_blank())  

# Save object with updated title and axes labels 

RashPlot <- RashPlot + 
  theme(plot.title    = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 14),
        axis.title  = element_text(face = "bold", size = 12))

RashPlot


# Modify axes -------------------------------------------------------------

# Modify axis text

RashPlot + theme(axis.text.x = element_text(face = "italic", angle = 50))

# Add axis line

RashPlot + theme(axis.line = element_line(color = "black"))

# Format axis ticks 

RashPlot + theme(axis.ticks.x = element_blank(),
                 axis.ticks.y = element_line(size = 2, color = "blue"))

# Change limits of an axis 

RashPlot + scale_y_continuous(limits = c(0, 60))

# Change breaks and labels

RashPlot + scale_y_continuous(breaks = c(0, 10, 30, 50),
                              labels = c("Ten", "30.0", 50.0, 0))

# Flip the coordinates

RashPlot + coord_flip()

# Save object with updated axes 

RashPlot <- RashPlot + 
  scale_y_continuous(limits = c(0, 50), 
                     expand = c(0, 0),
                     breaks = seq(from = 10, to = 40, by = 10)) + 
  theme(axis.line = element_line(color = "black"))


### Modify background -------------------------------------------------------

# Use a built-in theme

RashPlot + theme_bw()

RashPlot + theme_minimal()

# Change the color of the panel

RashPlot + theme(panel.background = element_rect(fill = "pink"),
                 panel.border = element_rect(color = "black", fill = NA))

# Change the color of the plot background

RashPlot + theme(plot.background = element_rect(fill = "orange"))

# Modify grid lines 

RashPlot + theme(panel.grid.minor   = element_blank(),
                 panel.grid.major.x = element_blank(), 
                 panel.grid.major.y = element_line(linetype = 3, color = "black"))

# Save object with updated background 

RashPlot <- RashPlot + 
  theme(panel.background   = element_rect(fill = "white"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(linetype = 3, color = "black")
        )


### Modify bars -------------------------------------------------------------

# Change the color of the bars

RashPlot + geom_bar(color = "cyan", fill = "#4daf4a")

# Change the width of the bars 

RashPlot + geom_bar(color = "cyan", width = 1)

RashPlot + geom_bar(color = "cyan", width = 0.2)

# To avoid adding a layer, you need to re-draw the plot

ggplot(data    = RawData,
       mapping = aes(x = rash)) +
  geom_bar(width = 0.2)


### Plotting two discrete variables -----------------------------------------

RashPlot

# Plot rash stratified by cat ownership

ggplot(data    = RawData,
       mapping = aes(x = rash, fill = cat)) +
  geom_bar()

ggplot(data    = RawData,
       mapping = aes(x = rash, fill = cat)) +
  geom_bar(position = position_dodge())

# Plot rash stratified by cat ownership in panels

ggplot(data    = RawData,
       mapping = aes(x = rash, fill = cat)) +
  facet_grid(~cat) +
  geom_bar()

# Save plot

RashCatPlot <- ggplot(data    = RawData,
                      mapping = aes(x = rash, fill = cat)) +
  facet_grid(~cat) +
  geom_bar() + 
  labs(y        = "Counts",
       x        = "Rash in the past year",
       title    = "Bar graph - two discrete variables",
       subtitle = "Example using ggplot2") + 
  scale_y_continuous(limits = c(0, 30), 
                     expand = c(0, 0),
                     breaks = seq(from = 5, to = 25, by = 5)) + 
  scale_fill_discrete(name = "Own a cat") 


### Plotting a discrete and a continuous variable ---------------------------

RashPlot

# Plot rash versus soda consumption

ggplot(data    = RawData,
       mapping = aes(x = rash, y = SODAQUAN)) +
  geom_boxplot()

ggplot(data    = RawData,
       mapping = aes(x = rash, y = SODAQUAN)) +
  geom_boxplot() + 
  coord_flip()

# Save plot

RashSodaPlot <- ggplot(data    = RawData,
                       mapping = aes(x = rash, y = SODAQUAN)) +
  geom_boxplot() + 
  labs(y        = "Quantity of soda consumed (units)",
       x        = "Rash in the past year",
       title    = "Bar graph - one discrete and one continuous variable",
       subtitle = "Example using ggplot2") 


### Save theme and apply it to plots ----------------------------------------

# Create a custom theme
custom.theme <- list(theme(plot.title         = element_text(face = "bold", size = 18),
                         plot.subtitle      = element_text(size = 14),
                         axis.title         = element_text(face = "bold", size = 12),
                         panel.background   = element_blank(), 
                         panel.grid.major.y = element_line(linetype = 3, color = "black"),
                         panel.border       = element_rect(colour = "black", fill = NA)))

# Apply custom theme to plots

RashSodaPlot

RashSodaPlot <- RashSodaPlot + custom.theme


RashCatPlot                                     

RashCatPlot <- RashCatPlot + custom.theme

RashCatPlot <- RashCatPlot + 
  theme(strip.background = element_blank(),
        strip.text       = element_blank(), 
        legend.title     = element_text(face = "bold"), 
        panel.spacing    = unit(0, "lines"))
  
RashPlot
RashCatPlot                                     
RashSodaPlot

