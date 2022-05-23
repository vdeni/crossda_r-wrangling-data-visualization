# Here we will learn about some of the most important ways one can customize the ggplot chart to suit various purposes

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(RColorBrewer)
library(gridExtra)


# Colors management

# Use color palettes
p01 + scale_fill_brewer(palette="Dark2")
p01 <- p01 + scale_fill_brewer(palette="Set1")
p01 + coord_flip()

# Palettes

##  X,Y lables, labs(x = "Year", y = "Temperature (°F)")
## using themes   theme(axis.text = element_text(color = "dodgerblue", size = 12),
#axis.text.x = element_text(face = "italic"))

# Publishing

#  scale_y_continuous(limits = c(128, 166), breaks = seq(126, 166, by = 5)) +
# scale_x_continuous(limits = c(2013, 2030), breaks = seq(2013, 2030, by = 1)) +
