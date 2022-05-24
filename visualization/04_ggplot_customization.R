# Here we will learn about some of the most important ways one can customize
# the ggplot chart for various purposes

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)

# Changing titles, axes lables and legend title - working with themes
### Barplot example
p07<-ggplot(dfw, aes(ahh_income, fill=asex)) +
    geom_bar(position="dodge") +
    labs(title="Frequency of household income groups by sex",
         x ="Household income",
         y = "Count") +
    labs(fill = "Sex") +
    theme_minimal() +
    theme(title = element_text(size = 20)) +
    theme(axis.text = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 90))
p07

# Full control over text
p07 +  theme(axis.title = element_text(size = 19, color = "firebrick",
                                   face = "italic"))
# Change the position of the legend
p07 + theme(legend.position="top")

# Adding values as text to graph
p07 + geom_text(stat='count', aes(label = ..count..), position =
                    position_dodge(.9), size=5, vjust=-0.7)
# Flip coordinates
p07 + coord_flip()

# Colors management
# https://r-graph-gallery.com/38-rcolorbrewers-palettes_files/figure-html/thecode-1.png

# Use color palettes
p01
p01 + scale_fill_brewer(palette = "Dark2")
p01 <- p01 + scale_fill_brewer(palette = "Set1")
p01

p07
# Change tick marks
# Scatterplot
p08<-ggplot(dfw, aes(aage, ases, col = asex)) +
    geom_point(size = 2, alpha = 0.2) +
    geom_smooth(method = lm)
p08
p08 <- p08 + scale_x_continuous(limits = c(18, 79),
                                breaks = seq(18, 79, by = 2))

p08 + theme(axis.text = element_text(size = 12))

# Change the focus
ggplot(dfl, aes(n_rooms, fill = sex)) +
    geom_density(alpha = 0.4, adjust = 1) +
    scale_x_continuous(limits = c(1, 10), breaks = seq(1:10))

table(dfl$n_rooms)

# Label individual points
p08 +
geom_text(aes(label = ifelse(ases > 2, as.character(rid),'')))

# Saving ggplot objects
ggsave("ses-age.pdf", height = 8, width = 12)

# Two plot side by side
pa<-ggplot(subset(dfw, !is.na(bsame_partner)), aes(ases, fill=bsame_partner)) +
    geom_density(size = 0.4, adjust = 0.3, alpha = 0.3)

pb<-ggplot(subset(dfw, !is.na(bsame_partner)), aes(bses, fill=bsame_partner)) +
    geom_density(size = 0.4, adjust = 0.3, alpha = 0.3)

plot_grid(pa, pb, labels=c("a", "b"), ncol = 2, nrow = 1)

# Saving ggplot objects
ggsave("sidebyside.pdf", height = 8, width = 16)

# Neat trick:
# Base plot style and variables of interest
pst<-ggplot(dfw, aes(col = beduc,fill = beduc)) +
    theme_minimal() +
    theme(title = element_text(size = 20))
# Plot multiple graphs on the same template
pst + geom_histogram(aes(bses))
pst + geom_point(aes(bdisagr,bses))




