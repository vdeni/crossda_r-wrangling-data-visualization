# Here we will learn about some of the most important ways one can customize
# the ggplot chart for various purposes

sub_data <- here::here('visualization',
                       '02_ggplot_basics.R')

source(sub_data)

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(cowplot)

# Changing titles, axes lables and legend title - working with themes

###########################
##### Barplot example #####
###########################

p07 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(ahh_income,
                                    fill = asex)) +
    ggplot2::geom_bar(position = "dodge") +
    ggplot2::labs(title = "Frequency of household income groups by sex",
                  x = "Household income",
                  y = "Count") +
    ggplot2::labs(fill = "Sex") +
    ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(size = 20)) +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

p07

# Full control over text
p07 +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 19,
                                                      color = "firebrick",
                                                      face = "italic"))

# Change the position of the legend
p07 +
    ggplot2::theme(legend.position = "top")

# Adding values as text to graph
p07 + ggplot2::geom_text(stat = 'count',
                         ggplot2::aes(label = ..count..),
                         position = ggplot2::position_dodge(0.9),
                         size = 5,
                         vjust = -0.7)
# Flip coordinates
p07 +
    ggplot2::coord_flip()

# Colors management
# https://r-graph-gallery.com/38-rcolorbrewers-palettes_files/figure-html/thecode-1.png

# Use color palettes
p01

p01 +
    ggplot2::scale_fill_brewer(palette = "Dark2")

p01 <- p01 +
    ggplot2::scale_fill_brewer(palette = "Set1")

p01

# Change tick marks
# Scatterplot

p08 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(aage,
                                    ases,
                                    col = asex)) +
    ggplot2::geom_point(size = 2,
                        alpha = 0.2) +
    ggplot2::geom_smooth(method = lm)

p08

p08 <- p08 +
    ggplot2::scale_x_continuous(limits = c(18, 79),
                                breaks = seq(18, 79,
                                             by = 2))

p08 +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12))

# Change the focus
ggplot2::ggplot(dfl,
                aes(n_rooms,
                    fill = sex)) +
    ggplot2::geom_density(alpha = 0.4,
                          adjust = 1) +
    ggplot2::scale_x_continuous(limits = c(1, 10),
                                breaks = seq(1:10))

table(dfl$n_rooms)

# Label individual points
p08 +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(ases > 2,
                                                   as.character(rid),
                                                   '')))

# Saving ggplot objects
ggplot2::ggsave("ses-age.pdf",
                height = 8,
                width = 12)

# Two plot side by side
pa <- ggplot2::ggplot(subset(dfw,
                             !is.na(bsame_partner)),
                      ggplot2::aes(ases,
                                   fill = bsame_partner)) +
    ggplot2::geom_density(size = 0.4,
                          adjust = 0.3,
                          alpha = 0.3)

pb <- ggplot2::ggplot(subset(dfw,
                             !is.na(bsame_partner)),
                      ggplot2::aes(bses,
                                   fill = bsame_partner)) +
    ggplot2::geom_density(size = 0.4,
                          adjust = 0.3,
                          alpha = 0.3)

cowplot::plot_grid(pa,
                   pb,
                   labels = c("a", "b"),
                   ncol = 2,
                   nrow = 1)

# Saving ggplot objects
ggplot2::ggsave("sidebyside.pdf",
                height = 8,
                width = 16)

# Neat trick:
# Base plot style and variables of interest
pst <- ggplot2::ggplot(dfw,
                       ggplot2::aes(col = beduc,
                                    fill = beduc)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(size = 20))

# Plot multiple graphs on the same template
pst +
    ggplot2::geom_histogram(aes(bses))

pst +
    ggplot2::geom_point(aes(bdisagr,
                            bses))
