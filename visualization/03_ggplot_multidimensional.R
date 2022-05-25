# Here we will learn about more complex charts types and focus on visualizing
# the relationship between multiple variables on the same chart.

sub_data <- here::here('visualization',
                       '02_ggplot_basics.R')

source(sub_data)

###########################
##### The R libraries #####
###########################

library(ggplot2)

# ggplot "extension" Which comes with helper functions for
# producing some more complex charts
library(GGally)

# melt function - quick transform to long
library(reshape2) 

#####################
##### Bivariate #####
#####################

# Stacked bar charts for two categorical variables (nominal, ordinal)
p01

p01 <- p01 +
    ggplot2::aes(fill = aparstat)

p01

# Combining some geoms doesn't make sense
p01 +
    ggplot2::geom_bar(position = "dodge")

# Let's create a new plot
p01a <-
    ggplot2::ggplot(dfw,
                    ggplot2::aes(asex,
                                 fill = aparstat))

# bars side by side
p01a +
    ggplot2::geom_bar(position = "dodge")

# bars stretched to unity
p01a +
    ggplot2::geom_bar(position = "fill")

### multiple density plots - one continuous and one categorical variable
# Use asex to fill the colors and define the groups
ggplot2::ggplot(dfw,
                ggplot2::aes(atask,
                             fill = asex)) +
    ggplot2::geom_density(adjust = 0.6,
                          alpha = 0.3)

ggplot2::ggplot(subset(dfw,
                       !is.na(bsame_partner)),
                ggplot2::aes(adisagr,
                             fill = bsame_partner)) +
    ggplot2::geom_density(alpha = 0.3)

# Scatterplot - two continuous variables
ggplot2::ggplot(dfw,
                ggplot2::aes(ases,asad)) +
    ggplot2::geom_point(size = 3,
                        alpha = 0.2) +
    ggplot2::geom_smooth(method = lm)

# Remove outliers
ggplot(dfw[dfw$ases < 2, ],
       ggplot2::aes(ases,
                    asad)) +
    ggplot2::geom_point(size = 3,
                        alpha = 0.2) +
    ggplot2::geom_smooth(method = lm)

########################
##### Multivariate #####
########################

# Scatterplot with groups - combine two continuous and one categorical variable
# Define the color of the dot by sex, and define the groups in such way
p05 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(x = aage,
                                    y = atask,
                                    col = asex)) +
    ggplot2::geom_jitter(size = 3,
                         alpha = 0.3) +
    # geom_smooth(method = lm) + # Add OLS line | uncommet to make it work
    ggplot2::geom_smooth(method = loess) # add loess line

dfws <- dfw[dfw$aparstat != "no partner", ]

p06 <- ggplot2::ggplot(dfws,
                       ggplot2::aes(x = aage,
                                    y = atask,
                                    col = asex)) + # copy-paste
    ggplot2::geom_point() +
    # ggplot2::geom_jitter(size = 3, alpha = 0.3) +
    ggplot2::geom_smooth(method = loess)

#####################
##### Linegraph #####
#####################

# To create a line graph, we should first make a summary table
# grouped tibble
dflg <- dplyr::group_by(dfl,
                        sex,
                        wave)

# calculate means and sds for sex ~ wave combinations
sumtab <- dplyr::summarise(dflg,
                           'mean' = mean(sat_part,
                                       na.rm = TRUE),
                           'sd' = sd(sat_part,
                                     na.rm = TRUE))
# Plot lines
ggplot2::ggplot(sumtab,
                ggplot2::aes(x = wave,
                             y = mean,
                             group = sex)) +
    ggplot2::geom_line(aes(linetype = sex,
                           col = sex),
                       size = 2) +
    ggplot2::geom_point(size = 5) +
    ggplot2::geom_errorbar(ggplot2::aes(x = wave,
                                        y = mean,
                                        ymin = mean - sd,
                                        ymax = mean + sd,
                                        col = sex),
                           size = 1.3)

########################
##### Trajectories #####
########################
ggplot2::ggplot(dfl,
                ggplot2::aes(wave,
                             ses,
                             group = rid)) +
    ggplot2::geom_line()

ggplot2::ggplot(dfl[dfl$rid %in% sample(dfl$rid, 50), ],
                ggplot2::aes(wave,
                             ses,
                             group = rid)) +
    ggplot2::geom_line(aes(col = sex))


#######################################
##### Boxplot for categories on x #####
#######################################

# Simple boxplot
ggplot2::ggplot(dfw,
                ggplot2::aes(x = beduc,
                             y = bdisagr)) +
    ggplot2::geom_boxplot()

# Add points to visualize individual values
ggplot2::ggplot(dfw,
                ggplot2::aes(x = beduc,
                             y = bdisagr)) +
    # if we set alpha to 0 we can lose doubles
    ggplot2::geom_boxplot(alpha = 0.4) + 
    ggplot2::geom_jitter(alpha = 0.3,
                         color = "red",
                         ggplot2::aes(size = asad))

# Cram more information in!
ggplot2::ggplot(dfw,
                ggplot2::aes(x = beduc,
                             y = bdisagr)) +
    ggplot2::geom_boxplot(alpha = 0) +
    ggplot2::geom_jitter(width = 0.25,
                         heigth = 0,
                         alpha = 0.3,
                         ggplot2::aes(size = asad,
                                      col = asex))

# Another boxplot
ggplot2::ggplot(dfw,
                ggplot2::aes(x = as.factor(ankids),
                             y = ases,
                             fill = asex)) +
    ggplot2::geom_boxplot(alpha = 0.4) +
    ggplot2::labs(fill = "asex") +
    # ggplot2::geom_jitter(width = 0.1,
    #                      alpha = 0.2) +
    ggplot2::geom_point(position =
                            ggplot2::position_jitterdodge(jitter.width = 0.4),
                        alpha = 0.1,
                        ggplot2::aes(col = asex)) +
    ggplot2::scale_x_discrete(limits = c("0", "1", "2", "3", "4"))

#########################################################
##### summary across variables on comparable scales #####
#########################################################

d <- dplyr::select(data_subset_long,
                   dplyr::matches('^disagr_|rid'))

md <- melt(d)

ggplot2::ggplot(md,
                ggplot2::aes(variable,
                             value)) +
    ggplot2::geom_jitter(alpha = 0.05) +
    ggplot2::geom_boxplot(alpha = 0.3) +
    ggplot2::stat_summary(ggplot2::aes(group = 1),
                          fun = mean,
                          geom = "line",
                          col = "red",
                          size = 2)

######################
##### Facet Wrap #####
######################

# Faceting allows for the presentation of the same chart side by side for
# members from various categories defined by a grouping variable. Faceting is
# applicable to any chart and serves as a flexible framework for including
# multiple dimensions.

p01

p01 + ggplot2::facet_wrap(~ aparstat)

# Recreate the age-task chart with longitudinal data and facets
ggplot2::ggplot(dfl,
                ggplot2::aes(age,
                             task,
                             col = sex)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_smooth() +
    ggplot2::facet_wrap(~ wave)

# Facets as combinations of two categorical variables
ggplot2::ggplot(dfl,
                ggplot2::aes(ses,
                             fill = sex)) +
    ggplot2::geom_density(adjust = 0.6,
                          alpha = 0.3) +
    ggplot2::facet_wrap(parstat~wave)
