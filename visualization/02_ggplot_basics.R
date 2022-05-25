# Here we will learn about the basic use of {ggplot2} package and the way in
# which the grammar of graphics is implemented.

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(here)
library(psych)

##########################################################
##### Selection of variables and preparation of data #####
##########################################################

sub_data <- here::here('wrangling',
                       '04_prepare-visualization-data.R')

source(sub_data)

# We will be using our subsetted long data set to have an interesting yet
# manageable set of variables to work with. We have some categorical variables
# like sex, education level, number of children and approximate category of
# household income. We want some continuous, quantitative variables to work with
# other than age, so we'll create some. There is a number of sets of items that
# share content and a scale. We can combine them to represent the hypothetical
# underlying construct. We would usually base this part on the formal
# measurement model and empirical assessment of the scale's validity.

# Usually we would have some theoretical framework that should to a large extent
# dictate the choice of variables for visualization. We can also approach the
# data from a more exploratory position and use visualization tools accordingly.

################################
##### Forming scale scores #####
################################

# Frequency of disagreements in various areas
# Assemble to own objects to ease handling

d <- dplyr::select(data_subset_long,
                   dplyr::matches('^disagr_'))

d

# Form a total score as mean rating
d$disagr <- rowMeans(d,
                     na.rm = TRUE)

d

# Evaluate number of missing by row

colSums(is.na(d))

# And by column

table(rowSums(is.na(d)))

# Balance in household tasks, 1 - I work much more, 3 - balanced, 5 - partner
# works much more

d_t <- dplyr::select(data_subset_long,
                     dplyr::matches('^task_'))

d_t$task <- rowMeans(d_t,
                     na.rm = TRUE)

# Felt sad or depressed in the last week

s <- dplyr::select(data_subset_long,
                   dplyr::matches('^sad_'))

s$sad <- rowMeans(s,
                  na.rm = TRUE)

# Ad hoc custom index of SES
ses <- select(data_subset_long,
              n_rooms,
              hh_ends_meet,
              hh_income,
              hh_2_car,
              hh_2_home)

# Show levels as numerci values
ses <- dplyr::mutate_if(ses,
                        .predicate = is.factor,
                        .funs = as.numeric)

ses

# Invert yes-no into binary variable
ses$hh_2_car <- (ses$hh_2_car - 2) * -1

ses$hh_2_home <- (ses$hh_2_home - 2) * -1

ses

# Calculate SES index as average value across standardized variables
ses$ses <- rowMeans(scale(ses),
                    na.rm = TRUE)

# Create a new long data frame for use in ggplot
dfl <- dplyr::tibble(data_subset_long,
                     "disagr" = d$disagr,
                     "task" = d_t$task,
                     "sad" = s$sad,
                     "ses" = ses$ses)

# Amount of missing data
mis <- rowSums(is.na(dfl))

# Plot using R base graphics
hist(mis)

psych::describe(dfl)

# Create a matching wide data frame
dfw <- tidyr::pivot_wider(dfl,
                          id_cols = 'rid',
                          names_from = 'wave',
                          values_from = !matches('rid|wave'),
                          names_glue = '{wave}{.value}')

# We will use the data frames with short names to make the ggplot code more
# readable

#########################
##### ggplot basics #####
#########################

# R's built in graphical system

# Plots tab in Rstudio - scale, zoom
plot(dfw$aage,
     dfw$adisagr)

# Empty canvas
ggplot2::ggplot()

# Define data
ggplot2::ggplot(data = dfw)

# map aesthetically age in t1 on the x axis
ggplot2::ggplot(dfw,
                ggplot2::aes(x = aage))

# Draw on the canvas
ggplot2::ggplot(dfw,
                ggplot2::aes(x = aage)) +
    # draw on the canvas
    ggplot2::geom_vline(xintercept = 50) +
    # modify the scale of the x axis
    ggplot2::xlim(10,
                  100) +
    ggplot2::geom_vline(xintercept = 50,
                        color = "blue",
                        size = 2)

# map disagreements to y
ggplot2::ggplot(dfw,
                ggplot2::aes(x = aage,
                             y = adisagr)) +
    ggplot2::geom_point()

# set graphical parameters: point size, alpha and position
ggplot2::ggplot(dfw,
                ggplot2::aes(x = aage,
                             y = adisagr)) +
    ggplot2::geom_point(size = 3,
                        alpha = 0.2,
                        position = "jitter")

# Same thing
ggplot2::ggplot(dfw,
                ggplot2::aes(x = aage,
                             y = adisagr)) +
    ggplot2::geom_jitter(size = 3,
                         alpha = 0.2)

# Add another layer - new geom
ggplot2::ggplot(dfw,
                ggplot2::aes(x = aage,
                             y = adisagr)) +
    ggplot2::geom_jitter(size = 3,
                         alpha = 0.3) +
    ggplot2::geom_smooth(method = lm,
                         size = 2,
                         col = "red")


#########################
##### Simple charts #####
#########################

####################### Frequency chart ###########################
##### Bar charts for categorical variables (nominal, ordinal) #####
###################################################################

# Saving intermediate steps

p01 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(asex))

# Empty bars

p01

p01 +
    ggplot2::geom_bar()

# Edit the values for the bars

p01 +
    ggplot2::geom_bar(# statistical transformation, based on What type of bar
                      # chart do we have: raw, summaries?
                      stat = "count",
                      width = 0.4, # Bar width
                      alpha = 0.7, # Bar alpha
                      col = "black", # Outline color
                      fill = "red") # Fill color

# We will rarely explicity assign colors, we will use color as a tool
p01 <- p01 +
    ggplot2::geom_bar(# Color based on values, new aesthetic mapping
                      ggplot2::aes(fill = asex),
                      width = 0.4,
                      alpha = 0.5)

p01

# The same thing:
p01 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(asex,
                                    fill = asex))

p01 <- p01 +
    ggplot2::geom_bar(width = 0.4,
                      alpha = 0.5)

p01

# What if we have data in the form of a frequency table?
# Set grouping b<y sex and calculate subsample sizes
sumtab <- dplyr::summarise(dplyr::group_by(dfw,
                                           asex),
                           n = dplyr::n())

sumtab

# We use stat identity to tell ggplot to use actual values, not a summary
p02 <- ggplot2::ggplot(sumtab,
                       ggplot2::aes(x = as.factor(asex),
                                    y = n))

p02 +
    ggplot2::geom_bar(stat = "identity") # keep original values

######################################
##### Density plot and histogram #####
######################################

# Map age at t1 to x
p03 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(aage))

p03 +
    ggplot2::geom_histogram()

# Histogram
p03 +
    geom_histogram(bins = 14,
                   col = "black")

# Density plot
p03 +
    ggplot2::geom_density(adjust = 0.3,
                          size = 2)

# Both on the same, chart, we need to match y scales:
p04 <- ggplot2::ggplot(dfw,
                       ggplot2::aes(aage))

p04 <- p04 +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                            bins = 14,
                            col = "black")

p04 +
    ggplot2::geom_density(adjust = 0.3,
                          size = 2)

#####################
##### Exercise: #####
#####################

# Plot a frequency chart for level of education

# Plot a density chart for disagreements, explore the variable with "adjust"
