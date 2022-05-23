# Here we will learn about the basic use of ggplot2 package and the way in which
# the grammar of graphics is implemented.

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(RColorBrewer)

##########################################################
##### Selection of variables and preparation of data #####
##########################################################

# Let's first select a dozen variables from our merged dataset to have an interesting yet manageable set of variables to work with. We'll select some categorical variables like sex, education level, number of children and approximate category of household income. We want some continuous, quantitative variables to work with other than age, so we'll create some. There is a number of sets of items that share content and a scale. We can combine them to represent the theorized underlying construct. We would usually based this part on the empirical assessment of the scale's validity.

# Usually we would have some theoretical framework that should to a large extent dictate the choice of variables for visualization. We can also approach the data from a more exploratory position and use visualization tools accordingly.


# Descriptives: age|sex|nkids|parstat|educ|int_pr|n225
# Scales:
#   - 401 - perceived balance of investment in the household
#   - 408 - number of disagreements on various topics
#   - 721 - sadness in the past week
# ses: 1002|1009|119|1001_i|1001_j
# Satisfactions: 145|402|407


# TODO from long
dissagreements<- dplyr::select(data_waves_merged,
                    dplyr::matches('^(b)(408_)(a|b|c|d|e|f|i)'))
dissagreements$dsag<-rowMeans(dissagreements, na.rm = FALSE)
colSums(is.na(dissagreements))

ses<- dplyr::select(data_waves_merged,
                               dplyr::matches('^(a)(1002|1009|119|1001_i|1001_j)$'))


mutate(ses, a1001_i=recode(as.factor(a1001_i), '1'=1, .default=0))
mutate(ses, a1001_j=recode(as.factor(a1001_i), '1'=1, .default=0))

ses$ses<-rowMeans(scale(ses), na.rm = FALSE)




dff<-tibble(data_waves_merged,"dsag"=dissagreements$dsag,"ses"=ses$ses)

# Zakej ovo ne radi?
dplyr::summarise(dissagreements,
                 dplyr::across(dplyr::matches('^(a|b|c)'),
                               list('mean' = mean)))



df <- dplyr::select(data_waves_merged,
                dplyr::matches('^(a|b|c)(age|sex|educ|numres|407|370|b1009)$'))

df$a370

ggplot(df, aes(x=as.factor(bnumres))) +
    geom_bar(aes(fill=as.factor(bsex),))

dplyr::glimpse(df)
summary(df)

# Renaming
df <- dplyr::rename(df,
                   'a_birth_year' = 'abyear',
                   'b_birth_year' = 'bbyear',
                   'c_birth_year' = 'cbyear')

df <- dplyr::rename_with(df,
                        .cols = dplyr::matches('407'),
                        .fn = stringr::str_replace,
                        pattern = '407',
                        replacement = '')

hist(df$afloor, breaks=18)
summary(df$afloor)

#

# 407

##

#########################
##### ggplot basics #####
#########################

# R's built in graphical system

# Plots tab in Rstudio - scale, zoom
plot(rnorm(100), rnorm(100))
plot(rnorm(100), rnorm(100), col="orange")

### ggplot

# Empty canvas
ggplot()

# Define data
ggplot(data = dff)

# map aesthetically age in t1 on the x axis
ggplot(dff, aes(x=aage))

# map disagreements to y
ggplot(dff, aes(x=aage, y=dsag)) +
    geom_point()

# set graphical parameters: point size, alpha and position
ggplot(dff, aes(x=aage, y=dsag)) +
    geom_point(size=3, alpha=0.2, position="jitter")

# Same thing
ggplot(dff, aes(x=aage, y=dsag)) +
    geom_jitter(size=3, alpha=0.2)

# Add another layer - new geom
ggplot(dff, aes(x=aage, y=dsag, col=as.factor(asex))) +
    geom_jitter(size=3, alpha=0.3) +
    geom_smooth(method=lm)


#########################
##### Simple charts #####
#########################

### Frequency chart - Bar charts for categorical variables (nominal, ordinal)
#############################################################################

# Saving intermediate steps

p01<-ggplot(dff, aes(as.factor(asex)))

# Empty bars

p01
p01 + geom_bar()
p01

# Edit the values for the bars

p01 + geom_bar(
    stat = "count", # statistical transformation, based on What type of bar
    # chart do we have: raw, summaries?
    width = 0.4, # Bar width
    alpha = 0.7, # Bar alpha
    col="black", # Outline color
    fill = "red") # Fill color

# We will rarely explicity assign colors, we will use color as a tool
p01 <- p01 + geom_bar(
    aes(fill=as.factor(asex)), # Color based on values, new aesthetic mapping
    width = 0.4,
    alpha = 0.5)
p01

# The same thing:
p01<-ggplot(dff, aes(as.factor(asex), fill=as.factor(asex)))
p01 <- p01 + geom_bar(
    width = 0.4,
    alpha = 0.5)
p01

# Use color palettes
p01 + scale_fill_brewer(palette="Dark2")
p01 <- p01 + scale_fill_brewer(palette="Set1")
p01 + coord_flip()

# What if we have data in the form of a frequency table?

sumtab<-dplyr::summarise(group_by(dff, asex),
                         n = dplyr::n())
sumtab

# We use stat identity to tell ggplot to use actual values, not a summary
p02 <- ggplot(sumtab, aes(x = as.factor(asex), y = n))
p02 + geom_bar()
p02 + geom_bar(stat="identity") # keep original values

### Density and histogram for continuous variables or those with many values
############################################################################

# Map age at t1 to x
p03 <- ggplot(dff, aes(aage))
p03 + geom_histogram()

p03 + geom_histogram(
    bins = 10
)

p03 + geom_density(size=2,alpha = 0.4)

# Both on the same, chart, we need to match y scales:

p04 <- ggplot(dff, aes(aage))
p04 <- p04 + geom_histogram(
    aes(y = ..density..),
    bins=12)
p04 + geom_density()

### Linegraph
#############

sumtab<-dplyr::summarise(group_by(df, asex),
                         across(df),
                         list('mean' = mean,
                              'sd' = sd))

sumtab <- summarise(dff,
          dplyr::across(dplyr::matches('^(a|b|c)407$'),
                        list('mean' = mean)))
sumtab

p05<

### Ecercise:
# Plot a frequency chart for level of education
# Plot a density chart for disagreements, try to find the "adjust" that is illustrative

ggplot(dff, aes(x = dsag))+
    geom_density(adjust=0.1)

ggplot(dff, aes(x = dsag))+
    geom_histogram(bins=50)

