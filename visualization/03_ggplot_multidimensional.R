# Here we will learn about more complex charts types and focus on visualizing
# the relationship between multiple variables on the same chart.

sub_data <- here::here('visualization',
                       '02_ggplot_basics.R')
source(sub_data)

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(GGally) # ggplot "extension" Which comes with helper functions for
# producing some more complex charts
library(reshape2) # melt function - quick transform to long

#####################
##### Bivariate #####
#####################

### Stacked bar charts for two categorical variables (nominal, ordinal)
p01
p01<-p01 + aes(fill=aparstat)
p01
# Combining some geoms doesn't make sense
p01 + geom_bar(position = "dodge")

# Let's create a new plot
p01a <- ggplot(dfw,aes(asex, fill=aparstat))
# bars side by side
p01a + geom_bar(position = "dodge")
# bars streched to unity
p01a + geom_bar(position = "fill")

### multiple density plots - one continuous and one categorical variable
# Use asex to fill the colors and define the groups
ggplot(dfw, aes(atask, fill=asex)) +
    geom_density(adjust=0.6, alpha = 0.3)

ggplot(subset(dfw, !is.na(bsame_partner)), aes(adisagr, fill=bsame_partner)) +
    geom_density(alpha=0.3)

### Scatterplot - two continuous variables
ggplot(dfw, aes(ases,asad))+
    geom_point(size=3,alpha=0.2)+
    geom_smooth(method=lm)

### Remove outliars
ggplot(dfw[dfw$ases<2,], aes(ases,asad))+
    geom_point(size=3,alpha=0.2)+
    geom_smooth(method=lm)

########################
##### Multivariate #####
########################

### Satterplot with groups - combine two continuous and one categorical variable
# Define the color of the dot by sex, and define the groups in such way
p05<-ggplot(dfw, aes(x=aage, y=atask, col=asex)) +
    geom_jitter(size=3, alpha=0.3) +
    #geom_smooth(method=lm) + # Add OLS line
    geom_smooth(method=loess) # add loess line

dfws<-dfw[dfw$aparstat!="no partner",]

p06<-ggplot(dfws, aes(x=aage, y=atask, col=asex)) + # copy-paste
    geom_point() +
    # geom_jitter(size=3, alpha=0.3) +
    geom_smooth(method=loess)

### Linegraph
#############

# To create a line graph, we should first make a summary table
# grouped tibble
dflg<-group_by(dfl, sex, wave)
# calculate means and sds for sex ~ wave combinations
sumtab<-summarise(dflg, mean = mean(sat_part, na.rm = TRUE),
                  sd = sd(sat_part, na.rm = TRUE))
# Plot lines
ggplot(sumtab, aes(wave,mean, group=sex))+
    geom_line(aes(linetype = sex, col=sex), size=2)+
    geom_point(size=5)+
    geom_errorbar(aes(wave, mean, ymin=mean-sd, ymax=mean+sd, col=sex),
                  size=1.3)

### Trajectories
################
ggplot(dfl, aes(wave,ses, group = rid)) +
    geom_line()

ggplot(dfl[dfl$rid %in% sample(dfl$rid,50),], aes(wave, ses, group = rid)) +
    geom_line(aes(col=sex))


### Boxplot for categories on x
###############################

# Simple boxplot
ggplot(dfw, aes(x = beduc, y = bdisagr)) +
    geom_boxplot()
# Add points to visualize individual values
ggplot(dfw, aes(x = beduc, y = bdisagr)) +
    geom_boxplot(alpha = 0.4) + # if we set alpha to 0 we can loose doubles
    geom_jitter(alpha = 0.3, color = "red", aes(size = asad))
# Cram more information in!
ggplot(dfw, aes(x = beduc, y = bdisagr)) +
    geom_boxplot(alpha = 0) +
    geom_jitter(width = 0.25, heigth=0, alpha = 0.3, aes(size = asad, col=asex))
# Another boxplot
ggplot(dfw, aes(x=as.factor(ankids), y = ases, fill = asex)) +
    geom_boxplot(alpha = 0.4) +
    labs(fill = "asex") +
    # geom_jitter(width=0.1,alpha=0.2) +
    geom_point(position=position_jitterdodge(jitter.width = 0.4), alpha=0.1,
               aes(col=asex)) +
    scale_x_discrete(limits=c("0","1","2","3","4"))

### summary across variables on comparable scales
d <- select(data_subset_long, matches('^(disagr_)'))
md<-melt(d)

ggplot(md, aes(variable, value)) +
    geom_jitter(alpha = 0.05) +
    geom_boxplot(alpha = 0.3) +
    stat_summary(aes(group=1), fun = mean, geom = "line", col="red", size=2)

######################
##### Facet Wrap #####
######################

# Faceting allows for the presentation of the same chart side by side for
# members from various categories defined by a grouping variable. Faceting is
# applicable to any chart and serves as a flexible framework for including
# multiple dimensions.

p01
p01 + facet_wrap(~aparstat)

# Recreate the age-task chart with longitudinal data and facets
ggplot(dfl, aes(age, task, col=sex)) +
    geom_point(alpha = 0.3) +
    geom_smooth() +
    facet_wrap(~wave)

# Facets as combinations of two categorical variaables
ggplot(dfl, aes(ses, fill=sex)) +
    geom_density(adjust = 0.6, alpha = 0.3) +
    facet_wrap(parstat~wave)
