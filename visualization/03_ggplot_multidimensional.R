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
library(reshape2)

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
p01a + geom_bar(position = "dodge") # bars side by side
p01a + geom_bar(position = "fill") # bars streched to unity

# Use jitter to visualize this
ggplot(dfw,aes(asex, aparstat))+
    geom_jitter()

### multiple density plots - one continuous and one categorical variable
ggplot(dfw, aes(atask, fill=asex))+ # Use asex to fill the colors and define the groups
    geom_density(adjust=0.6, alpha = 0.3)

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
ggplot(dfw, aes(x=aage, y=atask, col=asex)) +
    geom_jitter(size=3, alpha=0.3) +
    geom_smooth(method=lm)

### Linegraph
#############

ggplot(dfl, aes(x=wave, y=sat_part)) +
    geom_line(stat = "summary")

# Let's make a summary table
dflg<-group_by(dfl, sex, wave)
sumtab<-summarise(dflg, mean = mean(sat_part, na.rm = TRUE),
                  sd = sd(sat_part, na.rm = TRUE))

ggplot(sumtab, aes(wave,mean, group=sex))+
    geom_line(aes(linetype = sex), size=2)+
    geom_point(size=5)+
    geom_errorbar(aes(wave, mean, ymin=mean-sd, ymax=mean+sd), size=1.3)

### Boxplot for categories on x
###############################

# Simple boxplot
ggplot(dfw, aes(x = beduc, y = bdisagr)) +
    geom_boxplot()
# Add points to visualize individual values
ggplot(dfw, aes(x = beduc, y = bdisagr)) +
    geom_boxplot(alpha = 0.4) + # if we set alpha to 0 we can loose the double points
    geom_jitter(alpha = 0.3, color = "red", aes(size = asad))
# Cram more information in!
ggplot(dfw, aes(x = beduc, y = bdisagr)) +
    geom_boxplot(alpha = 0.4) + # if we set alpha to 0 we can loose the double points
    geom_jitter(width = 0.25, heigth=0, alpha = 0.3, aes(size = asad, col=asex))
# Another boxplot
ggplot(dfw, aes(x=as.factor(ankids), y = ases, fill = asex)) +
    geom_boxplot(alpha = 0.4) +
    geom_jitter(width=0.1,alpha=0.2) +
    # geom_point(position=position_jitter(),alpha=0.3, aes(col=asex)) +
    scale_x_discrete(limits=c("0","1","2","3","4"))

###summary across variables on comparable scales
md<-melt(d[,-10])

ggplot(md, aes(variable, value)) +
    geom_jitter(alpha=0.05) +
    geom_boxplot(alpha= 0.3) +
    stat_summary(aes(group=1), fun = mean, geom = "line", col="red", size=2)

######################
##### Facet Wrap #####
######################


