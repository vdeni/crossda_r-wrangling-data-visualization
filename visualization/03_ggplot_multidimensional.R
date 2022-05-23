# Here we will learn about more complex charts types and focus on visualizing the relationship betzween multiple variables on the same chart.

###########################
##### The R libraries #####
###########################

library(ggplot2)
library(RColorBrewer)

#####################
##### Bivariate #####
#####################

# stacked bar charts for two categorical variables (nominal, ordinal), summary charts

p01 + aes(fill=as.factor(numres))
p01<-p01 + aes(fill=as.factor(aparstat))
# Nono
p01 + geom_bar(position = "dodge")

p01a <- ggplot(dff,aes(as.factor(asex), fill=as.factor(aparstat)))
p01a + geom_bar(position = "dodge")
p01a + geom_bar(position = "fill")

p01 + geom_bar(stat="summary", position ="fill")

# ? heatmap two categorical variables (nominal, ordinal)

# multiple density plots
ggplot(dff, aes(x = dsag, fill=as.factor(bint_pr)))+
    geom_density(adjust=0.8, alpha = 0.3)

# multiple density plots
ggplot(dff, aes(x = ses, fill=as.factor(beduc)))+
    geom_density(adjust=0.7, alpha = 0.3)+
    scale_fill_brewer(palette="Set1")


# scatterplot and scatterplot matrix

# cor age-dsgr

# Line graph

colSums(is.na(select(dff, matches('^(a|b|c)(age)$'))))

# Boxplot

# for categories of one variable
# as summary across variables on comparable scales

# facet wrap grid



