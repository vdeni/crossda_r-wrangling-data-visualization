### 03 multidimensional ggplot

library(ggplot2)
library(GGally)
library(reshape2)

ggplot(dfw, aes(asex, fill = aparstat)) +
    geom_bar(position = "fill")

# Multiple densities
ggplot(dfw,aes(atask,fill=asex)) +
    geom_density(alpha = 0.3, adjust = 0.6)

# Scatterplot
ggplot(dfw[dfw$ases<2,], aes(ases, asad)) +
    geom_point(size = 3, alpha = 0.2 ) +
    geom_smooth(method = lm)

# Scatterplot 3d
ggplot(dfw, aes(aage, atask, col = asex)) +
    geom_point(size = 3, alpha = 0.3, position = "jitter") +
    geom_smooth(method = loess)

dfws<-dfw[dfw$aparstat!="no partner",]
dim(dfws)

ggplot(dfw[dfw$aparstat!="no partner",], aes(aage, atask, col = asex)) +
    geom_point(size = 3, alpha = 0.3, position = "jitter") +
    geom_smooth(method = loess)

### Linegraph

sumtab <- summarise(group_by(dfl, sex, wave),
                    mean = mean(sat_part, na.rm=T),
                    sd = sd(sat_part, na.rm=T))
sumtab

ggplot(sumtab, aes(wave, mean, group=sex)) +
    geom_line(aes(linetype = sex, col = sex), size = 2) +
    geom_point() +
    geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd, col = sex), size=1.5)

View(dfw)

# Boxplot
ggplot(dfw, aes(beduc, bdisagr)) +
    geom_point(alpha = 0.3, position = "jitter", aes(col = bsex, size = bsad)) +
    geom_boxplot(alpha = 0)

# Boxplot summary across multiple variables
d <- select(data_subset_long, matches("^disagr"))
d

md<-melt(d)
ggplot(md, aes(variable, value)) +
    geom_boxplot() +
    geom_point(position = "jitter", alpha = 0.1) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", col="red", size=2)

# Faceting
p01 + facet_wrap(~aparstat)

ggplot(dfl, aes(age,task,col=sex)) +
    geom_point(alhpa = 0.3) +
    geom_smooth() +
    facet_wrap(~wave)

ggplot(dfl, aes(ses, fill=sex)) +
    geom_density(adjust=0.6, alpha=0.3) +
    facet_wrap(parstat~wave)
