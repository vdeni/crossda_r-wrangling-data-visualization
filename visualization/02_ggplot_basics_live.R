# 02 ggplot basics

library(here)
library(ggplot2)
library(psych)
library(dplyr)

sub_data<-here("wrangling","04_prepare-visualization-data.R")
sub_data
source(sub_data)

data_subset_long

# Disagreements
d<-select(data_subset_long,matches('^disagr_'))
tail(d)
d$disagr <- rowMeans(d, na.rm = TRUE)
d$disagr

# hh task balance
t<-select(data_subset_long,matches('^task_'))
t$task <- rowMeans(t, na.rm = TRUE)

# sadness
s<-select(data_subset_long,matches('^sad_'))
s$sad <- rowMeans(s, na.rm = TRUE)

# ses
ses <- select(data_subset_long,
              n_rooms,
              hh_ends_meet,
              hh_income,
              hh_2_car,
              hh_2_home)
ses
ses<-mutate_if(ses, is.factor, as.numeric)
ses

ses$hh_2_car <- (ses$hh_2_car - 2) * -1
ses$hh_2_home <- (ses$hh_2_home - 2) * -1
ses

ses$ses <- rowMeans(scale(ses), na.rm = TRUE)
ses

dfl<-tibble(data_subset_long,
            "disagr"=d$disagr,
            "task"=t$task,
            "sad"=s$sad,
            "ses"=ses$ses
            )

mis <- rowSums(is.na(dfl))

# Plot using base
h1<-hist(mis)

# Describe
psych::describe(dfl)
glimpse(dfl)

dfw <- pivot_wider(dfl,
                   id_cols = "rid",
                   names_from="wave",
                   values_from = !matches("rid|wave"),
                   names_glue = "{wave}{.value}")
                   )

dfw

### ggplot!

ggplot(data = dfw, mapping = aes(x = aage)) +
    geom_vline(xintercept = mean(dfw$aage),
               col = "orange",
               size = 2) +
    xlim(17,79)

ggplot(dfw, aes(x = aage, y = adisagr)) +
    geom_point(size = 3, alpha=0.1, position = "jitter") +
    geom_smooth(method = lm, size = 2, col = "red")

cor<-cor(dfw$aage, dfw$adisagr, use="complete.obs")
cor.test(dfw$aage, dfw$adisagr, use="complete.obs")

### Frequency chart
p01<-ggplot(dfw, aes(asex))
p01
p01 + geom_bar(stat = "count",
               width = 0.6,
               alpha = 0.7,
               col = "black",
               fill = "orange")

p01 <- p01 + geom_bar(width = 0.6, alpha = 0.7, aes(fill = asex))

ggplot(dfw, aes(asex, fill=asex)) +
    geom_bar(aes(fill = asex))

sumtab <- summarise(group_by(dfw, asex), n = n())
sumtab

p02 <- ggplot(sumtab, aes(x = asex, y = n))
p02 + geom_bar(stat = "identity")

# Histograms and density
p03 <- ggplot(dfw, aes(aage))
p03 + geom_histogram(bins = 15, alpha = 0.4) +
    geom_histogram(bins = 30, alpha = 0.4)

p03 + geom_density(adjust = 0.2)

#
