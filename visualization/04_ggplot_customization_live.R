### 04 ggplot customization

library(ggplot2)
library(RColorBrewer)
library(cowplot)

p07 <- ggplot(dfw, aes(ahh_income, fill=asex)) +
    geom_bar(position="dodge")
p07 <- p07 + labs(
    title = "Frequency of household income by sex",
    x="Household income",
    y="Count",
    fill = "Sex"
)
p07 <- p07 + theme_minimal()
p07<- p07 + theme(plot.title = element_text(size=20, hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")
p07 + geom_text(stat="count", aes(label=..count..), position = position_dodge(0.9), size=5, vjust = -0.7)

p07 + coord_flip()

# Color
p01
p01 + scale_fill_brewer(palette = "Set2")
ourcolor<-c("#AA3939", "#413075", "#2D882D")
p01 + scale_fill_manual(values = ourcolor)

p08<-ggplot(dfw, aes(aage, ases, col=asex)) +
    geom_point(size = 2, alpha = 0.3) +
    geom_smooth()
p08 + scale_x_continuous(limits = c(17,79),
                         breaks = seq(17,79, by=2))

p08
p07

library(cowplot)
plot_grid(p07, p08, labels = c("First plot", "Second plot"), ncol=2, nrow=1)

ggsave(here("visualization","sidebyside.pdf"), height = 8, width = 16)
