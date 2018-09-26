# this script is for 

library("ggplot2")
# load data
load('HPResults.RData')
HPEarnings = totalEarnings
rm(totalEarnings)
load('LPResults.RData')
LPEarnings = totalEarnings

# plot theme 
myTheme = theme(panel.background = element_rect(fill = "white",colour = "white"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ 
  theme(axis.title=element_text(size=12), title =element_text(size=12, face='bold'), 
        plot.title = element_text(hjust = 0.5))

# plot distribution of earnings 
plotData = data.frame(totalEarnings = c(HPEarnings, LPEarnings),
                      condition = rep(c("HP", "LP"), each = 625))
ggplot(plotData, aes(plotData$totalEarnings)) + geom_histogram(bins = 10) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of blocks") + myTheme + xlim(c(0, 600))
ggsave("figures/earningSml.pdf", width = 8, height = 4)


# find best para
initialSpace = matrix(NA, 5^4, 4)
initialSpace[,1] = rep(seq(0.1, 1, 0.2), each = 5^3)
initialSpace[,2] = rep(rep(seq(0.1, 1, 0.2), each = 5^2), 5)
initialSpace[,3] = rep(rep(seq(0.1, 1, 0.2), each = 5), 5^2)
initialSpace[,4] = rep(seq(0.1, 1, 0.2), 5^3)

meanHP = rowSums(HPEarnings) / 5
meanLP = rowSums(LPEarnings) / 5
max(meanHP)
max(meanLP)
HPPara = initialSpace[which(meanHP == max(meanHP)),]
LPPara = initialSpace[which(meanLP == max(meanLP)),]

