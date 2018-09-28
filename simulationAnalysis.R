# this script is for 

# library
library("ggplot2")
library("dplyr")
library("tidyr")
library("Scale")

# initialSpace
initialSpace = matrix(NA, 5^4, 4)
initialSpace[,1] = rep(seq(0.1, 1, 0.2), each = 5^3)
initialSpace[,2] = rep(rep(seq(0.1, 1, 0.2), each = 5^2), 5)
initialSpace[,3] = rep(rep(seq(0.1, 1, 0.2), each = 5), 5^2)
initialSpace[,4] = rep(seq(0.1, 1, 0.2), 5^3)

# load totalEarnings data
load('HPTotalEarnings.RData')
HPEarnings = totalEarnings
rm(totalEarnings)
load('LPTotalEarnings.RData')
LPEarnings = totalEarnings

# plot theme 
myTheme = theme(panel.background = element_rect(fill = "white",colour = "white"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+ 
  theme(axis.title=element_text(size=12), title =element_text(size=12, face='bold'), 
        plot.title = element_text(hjust = 0.5))

############ compare with-para and between parameter variance
aovData = gather(as.data.frame(HPEarnings), key, value)
aovData$paraComb = rep(1:625, 5)
fit = aov(value ~ paraComb, aovData)
summary(fit)

aovData = gather(as.data.frame(LPEarnings), key, value)
aovData$paraComb = rep(1:625, 5)
fit = aov(value ~ paraComb, aovData)
summary(fit)


############ plot distribution of earnings 
meanHP = rowSums(HPEarnings) / ncol(HPEarnings)
meanLP = rowSums(LPEarnings) / ncol(LPEarnings)

plotData = data.frame(totalEarnings = c(meanHP, meanLP),
                      condition = rep(c("HP", "LP"), each = 625), phi = initialSpace[,1],
                      tau = initialSpace[,2], gamma = initialSpace[,3],
                      lambda = initialSpace[,4]
                      )

ggplot(plotData, aes(plotData$totalEarnings)) + geom_histogram(bins = 10) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of simulations") + myTheme 
ggsave("figures/earningSml.pdf", width = 8, height = 4)


############ summarise effect of different parameters ###########
paraNames = c("phi", "tau", "gamma", "lambda")
paraValues = seq(0.1, 0.9, 0.2) 
summaryData = data.frame(condition = rep(c("HP", "LP"), each = 5, 4),
                         paraNames = rep(paraNames, each = 10),
                         paraValues = rep(paraValues, 8))
summaryData$paraNames = factor(summaryData$paraNames, levels = paraNames)

# summarise mu and sd
mu = rep(NA, nrow(summaryData))
std = rep(NA, nrow(summaryData))
tempt = summarise(group_by(plotData, condition, phi), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[1:10] = tempt$mu; std[1:10] = tempt$std
tempt = summarise(group_by(plotData, condition, tau), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[11:20] = tempt$mu; std[11:20] = tempt$std
tempt = summarise(group_by(plotData, condition, gamma), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[21:30] = tempt$mu; std[21:30] = tempt$std
tempt = summarise(group_by(plotData, condition, lambda), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[31:40] = tempt$mu; std[31:40] = tempt$std
summaryData$mu = mu
summaryData$std = std
se = std / sqrt((5 ^ 3))
summaryData$ymin = mu - std
summaryData$ymax = mu + std

# plot for HP
conditionNames = c("HP", "LP")
conditionColors = c("#7b3294", "#008837")
for(c in 1:2){
  cond = conditionNames[c]
  ggplot(summaryData[summaryData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + facet_wrap(~paraNames, nrow = 1)+
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.2) + myTheme +
    xlab("Parameter value") + ylab("Total Earnings") + ggtitle(cond) +
    coord_cartesian(ylim=c(100,500))
  fileName = sprintf("figures/paraEffect%s.pdf", cond)
  ggsave(fileName, width = 12, height = 4) 
}



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

