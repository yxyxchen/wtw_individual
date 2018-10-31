# this script is for 

# outFile
outFile = 'QStar_figures'
# library
library("ggplot2")
library("dplyr")
library("tidyr")
library('scales')
source('plotTheme.R')
source('wtwSettings.R')

# initialSpace
load( file = 'QStarData/initialSpace.RData') 

#### 
load('QStarData/colpData.RData')
load('QStarData/rawWTW.RData')
load('QStarData/hdrData.RData')
####
####### plot distribution of totalEarnings
plotData = data.frame(totalEarnings = c(colpHPData$totalEarnings, colpLPData$totalEarnings),
                      condition = rep(c("HP", "LP"), each = nComb), tau = initialSpace[,1],
                      gamma = initialSpace[,2]
)

ggplot(plotData, aes(totalEarnings)) + geom_histogram(bins = 15) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of simulations") + saveTheme + xlim(c(0, 600))
fileName = file.path(outFile, 'earningSml.pdf')
ggsave(fileName, width = 16, height = 8)

# calculate range
summarise(group_by(plotData, condition),
          minEarning = min(totalEarnings),
          maxEarning = max(totalEarnings))

############ summarise para effects on total earnings ###########
paraValues = 1 : nValue
summaryData = data.frame(condition = rep(c("HP", "LP"), each = nValue, nPara),
                         paraNames = rep(paraNames, each = nValue * 2),
                         paraValues = rep(paraValues, nPara * 2))
summaryData$paraNames = factor(summaryData$paraNames, levels = paraNames)
# summarise mu and sd
mu = rep(NA, nrow(summaryData))
std = rep(NA, nrow(summaryData))
tempt = summarise(group_by(plotData, condition, tau), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[1: (nValue * 2)] = tempt$mu; std[1: (nValue * 2)]= tempt$std
tempt = summarise(group_by(plotData, condition, gamma), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[(nValue * 2 + 1): (nValue * 4)] = tempt$mu; std[(nValue * 2 + 1): (nValue * 4)]= tempt$std
summaryData$mu = mu
summaryData$std = std
summaryData$ymin = mu - std
summaryData$ymax = mu + std

# plot for HP
for(c in 1:2){
  cond = conditionNames[c]
  ggplot(summaryData[summaryData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.2)+
    facet_wrap(~paraNames, nrow = 1)+ saveTheme +
    xlab("Parameter value") + ylab("Total Earnings") + ggtitle(cond) 
  fileName = file.path(outFile, sprintf("paraEffect%s.pdf", cond))
  ggsave(fileName, width = 16, height = 8) 
}

############ look at actionValues group by totalEanrings #########
perc = 0.1
nUse= floor(nComb* perc);
rankings = c('Top', 'Bottom')

for(c in 1 : 2){
  cond = conditions[c]
  condName = conditionNames[c]
  condColor = conditionColors[c]
  
  if(condName == 'HP'){
    hdrData = hdrHPData
    inputData = colpHPData
  }else{
    hdrData = hdrLPData
    inputData = colpLPData  
  }
  
  for(r in 1:2){
    ranking = rankings[r]
    tempt = order(inputData$totalEarnings, decreasing = (ranking == 'Top'))
    UseVaWaits = inputData$vaWaits[tempt[1:nUse],]
    UseVaQuits= inputData$vaQuits[tempt[1:nUse],]


    # plot
    vas = cbind(UseVaWaits, UseVaQuits)
    meanValues = apply(vas, FUN = function(x) mean(x[!is.na(x)]), MARGIN = 2)
    stdValues = apply(vas, FUN = function(x) sd(x[!is.na(x)]), MARGIN = 2)
    maxValues = meanValues + stdValues
    minValues = meanValues - stdValues
    actions = factor(rep(c('wait', 'quit'), each = hdrData$nTimeStep), levels = c('wait', 'quit'))
    plotData = data.frame(meanValues, stdValues, maxValues, minValues, 
                          timeSteps = rep(seq(1,hdrData$nTimeStep),2), actions )
    graphics.off()
    titleText = sprintf("%s, %s%s total earnings",condName, ranking, percent(perc))
    ggplot(plotData, aes(timeSteps, meanValues, linetype = actions)) + 
      geom_ribbon(data = plotData[plotData$actions == 'wait',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_ribbon(data = plotData[plotData$actions == 'quit',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") + 
      geom_line(color = conditionColors[c], size = 1) + xlab('Time step') + ylab('Action value') + ggtitle(titleText)+ saveTheme +
      scale_linetype_discrete(name = "Action") 
    #coord_cartesian(ylim=c(-2,5))
    fileName = sprintf('%s/actionValue%s%s.pdf', outFile, condName, ranking)
    ggsave(file = fileName, width = 10, height = 6)
  }
}

######### plot aucCompare and wtwCompare #######
p1 = matrix(c(colpHPData[[1]],colpHPData[[5]], colpHPData[[6]],colpHPData[[7]]),
ncol = 4)
p2 = matrix(c(colpLPData[[1]],colpLPData[[5]], colpLPData[[6]],colpLPData[[7]]),
            ncol = 4)
plotData = rbind(p1, p2)
plotData = as.data.frame(plotData)
colnames(plotData) = names(colpHPData)[c(1,5,6,7)]
plotData$condition = rep(c('HP', 'LP'), each = length(colpHPData$totalEarnings))

plotData = plotData %>% arrange(totalEarnings) %>%group_by(condition) %>%
  mutate(earningRank = rank(totalEarnings, ties.method = "first"))

ggplot(plotData, aes(condition, AUC)) + geom_jitter(aes(color =  earningRank ), size = 4) +
  scale_color_gradient(low="red", high="yellow", name = 'Earning ranking') +
  geom_segment(aes(x= 0.7, xend = 1.3, y=optimWaitTimes$HP,yend=optimWaitTimes$HP), size = 2) +
  geom_segment(aes(x= 1.7, xend = 2.3, y=optimWaitTimes$LP,yend=optimWaitTimes$LP), size = 2) + saveTheme
fileName = file.path(outFile, "acuCompare.pdf")
ggsave(fileName, width = 12, height = 8)

ggplot(plotData[plotData$condition == 'LP',], aes(AUC, totalEarnings)) + geom_point() +
  saveTheme + ylab('Total earnings')
fileName = file.path(outFile, "AUCLP_earnings.pdf") 
ggsave(fileName, width = 6, height = 4)


#### wtw
ggplot(plotData, aes(condition, wtw)) + geom_jitter(aes(color =  earningRank ), size = 4) +
  scale_color_gradient(low="red", high="yellow", name = 'Earning ranking') +
  geom_segment(aes(x= 0.7, xend = 1.3, y=optimWaitTimes$HP,yend=optimWaitTimes$HP), size = 2) +
  geom_segment(aes(x= 1.7, xend = 2.3, y=optimWaitTimes$LP,yend=optimWaitTimes$LP), size = 2) + saveTheme
fileName = file.path(outFile, "wtwCompare.pdf")
ggsave(fileName, width = 12, height = 8)


#### check immediete quit
# HP 
a = (rawHPData$timeWaited == 0) & (rawHPData$rewardDelays != 0)
endTicks = apply(rawHPData$rewardDelays, MARGIN = c(1,2),
                 FUN = function(x) match(0, x) - 1)
sum(a[!is.na(a)]) / (5 * 243 * mean(endTicks))


# LP
a = (rawLPData$timeWaited == 0) & (rawLPData$rewardDelays != 0)
endTicks = apply(rawLPData$rewardDelays, MARGIN = c(1,2),
                 FUN = function(x) match(0, x) - 1)
sum(a[!is.na(a)]) / 5 / 243 / mean(endTicks)

#### check wtw change
# HP
