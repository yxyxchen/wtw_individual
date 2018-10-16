# this script explored the dynamic of action value
# load data
load('QStarData/data.RData')
load('QStarData/hdrData.RData')

# load setting 
library(ggplot2)
source('wtwSettings.R')
source('getPara.R')
source('plotTheme.R')
source('simulate.R')
# input 
cIdx = 1
cond = conditions[cIdx]
condName = conditionNames[cIdx]
condColor = conditionColors[cIdx]
stepDuration = 0.5;
MSPara = getMSPara(cond, stepDuration, nMS)
otherPara = getOtherPara(cond, stepDuration)

# initialSpace
nPara = 5
nValue = 3
nComb = nValue ^ nPara
tMax = otherPara[['tMax']]
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.2, 0.8, 0.3), each = nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^2), nValue^(nPara - 3)) 
initialSpace[,4] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^3), nValue^(nPara - 4)) 
initialSpace[,5] = rep(rep(seq(2, 8, 3), each = nValue^4), nValue^(nPara - 5)) 


# HP, best 28, worst 88
# LP, best 109, worst 237

para = initialSpace[28,]
source('simulate.R')
tempt=  simulate(para,MSPara, otherPara, cond)

vaWaits = apply(tempt$vaWaits, MARGIN = 1,
                FUN = function(x) mean(x[!is.na(x)]))
vaQuits = apply(tempt$vaQuits, MARGIN = 1,
                FUN = function(x) mean(x[!is.na(x)]))

plotData = data.frame(va = c(vaWaits, vaQuits),
                      timeStep = rep(1 : (tMax / stepDuration), 2),
                      action = rep(c('wait', 'quit'), each = tMax / stepDuration))
ggplot(plotData, aes(timeStep, va, linetype = action)) + geom_line(color = condColor, size = 2) +
  xlab('Time step') + ylab('Action value') + saveTheme + ggtitle(condName) 
filename = sprintf('figures/actionValue%sBest.pdf', condName)
ggsave(filename, width = 6, height = 4)
