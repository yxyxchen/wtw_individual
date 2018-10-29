library('ggplot2')
library('dplyr')
library('tidyr')
source('simulate.R') # QStar model
source('wtwSettings.R') # wtw settings for both HP and LP
source('getPara.R') # functions to get MSPara and otherPara from inputs and wtwSettings
source('helperFxs.R')
################ selec condition ################
# cond input
condIdx = 1
cond = conditions[condIdx];
condName = conditionNames[condIdx]
condColor = conditionColors[condIdx]
sprintf('Condition : %s %s', cond, condName)
stepDuration = 0.5
otherPara = getOtherPara(cond, stepDuration)
tMax = otherPara[['tMax']]
trialTick = trialTicks[[condName]]

# initial space
nPara = 5
nValue = 3
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.2, 0.8, 0.3), each = nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^2), nValue^(nPara - 3)) 
initialSpace[,4] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^3), nValue^(nPara - 4)) 
initialSpace[,5] = rep(rep(seq(2, 8, 3), each = nValue^4), nValue^(nPara - 5)) 
########### simulate #############
combIdx = 29 # earn high AUC = 14
rIdx = 2
stepDuration = 0.5;

para = initialSpace[combIdx,] # 0.2 0.2 0.2 0.5 2.0
para = c(0.2, 20, 0.98, 0.98, 2)

tempt = QStarModel(para, MSPara, otherPara, cond)

# summarise earnings, AUC, wtw 
totalEarnings = sum(tempt$trialEarnings)

waitDuration = tempt$timeWaited
rewardDelay = tempt$rewardDelays
quitIdx = (tempt$trialEarnings == 0)
waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
endTick = match(0,rewardDelay)
waitDuration = waitDuration[1 : (endTick - 1)]
quitIdx = quitIdx[1 : (endTick - 1)]
kmscResults =  kmscSimple(waitDuration, quitIdx, tMax, trialTick)
AUC = kmscResults$auc
label = sprintf('earn: %d, AUC: %.2f',
                totalEarnings,AUC)

# plot trialData
blockData = data.frame(trialEarnings = tempt$trialEarnings,
                       scheduledWait = tempt$rewardDelays,
                       timeWaited = tempt$timeWaited,
                       trialNum = 1 : length(tempt$timeWaited)
)
endTick = match(0, tempt$rewardDelays) - 1
blockData = blockData[1:endTick, ]
trialPlots(blockData, label)

# plot survival
plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks[[condName]])
ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
  ggtitle(label)

# check ws
meanVaWaits = apply(tempt$vaWaits[,4:5], MARGIN = 1, FUN = function(x) mean(x[!is.na(x)]))
meanVaQuits = apply(tempt$vaQuits[,4:5], MARGIN = 1, FUN = function(x) mean(x[!is.na(x)]))
plotData = data.frame(va = c(meanVaWaits, meanVaQuits),
                    time = rep( 1 : (otherPara$tMax / otherPara$stepDuration), 2),
                    action = rep(c('wait', 'quit'), each = otherPara$tMax / otherPara$stepDuration))

ggplot(plotData, aes(time, va, color = action)) + geom_line()

# probability of wait
tau = para[2]
tempt$waitProb = exp(tempt$vaWaits * tau) / (exp(tempt$vaWaits*tau) + exp(tempt$vaQuits* tau))
plotData$waitProb = rep(apply(tempt$waitProb, MARGIN = 1, FUN = function(x) mean(x[!is.na(x)])), 2) 
ggplot(plotData[plotData$action == 'wait',], aes(time, waitProb)) + geom_line()

# quikQuit
endTick = match(0, tempt$rewardDelays) - 1
sum(tempt$timeWaited[1:endTick] < 1 & !is.na(tempt$timeWaited[1:endTick]))




