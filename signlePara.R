library('ggplot2')
library('dplyr')
library('tidyr')
source('simulate.R') # QStar model
source('wtwSettings.R') # wtw settings for both HP and LP
# can't change
source('getPara.R') # functions to get MSPara and otherPara from inputs and wtwSettings
# can change for different MS model, and 
#### load
load('QStarData/colpData.RData')

################ selec condition ################
# cond input
condIdx = 2
cond = conditions[condIdx];
condName = conditionNames[condIdx]
condColor = conditionColors[condIdx]
sprintf('Condition : %s %s', cond, condName)

# get input
if(condName == 'HP') inputRaw = rawHPData else inputRaw = rawLPData 

########### extract raw data #############
combIdx = 29
rIdx = 2

para = initialSpace[combIdx,]
tempt = list(ws = inputRaw$ws[combIdx, rIdx,],
             timeWaited = inputRaw$timeWaited[combIdx, rIdx,],
             rewardDelays = inputRaw$rewardDelays[combIdx, rIdx,],
             trialEarnings = inputRaw$trialEarnings[combIdx, rIdx,],
             vaWaits = inputRaw$vaWaits[combIdx, rIdx, , ],
             vaQuits = inputRaw$vaQuits[combIdx, rIdx, , ])
tempt = QStarModel(para, MSPara, otherPara, cond)

# check ws
meanVaWaits = apply(tempt$vaWaits, MARGIN = 1, FUN = function(x) mean(x[!is.na(x)]))
meanVaQuits = apply(tempt$vaQuits, MARGIN = 1, FUN = function(x) mean(x[!is.na(x)]))
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
quikQuit = sum(tempt$timeWaited[1:endTick] < 1 & !is.na(tempt$timeWaited[1:endTick]))


waitDuration = tempt$timeWaited
rewardDelay = tempt$rewardDelays
quitIdx = (tempt$trialEarnings == 0)
waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
endTick = match(0,rewardDelay)
waitDuration = waitDuration[1 : (endTick - 1)]
quitIdx = quitIdx[1 : (endTick - 1)]
kmscResults = kmscSimple(waitDuration, quitIdx, tMax, trialTick)
plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks$LP)
p = ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
  ggtitle(label)
print(p)


