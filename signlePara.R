library('ggplot2')
library('dplyr')
library('tidyr')
source('helperFxs.R')
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
trialTick = trialTicks[[condName]]

########### extract raw data #############
combIdx = 100
para = initialSpace[combIdx,]
MSPara = getMSPara(cond, stepDuration, nMS, traceValues, sigma)
otherPara = getOtherPara(cond, stepDuration)
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
                totalEarnings, AUC)

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


#probability of wait
tau = para[2]
waitProb = exp(vaWaits * tau) / (exp(vaWaits*tau) + exp(vaQuits* tau))
plotData$waitProb = waitProb[,endTick]

# check ws
# 
vaWaits = tempt$vaWaits
vaQuits = tempt$vaQuits

wIni = para[5]
gamma = para[3]
nTimeStep = otherPara$tMax / otherPara$stepDuration

for(i in 1 : endTick){
  vaQuits[is.na(vaQuits[,i]),i] = vaQuits[match(NA, vaQuits[,i]) -1,i]
}

for(i in 1: endTick){
  cIdx = i
  plotData = data.frame(va =c(vaWaits[,cIdx], vaQuits[,cIdx]),
                        time = rep( 1 : (otherPara$tMax / otherPara$stepDuration), 2),
                        action = rep(c('wait', 'quit'),
                                     each = otherPara$tMax / otherPara$stepDuration))
  label = sprintf('last, rwd = %d, tw = %.2f; rwd = %d, tw =%.2f',
                  tempt$trialEarnings[i-1], waitDuration[i-1],
                  tempt$trialEarnings[i], waitDuration[i])
  if(is.na(tempt$timeWaited[i] )){
    int_num = floor(tempt$rewardDelays[i]) 
    endStep =  tempt$rewardDelays[i] + 0.5  + 0.5 * ((tempt$rewardDelays[i] - int_num) > 0.5)
    endStep = endStep / stepDuration
  }else{
    endStep = tempt$timeWaited[i] / stepDuration
  }
  
  p = ggplot(plotData, aes(time, va, color = action)) + geom_line() +
    geom_vline(xintercept = endStep) +
    ggtitle(label) + xlab('step') 
  # plotData$waitProb = waitProb[,i]
  # p = ggplot(plotData[plotData$action == 'wait',], aes(time, waitProb)) + geom_line()+
  #   ggtitle(label) + xlim(c(1,3)) + xlab('step')
  print(p)
  readline(prompt = paste(i, '(hit ENTER to continue)'))
}


# quikQuit
endTick = match(0, tempt$rewardDelays) - 1
sum(tempt$timeWaited[1:endTick] < 1 & !is.na(tempt$timeWaited[1:endTick]))




