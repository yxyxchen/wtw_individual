# this script plot the dynamics of action values for a given para comb

# load functions and data 
load('outputs/QStarData/initialSpace.RData')
source('subFxs/helperFxs.R')
source('subFxs/paraFxs.R')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
source('model.R')
library('ggplot2')
library('dplyr')
library('tidyr')

################ selec condition ################
# cond input
condIdx = 2
cond = conditions[condIdx];
condName = conditionNames[condIdx]
condColor = conditionColors[condIdx]
sprintf('Condition : %s %s', cond, condName)

# choose hdrData depending on Cond
load('outputs/QStarData/hdrData.RData')
if(condName == 'LP') hdrData = hdrLPData else hdrData = hdrHPData
stepDuration = hdrData$stepDuration
traceValues = hdrData$traceValues
trialTick = trialTicks[[condIdx]]
sigma = hdrData$sigma
tMax = hdrData$tMax

########### choose the para and simulate data #############
combIdx = 100
para = initialSpace[combIdx,]
MSPara = getMSPara(cond, stepDuration, nMS, traceValues, sigma)
otherPara = getOtherPara(cond, stepDuration)
tempt = QStarModel(para, MSPara, otherPara, cond)


#######  calculate totalEarnings and conduct kmsc #######
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

######### plot trialData ##########
blockData = data.frame(trialEarnings = tempt$trialEarnings,
                       scheduledWait = tempt$rewardDelays,
                       timeWaited = tempt$timeWaited,
                       trialNum = 1 : length(tempt$timeWaited)
)
endTick = match(0, tempt$rewardDelays) - 1
blockData = blockData[1:endTick, ]
trialPlots(blockData, label)

######### plot survival curve ##########
plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks[[condName]])
ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
  ggtitle(label)


############ plot the dynamics for action values #######
# prepare data 
vaWaits = tempt$vaWaits
vaQuits = tempt$vaQuits
# unupdated vaWaits were not recorded, so here we manually
# set them identical with the last updated value
for(i in 1 : endTick){
  vaQuits[is.na(vaQuits[,i]),i] = vaQuits[match(NA, vaQuits[,i]) -1,i]
}

# plot for each time step
for(i in 1: endTick){
  cIdx = i
  plotData = data.frame(va =c(vaWaits[,cIdx], vaQuits[,cIdx]),
                        time = rep( 1 : (otherPara$tMax / otherPara$stepDuration), 2),
                        action = rep(c('wait', 'quit'),
                                     each = otherPara$tMax / otherPara$stepDuration))
  trialTitle =  sprintf('Trial %d, ', i)
  rewardTitle = sprintf('preR = %d, preT = %.2f; nowR = %d, nowT =%.2f',
                  tempt$trialEarnings[i-1], waitDuration[i-1],
                  tempt$trialEarnings[i], waitDuration[i])
  label = c(trialTitle, rewardTitle)
  
  if(is.na(tempt$timeWaited[i] )){
    int_num = floor(tempt$rewardDelays[i]) 
    endStep =  tempt$rewardDelays[i] + 0.5  + 0.5 * ((tempt$rewardDelays[i] - int_num) > 0.5)
    endStep = endStep / stepDuration
  }else{
    endStep = tempt$timeWaited[i] / stepDuration
  }
  
  p = ggplot(plotData, aes(time, va, color = action)) + geom_line() +
    geom_vline(xintercept = endStep) +
    ggtitle(label) + xlab('time step') + ylab('action value') + displayTheme
  print(p)
  readline(prompt = paste(i, '(hit ENTER to continue)'))
}


# quikQuit
endTick = match(0, tempt$rewardDelays) - 1
sum(tempt$timeWaited[1:endTick] < 1 & !is.na(tempt$timeWaited[1:endTick]))




