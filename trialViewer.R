# we should upload it
source('helperFxs.R')
library('ggplot2')
source('plotTheme.R')
source('wtwSettings.R')

# load data 
####
load('QStarData/rawdata.RData')
load('QStarData/colpData.RData')
load('QStarData/initialSpace.RData')


# prepare
####
condIdx = 2
cond = conditions[condIdx]
condName = conditionNames[condIdx]

inputColp = if(condName == 'HP') inputColp = colpHPData else inputColp = colpLPData
inputRaw = if(condName == 'HP') inputRaw = rawHPData else inputRaw = rawLPData

tMax = tMaxs[condIdx]
trialTick = trialTicks[[condIdx]] # so here if use [2] then get a list
# choose combs you want to plot
nCombList = which(inputColp$AUC <= 6 & inputColp$AUC >= 2) # combs supposed to earn most 
nCombList = which(inputColp$AUC > 10 ) 
plotTrialData = T
plotKMSC= T
drawTimeSample = T

for (nCb in 1 : length(nCombList)){
  i = nCombList[nCb]
  j = 1
  
  # prepare total earnings, wtw and AUC
  totalEarnings = inputColp$totalEarnings[i]
  wtw = inputColp$wtw[i]
  AUC = inputColp$AUC[i]
  
  label = sprintf('colp stat, earn: %d, wtw: %.2f, AUC: %.2f',
                  totalEarnings, wtw, AUC)
  
  if(plotTrialData){
    # prepare trialData
    blockData = data.frame(trialEarnings = inputRaw$trialEarnings[i,j,],
                           scheduledWait = inputRaw$rewardDelays[i,j,],
                           timeWaited = inputRaw$timeWaited[i,j,],
                           trialNum = 1 : length(inputRaw$timeWaited[i,j,])
    )
    endTick = match(0, inputRaw$rewardDelays[i,j,]) - 1
    blockData = blockData[1:endTick, ]
    # plot
    trialPlots(blockData, label)
  }
  
  if(plotTrialData) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }

  
  ## look at kmsc
  if(plotKMSC){
    waitDuration = inputRaw$timeWaited[i, j, ]
    rewardDelay = inputRaw$rewardDelays[i, j, ]
    quitIdx = (inputRaw$trialEarnings[i, j, ] == 0)
    waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
    endTick = match(0,rewardDelay)
    waitDuration = waitDuration[1 : (endTick - 1)]
    quitIdx = quitIdx[1 : (endTick - 1)]
    
    kmscResults = kmscSimple(waitDuration, quitIdx, tMax, trialTick)
    plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks[[condName]])
    p = ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
      ggtitle(label)
    print(p)
  }
  
  if(plotKMSC) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
  
  ### draw wait duration distribution
  if(drawTimeSample){
    # prepare pdf
    cdf = 1 - kmscResults$kmOnGrid;
    cdf[length(cdf)] = 1
    pdf = diff(c(0, cdf)) # hre 0 is the time tick before 0
    
    # 
    draws = sample(trialTicks[[condName]], size = 1000, replace = TRUE, prob = pdf)
    p = ggplot(data.frame(draws),aes(draws)) + geom_histogram(bins = 50) + xlim(c(0 - 1, tMax+3)) +
      displayTheme + xlab('Wait duration / s') + ggtitle(label)
    print(p)
  }
    
  if(any(drawTimeSample)) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
  
}


############ compare to exp data #######
load('expData/groupData.RData')
nCombList = which(groupData$AUC <= 6 & groupData$AUC >= 2 & groupData$condition == 'LP')
nCombList = which(groupData$totalEarnings > 420 & groupData$condition == 'LP')
# nCombList = 160
for(nCb in 1 : length(nCombList)){
  idx = nCombList[nCb]
  sIdx = ceiling(idx / 2) 
  bkIdx = idx - (sIdx - 1) * 2
  
  # get data
  thisID = allIDs[sIdx]
  thisTrialData = trialData[[thisID]]
  thisCond = unique(thisTrialData$condition)
  thisBlockIdx = (thisTrialData$blockNum == bkIdx)
  thisTrialData = thisTrialData[thisBlockIdx,]
  thisFunction = unique(thisTrialData$trial_function)
  label = sprintf('Subject %s, earn %d, AUC %.2f)',thisID,
                  groupData$totalEarnings[idx],
                  groupData$AUC[idx])
  
  tMax = ifelse(thisCond == conditionNames[1], tMaxs[1], tMaxs[2])
  kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.
  # plot trialData
  if (plotTrialData) {
    trialPlots(thisTrialData,label)
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }

  
  # survival analysis
  if(plotKMSC){
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }

  
  # plot wait time distribution based on survival analysis
  if(drawTimeSample){
    # prepare pdf
    cdf = 1 - kmscResults$kmOnGrid;
    cdf[length(cdf)] = 1
    pdf = diff(c(0, cdf)) # hre 0 is the time tick before 0
    
    # 
    draws = sample(trialTicks[[thisCond]], size = 1000, replace = TRUE, prob = pdf)
    p = ggplot(data.frame(draws),aes(draws)) + geom_histogram() + xlim(c(0 - 1, tMax+3)) +
      displayTheme + xlab('Wait duration / s') + ggtitle(label)  + xlab('Wait duration / s')
    print(p)
  }
  
  # # wait for input before continuing, if individual plots were requested
  if(any(plotKMSC, plotTrialData, drawTimeSample)){
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
}

  
## look deep 
check = rawLPData$vaWaits[4 , 1,  , ]
record = data.frame( rawLPData$trialEarnings[4, 1,],
                     rawLPData$timeWaited[4,1,])

  
  
  
  