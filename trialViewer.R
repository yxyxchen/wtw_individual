# 
source('helperFxs.R')
library('ggplot2')
source('plotTheme.R')

#####

####
load('QStarData/rawdata.RData')
load('QStarData/colpData.RData')

####

# choose combs you want to plot
nCombList = which(colpLPData$AUC <= 6 & colpLPData$AUC >= 2) # combs supposed to earn most 
#nCombList = which(colpLPData$totalEarnings > 410) # combs actually earn most
plotTrialData = T
plotKMSC= F
drawTimeSample = F

for (nCb in 1 : length(nCombList)){
  i = nCombList[nCb]
  j = 1
  
  # prepare total earnings, wtw and AUC
  totalEarnings = sum(rawLPData$trialEarnings[i,j,])
  wtw = colpLPData$wtw[i]
  AUC = colpLPData$AUC[i]
  label = sprintf('earn: %d, wtw: %.2f, AUC: %.2f',
                  totalEarnings, wtw, AUC)
  
  if(plotTrialData){
    # prepare trialData
    blockData = data.frame(trialEarnings = rawLPData$trialEarnings[i,j,],
                           scheduledWait = rawLPData$rewardDelays[i,j,],
                           timeWaited = rawLPData$timeWaited[i,j,],
                           trialNum = 1 : length(rawLPData$timeWaited[i,j,])
    )
    endTick = match(0, rawLPData$rewardDelays[i,j,]) - 1
    blockData = blockData[1:endTick, ]
    # plot
    trialPlots(blockData, label)
  }

  
  ## look at kmsc
  if(plotKMSC){
    waitDuration = rawLPData$timeWaited[i, j, ]
    rewardDelay = rawLPData$rewardDelays[i, j, ]
    quitIdx = (rawLPData$trialEarnings[i, j, ] == 0)
    waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
    endTick = match(0,rewardDelay)
    waitDuration = waitDuration[1 : (endTick - 1)]
    quitIdx = quitIdx[1 : (endTick - 1)]
    
    kmscResults = kmscSimple(waitDuration, quitIdx, tMax, trialTick)
    plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks$LP)
    p = ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
      ggtitle(label)
    print(p)
  }
  
  ### draw wait duration distribution
  if(drawTimeSample){
    # prepare pdf
    cdf = 1 - kmscResults$kmOnGrid;
    cdf[length(cdf)] = 1
    pdf = diff(c(0, cdf)) # hre 0 is the time tick before 0
    
    # 
    draws = sample(trialTicks$LP, size = 1000, replace = TRUE, prob = pdf)
    p = ggplot(data.frame(draws),aes(draws)) + geom_histogram(bins = 50) + xlim(c(0 - 1, tMax+3)) +
      displayTheme + xlab('Wait duration / s') + ggtitle(label)
    print(p)
  }
    
    
  # # wait for input before continuing, if individual plots were requested
  if(any(plotAUC, plotTrialData)) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
}


############ compare to exp data #######
nCombList = which(groupData$AUC <= 6 & groupData$AUC >= 2 & groupData$condition == 'LP')
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
  label = sprintf('Subject %s, earn %d)',thisID, thisTrialData$totalEarnings)
  
  tMax = ifelse(thisCond == conditionNames[1], tMaxs[1], tMaxs[2])
  kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.
  # plot trialData
  if (plotTrialData) {
    trialPlots(thisTrialData,label)
  }
  
  # survival analysis
  if(plotKMSC){
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
  }

  
  # plot wait time distribution based on survival analysis
  if(drawTimeSample){
    # prepare pdf
    cdf = 1 - kmscResults$kmOnGrid;
    cdf[length(cdf)] = 1
    pdf = diff(c(0, cdf)) # hre 0 is the time tick before 0
    
    # 
    draws = sample(trialTicks$LP, size = 1000, replace = TRUE, prob = pdf)
    p = ggplot(data.frame(draws),aes(draws)) + geom_histogram() + xlim(c(0 - 1, tMax+3)) +
      displayTheme + xlab('Wait duration / s') + ggtitle(label)  + xlab('Wait duration / s')
    print(p)
  }
  
  # # wait for input before continuing, if individual plots were requested
  if(any(plotKMSC, plotTrialData, drawTimeSample)){
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
}

  
  
  
  
  
  