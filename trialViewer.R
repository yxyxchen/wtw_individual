# 
source('helperFxs.R')

#####

# plot trialwise responses in detail
trialPlots <- function(blockData,blockLabel) {
  # vectors to be plotted
  rwdIdx = blockData$trialEarnings != 0
  quitIdx = blockData$trialEarnings == 0
  rwdTrialNo = blockData$trialNum[rwdIdx]
  quitTrialNo = blockData$trialNum[quitIdx]
  rwdSchedDelay = blockData$scheduledWait[rwdIdx]
  quitSchedDelay = blockData$scheduledWait[quitIdx]
  waitDuration = blockData$timeWaited
  quitTime = waitDuration[quitIdx]
  # other parameters
  nTrials = nrow(blockData)
  # make the plot and add series
  rewardData = data.frame(rwdTrialNo, rwdSchedDelay)
  quitData = data.frame(quitTrialNo, quitTime, quitSchedDelay)
  
  ggplot(rewardData, aes(rwdTrialNo, rwdSchedDelay)) +
    geom_point(color = 'blue', size = 1) + geom_line(color = 'blue')
    geom_point(quitData, aes(quitTrialNo, quitTime), color = 'red', size = 1) +
      geom_line(color = 'red')
      
    geom_point(quitData, aes(quitTrialNo, quitSchedDelay), color = 'black', size = 1)
  
  #   plot(1, type='n', xlim=c(1,nTrials), ylim=c(0,32), bty='n',
  #      xlab='Trial', ylab='Trial duration (s)', main=sprintf('Trial data: %s',blockLabel))
  # lines(rwdTrialNo, rwdSchedDelay, col='blue', type='o', lwd=2, pch=16)
  # lines(quitTrialNo, quitTime, col='red', type='o', lwd=2, pch=16)
  # lines(quitTrialNo, quitSchedDelay, col='black', type='o', lwd=2, lty=0, pch=16)
  
}

####
load('QStarData/rawLPdata.RData')
load('QStarData/colpData.RData')

####
i = 60
j = 2
totalEarnings = sum(rawLPData$trialEarnings[i,j,])
wtw = colpLPData$wtw[i]
AUC = colpLPData$AUC[i]
label = sprintf('earn: %d, wtw: %.2f, AUC: %.2f',
                totalEarnings, wtw, AUC)
blockData = data.frame(trialEarnings = rawLPData$trialEarnings[i,j,],
                       scheduledWait = rawLPData$rewardDelays[i,j,],
                       timeWaited = rawLPData$timeWaited[i,j,],
                       trialNum = 1 : length(rawLPData$timeWaited[i,j,])
                       )
endTick = match(0, rawLPData$rewardDelays[i,j,]) - 1
blockData = blockData[1:endTick, ]
trialPlots(blockData, label)


## check syntax
rewardOccur = TRUE
ifelse(rewardOccur,  NA, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))

