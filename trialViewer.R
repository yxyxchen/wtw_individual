# 
source('helperFxs.R')

#####

####
load('QStarData/rawdata.RData')
load('QStarData/colpData.RData')

####
which(colpLPData$AUC <= 6 & colpLPData$AUC >= 2)
#  19  21  46  47  48  74 100 202 229 230 231
i = 19
j = 3
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

