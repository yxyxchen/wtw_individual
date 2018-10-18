library("dplyr")
library("tidyr")
library('ggplot2')
library('scales')
source('plotTheme.R')
source('wtwSettings.R')
source('helperFxs.R')
## outFile
outFile = 'QStarData'
############# load raw data
load('QStarData/rawData.RData')
load('QstarData/hdrData.RData')

############ calculate colpData
# colpTrialEarnings 
colpTrialEarnings = vector(mode = "list", 2)
colpTotalEarnings = vector(mode = "list", 2)
for(c in 1 : 2){
  cond = conditions[c];
  condName = conditionNames[c];
  
  if(condName == "HP") inputData = rawHPData else inputData = rawLPData
  
  colpTrialEarnings[[condName]] =
    apply(inputData$trialEarnings, MARGIN = c(1,3), FUN = mean)
  colpTotalEarnings[[condName]] = apply(colpTrialEarnings[[condName]],
                                        MARGIN = 1, FUN = sum)
}

# colpVaWaits & colpVaQuits
colpVaWaits = vector(mode = "list", 2)
colpVaQuits = vector(mode = "list", 2)
for(c in 1 : 2){
  cond = conditions[c];
  condName = conditionNames[c];
  if(condName == "HP") inputData = rawHPData else inputData = rawLPData
  
  colpVaWaits[[condName]]  =  apply(inputData$vaWaits, MARGIN = c(1, 3),
                                    FUN = function(x) mean(x[!is.na(x)]))
  colpVaQuits[[condName]]  =  apply(inputData$vaQuits, MARGIN = c(1, 3),
                                    FUN = function(x) mean(x[!is.na(x)]))
}

###### AUC data #####
colpAUC = list()
rawWTW = list()
for(c in 1 : 2){
  cond = conditions[c];
  condName = conditionNames[c];
  
  # input
  if(condName == "HP") inputData = rawHPData else inputData = rawLPData
  tMax = tMaxs[c]
  trialTick = trialTicks[[condName]]
  
  # dim 
  nComb = dim(inputData$timeWaited)[1]
  nRep = dim(inputData$timeWaited)[2]
  
  # temporary output
  output = matrix(NA, nComb, nRep)
  wtwResult = array(dim = c(nComb, nRep, length(tGrid)))
  for(i in 1 : nComb){
    for(j in 1 : nRep){
      waitDuration = inputData$timeWaited[i, j, ]
      rewardDelay = inputData$rewardDelays[i, j, ]
      quitIdx = (inputData$trialEarnings[i, j, ] == 0)
      
      waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
      endTick = match(0,rewardDelay)
      waitDuration = waitDuration[1 : (endTick - 1)]
      quitIdx = quitIdx[1 : (endTick - 1)]
      
      output[i, j] = kmscSimple(waitDuration, quitIdx, tMax, trialTick)$auc
      wtwResult[i, j, ] = wtwTSSimple(waitDuration, quitIdx, tGrid, tMax)
    } # end of comb
  }# end of condition
  colpAUC[[condName]] = rowSums(output) / ncol(output)
  rawWTW[[condName]] = wtwResult
}

### timeWaited
endTicks = apply(rawLPData$rewardDelays, MARGIN = c(1,2),
                 FUN = function(x) match(0, x) - 1)
colpTimeWaited = list()
for(c in 1:2){
  condName = conditionNames[c]
  if(condName == 'HP') inputData = rawHPData else  inputData = rawLPData 
  output = matrix(NA, length(colpHPData$AUC), 5)
  for(i in 1 : length(colpHPData$AUC)){
    for(j in 1 : 5){
      timeWaited = inputData[['timeWaited']][i, j, 1 : endTicks[i,j]]
      output[i, j] = mean(timeWaited[!is.na(timeWaited)])
    }
  } 
  output = rowSums(output) / ncol(output)
  if(condName == 'HP') colpTimeWaited$HP = output else colpTimeWaited$LP = output
}


### organize colpHPData
colpHPData = list(totalEarnings = colpTotalEarnings$HP,
                  trialEarnings = colpTrialEarnings$HP,
                  vaQuits = colpVaQuits$HP,
                  vaWaits = colpVaWaits$HP,
                  AUC = colpAUC$HP,
                  timeWaited = colpTimeWaited$HP,
                  wtw = apply(rawWTW$HP, MARGIN = 1, mean))
colpLPData = list(totalEarnings = colpTotalEarnings$LP,
                  trialEarnings = colpTrialEarnings$LP,
                  vaQuits = colpVaQuits$LP,
                  vaWaits = colpVaWaits$LP,
                  AUC = colpAUC$LP,
                  timeWaited = colpTimeWaited$LP,
                  wtw = apply(rawWTW$LP, MARGIN = 1, mean)
)

fileName = sprintf("%s/colpData.RData", outFile)
save('colpLPData', 'colpHPData', file = fileName )
fileName = sprintf("%s/rawWTW.RData", outFile)
save('rawWTW', file = fileName)
