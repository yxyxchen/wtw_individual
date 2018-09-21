# individual difference analysis
# Cyx


# analysis sub-functions
source('load.R')
source('helperFxs.R')
library("ggplot2")
library('dplyr')
# load all data
allData = load()
hdrData = allData$hdrData             # unpack header data
# data frame with variables "ID", "Cbal", "Condition1", "Condition2"
trialData = allData$trialData         # unpack trial data
# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
cat('Analyzing data for n','=',n,'subjects.\n')

# 
# control which individual-level plots to generate
plotScheduledDelays = F
plotTrialwiseData = F
plotKMSC = F
plotRT = F

# initialize structures to hold group data
grpAUC = numeric(length =n * 2)
earningsByBlock = numeric(length= n * 2)
condByBlock = vector(length= n * 2)
FunctionByBlock = vector(length= n * 2)
wtw = list()
kmGrid = seq(0, 20, by=0.1) # grid on which to average survival curves.

# sc_rising1 = matrix(NA, nrow=n, ncol=length(kmGrid))
# sc_falling1 = matrix(NA, nrow=n, ncol=length(kmGrid))
# sc_rising2 = matrix(NA, nrow=n, ncol=length(kmGrid))
# sc_falling2 = matrix(NA, nrow=n, ncol=length(kmGrid))
# earningsHP = numeric(length=n)
# earningsLP = numeric(length=n)
# aucHP = numeric(length=n)
# aucLP = numeric(length=n)

# descriptive statistics for individual subjects and blocks
for (sIdx in 1:n) {
  
  # pull this subject's data
  thisID = allIDs[sIdx]
  
  for (bkIdx in 1:2) {
    
    thisTrialData = trialData[[thisID]]
    thisBlockIdx = (thisTrialData$blockNum == bkIdx)
    thisTrialData = thisTrialData[thisBlockIdx,]
    thisCond = unique(thisTrialData$condition)
    thisFunction = unique(thisTrialData$trial_function)
    thisFunction = unique(thisTrialData$trial_function)
    label = sprintf('Subject %s, Block %d (Cbal %d)',thisID,bkIdx,hdrData$Cbal[sIdx])
    
    # conditionByBlock $ FunctionByBlock
    if(bkIdx == 1){condByBlock[sIdx*2 - 1] = thisCond}
    if(bkIdx == 2){condByBlock[sIdx*2] = thisCond}
    if(bkIdx == 1){FunctionByBlock[sIdx*2 - 1] = thisFunction}
    if(bkIdx == 2){FunctionByBlock[sIdx*2] = thisFunction}
    
    # earnings in this block
    if(bkIdx == 1){earningsByBlock[sIdx*2 - 1] = sum(thisTrialData$trialEarnings)}
    if(bkIdx == 2){earningsByBlock[sIdx*2] = sum(thisTrialData$trialEarnings)}

    
    # survival analysis
    tMax = 30 # time window for the survival analysis
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
    if(bkIdx == 1){grpAUC[sIdx*2 -1] = kmscResults[['auc']]}
    if(bkIdx == 2){grpAUC[sIdx*2] = kmscResults[['auc']]}
    
    # save the full AUC in the appropriate place
    # if (thisCond=='rising' & bkIdx<=2) {sc_rising1[sIdx,] = kmscResults[['kmOnGrid']]}
    # if (thisCond=='falling' & bkIdx<=2) {sc_falling1[sIdx,] = kmscResults[['kmOnGrid']]}
    # if (thisCond=='rising' & bkIdx>=3) {sc_rising2[sIdx,] = kmscResults[['kmOnGrid']]}
    # if (thisCond=='falling' & bkIdx>=3) {sc_falling2[sIdx,] = kmscResults[['kmOnGrid']]}
    # 
    # WTW time series - for an individual block. 
    tGrid = seq(0, 10*60, by=1) # time grid within the block
    wtwCeiling = 40 # since trials exceeding this duration are infrequent
    # (e.g., 2 perfectly patient individuals could have different results depending what max time they got)
    wtwtsResults = wtwTS(thisTrialData, tGrid, wtwCeiling)
    if(bkIdx == 1){wtw[[sIdx * 2 -1]] = wtwtsResults}
    if(bkIdx == 2){wtw[[sIdx * 2]] = wtwtsResults}
    # ***Other possible metrics
    #     width or inflection point of the KMSC (sigmoid fit?)
    #     slope or lower bound of the WTW function. spline fit?
    #     2nd-half AUC
    
    # wait for input before continuing, if individual plots were requested
    if (any(plotScheduledDelays, plotTrialwiseData, plotKMSC, plotRT)) {
      readline(prompt = paste('subject',thisID,'(hit ENTER to continue)'))
    }
  } # loop over blocks
}
# organize groupdata 
groupData = data.frame(id = rep(allIDs, each = 2), blockNum = rep(c(1,2), n),
                       cbal = rep(hdrData$Cbal, each = 2), condition = condByBlock,
                       trialFun = FunctionByBlock, AUC = grpAUC)

       
# compare AUC for different environments and trialFun
summarise(group_by(groupData, condition, trialFun), meanAUC =  mean(AUC))

# find maxmal and minimal waiting time if quit
holdOnTimesMin = rep(0, n)
holdOnTimesMax = rep(0, n)
for(sIdx in 1 : n){
  # pull this subject's data
  thisID = allIDs[sIdx]  
  thisTrialData = trialData[[thisID]]
  holdOnTimesMin[sIdx] = min(thisTrialData$timeWaited[thisTrialData$trialEarnings == 0])
  holdOnTimesMax[sIdx] = max(thisTrialData$timeWaited[thisTrialData$trialEarnings == 0])
   

  # hist(thisTrialData$timeWaited)
  # readline(prompt = paste('subject',thisID,'(hit ENTER to continue)'))
}
