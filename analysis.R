# individual difference analysis
# Cyx

# analysis sub-functions
source('load.R')
source('helperFxs.R')
library("ggplot2")
library('dplyr')
source("plotTheme.R")
source("wtwSettings.R")
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
plotWTW = F
plotTimeEarnings = T


# initialize outputs, organised by block
grpAUC = numeric(length =n * 2)
earningsByBlock = numeric(length= n * 2)
condByBlock = vector(length= n * 2)
FunctionByBlock = vector(length= n * 2)
wtw = matrix(NA, length(tGrid), n * 2)
cumEarn = matrix(NA, length(tGrid), n * 2)

# para for km analysis
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
    label = sprintf('Subject %s, Cond %s, Fun %s)',thisID, thisCond, thisFunction)
    
    #
    tMax = ifelse(thisCond == "unif16", tMaxs[1], tMaxs[2])
    kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.
    tGrid = seq(0, blockSecs, by = 0.1)
    
    #  summarise Cond and Fun in this block
    if(bkIdx == 1){condByBlock[sIdx*2 - 1] = thisCond}
    if(bkIdx == 2){condByBlock[sIdx*2] = thisCond}
    if(bkIdx == 1){FunctionByBlock[sIdx*2 - 1] = thisFunction}
    if(bkIdx == 2){FunctionByBlock[sIdx*2] = thisFunction}
    
    # summarise earnings in this block
    if(bkIdx == 1){earningsByBlock[sIdx*2 - 1] = sum(thisTrialData$trialEarnings)}
    if(bkIdx == 2){earningsByBlock[sIdx*2] = sum(thisTrialData$trialEarnings)}

    # plot trial-by-trial data
    if (plotTrialwiseData) {
      trialPlots(thisTrialData,label)
    }
    
    # survival analysis
    tMax = ifelse(thisCond == "unif16", 16, 32)# time window for the survival analysis
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
    wtwCeiling =ifelse(thisCond == "unif16", 16, 32)# since trials exceeding this duration are infrequent
    # (e.g., 2 perfectly patient individuals could have different results depending what max time they got)
    wtwtsResults = wtwTS(thisTrialData, tGrid, wtwCeiling, label, plotWTW)
    if(bkIdx == 1) wtw[,sIdx * 2 -1] = wtwtsResults else wtw[,sIdx * 2] = wtwtsResults
    # ***Other possible metrics
    #     width or inflection point of the KMSC (sigmoid fit?)
    #     slope or lower bound of the WTW function. spline fit?
    #     2nd-half AUC
    
    # calculate timeEarnings 
    timeEarnings = getTimeEarnings(thisTrialData, tGrid, label, plotTimeEarnings)
    if(bkIdx == 1) cumEarn[,sIdx * 2 -1] = timeEarnings else cumEarn[,sIdx * 2] = timeEarnings
    
    
    # wait for input before continuing, if individual plots were requested
    if (any(plotScheduledDelays, plotTrialwiseData, plotKMSC, plotRT, plotWTW, plotTimeEarnings)) {
      readline(prompt = paste('subject',thisID, "block", bkIdx, '(hit ENTER to continue)'))
      graphics.off()
    }
  } # loop over blocks
}
# organize groupdata 
groupData = data.frame(id = rep(allIDs, each = 2), blockNum = rep(c(1,2), n),
                       cbal = rep(hdrData$Cbal, each = 2), condition = condByBlock,
                       trialFun = FunctionByBlock, AUC = grpAUC,
                       totalEarnings = earningsByBlock)


# compare AUC for different environments and trialFun
summarise(group_by(groupData, condition, trialFun), meanAUC =  mean(AUC))

# plot totalEarnings 
ggplot(groupData, aes(groupData$totalEarnings)) + geom_histogram(bins = 10) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of blocks") + myTheme + xlim(c(0, 600))
dir.create('figures')
ggsave("figures/earningExp.pdf", width = 8, height = 4)

# find maxmal and minimal waiting time if quit
holdOnTimesMin = rep(0, n)
for(sIdx in 1 : n){
  # pull this subject's data
  thisID = allIDs[sIdx]  
  thisTrialData = trialData[[thisID]]
  holdOnTimesMin[sIdx] = min(thisTrialData$timeWaited[thisTrialData$trialEarnings == 0])
  # hist(thisTrialData$timeWaited)
  # readline(prompt = paste('subject',thisID,'(hit ENTER to continue)'))
}
plotData = data.frame(holdOnTimesMin[holdOnTimesMin != max(holdOnTimesMin)], 
                      x = holdOnTimesMin[holdOnTimesMin != max(holdOnTimesMin)])

ggplot(plotData, aes(plotData$holdOnTimesMin)) + geom_histogram(bins = 10) + xlab("Time / sec") +
  ylab('Num of Participants') + ggtitle("Min Wait_Duration Before Quit") + saveTheme
ggsave("theme.pdf", width = 5, height = 4)

###### analyse cumEarn
idxSeq = 1 : (n * 2)
passiveIdx = idxSeq[groupData$condition == 'HP' & groupData$blockNum == 1]
useData = cumEarn[,passiveIdx]

meanValues =apply(useData, 1, mean)
stdValues = apply(useData, 1, sd)
minValues = meanValues - stdValues / sqrt(ncol(useData))
maxValues = meanValues + stdValues / sqrt(ncol(useData))
plotData = data.frame(meanValues, stdValues, minValues, maxValues, tGrid)

ggplot(plotData, aes(tGrid, meanValues)) + geom_line() +
  geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.5, color = "#bababa")+
  displayTheme

