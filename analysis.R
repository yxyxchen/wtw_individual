# individual difference analysis
# Cyx


# analysis sub-functions
source('load.R')
source('helperFxs.R')

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
plotScheduledDelays = T
plotTrialwiseData = T
plotKMSC = T
plotRT = T

# initialize structures to hold group data
blockAUC = numeric(length=n)
# grpAUC = numeric(length=n)
grpAUC = matrix(NA, nrow=n, ncol=4)
earningsByBlock = matrix(NA, nrow=n, ncol=4)
kmGrid = seq(0, 20, by=0.1) # grid on which to average survival curves.
sc_rising1 = matrix(NA, nrow=n, ncol=length(kmGrid))
sc_falling1 = matrix(NA, nrow=n, ncol=length(kmGrid))
sc_rising2 = matrix(NA, nrow=n, ncol=length(kmGrid))
sc_falling2 = matrix(NA, nrow=n, ncol=length(kmGrid))
earningsHP = numeric(length=n)
earningsLP = numeric(length=n)
aucHP = numeric(length=n)
aucLP = numeric(length=n)

# descriptive statistics for individual subjects and blocks
for (sIdx in 1:n) {
  
  # pull this subject's data
  thisID = allIDs[sIdx]
  
  for (bkIdx in 1:4) {
    
    thisTrialData = trialData[[thisID]]
    thisBlockIdx = (thisTrialData$blockNum == bkIdx)
    thisTrialData = thisTrialData[thisBlockIdx,]
    thisCond = unique(thisTrialData$condition)
    label = sprintf('Subject %s, Block %d (Cbal %d)',thisID,bkIdx,hdrData$Cbal[sIdx])
    
    # earnings in this block
    earningsByBlock[sIdx, bkIdx] = sum(thisTrialData$trialEarnings)
    
    # plot and summarize the distribution of scheduled delays
    if (plotScheduledDelays) {
      scheduledDelays(thisTrialData,label)
    }
    
    # plot trial-by-trial data
    if (plotTrialwiseData) {
      trialPlots(thisTrialData,label)
    }
    
    # survival analysis
    tMax = 30 # time window for the survival analysis
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
    grpAUC[sIdx, bkIdx] = kmscResults[['auc']]
    # save the full AUC in the appropriate place
    if (thisCond=='rising' & bkIdx<=2) {sc_rising1[sIdx,] = kmscResults[['kmOnGrid']]}
    if (thisCond=='falling' & bkIdx<=2) {sc_falling1[sIdx,] = kmscResults[['kmOnGrid']]}
    if (thisCond=='rising' & bkIdx>=3) {sc_rising2[sIdx,] = kmscResults[['kmOnGrid']]}
    if (thisCond=='falling' & bkIdx>=3) {sc_falling2[sIdx,] = kmscResults[['kmOnGrid']]}
    
    # WTW time series - for an individual block. 
    tGrid = seq(0, 10*60, by=1) # time grid within the block
    wtwCeiling = 20 # since trials exceeding this duration are infrequent
    # (e.g., 2 perfectly patient individuals could have different results depending what max time they got)
    wtwtsResults = wtwTS(thisTrialData, tGrid, wtwCeiling)
    
    # RT as a function of preceding delay
    # rewardRT(thisTrialData, label, plotRT)
    
    
    # ***Other possible metrics
    #     width or inflection point of the KMSC (sigmoid fit?)
    #     slope or lower bound of the WTW function. spline fit?
    #     2nd-half AUC
    
    # wait for input before continuing, if individual plots were requested
    if (any(plotScheduledDelays, plotTrialwiseData, plotKMSC, plotRT)) {
      readline(prompt = paste('subject',thisID,'(hit ENTER to continue)'))
    }
    
    # temporary: inspect only a few subjects
    # if (sIdx>2) {break}
    
  } # loop over blocks