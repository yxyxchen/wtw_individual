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
plotTimeEarnings = F
plotTrialEarnings = F


# initialize outputs, organised by block
grpAUC = numeric(length =n * 2)
earningsByBlock = numeric(length= n * 2)
condByBlock = vector(length= n * 2)
FunctionByBlock = vector(length= n * 2)
wtw = matrix(NA, 9001, n * 2)
cumEarn = matrix(NA, 9001, n * 2)

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
  thisID = allIDs[sIdx]
  # pull this subject's data
  for (bkIdx in 1:2){
    thisTrialData = trialData[[thisID]]
    thisCond = unique(thisTrialData$condition)
    thisBlockIdx = (thisTrialData$blockNum == bkIdx)
    thisTrialData = thisTrialData[thisBlockIdx,]
    thisFunction = unique(thisTrialData$trial_function)
    label = sprintf('Subject %s, Cond %s, Fun %s)',thisID, thisCond, thisFunction)
    
    #
    tMax = ifelse(thisCond == conditionNames[1], tMaxs[1], tMaxs[2])
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
    wtwCeiling = tMax
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
    
    plotData = data.frame(trialNum = thisTrialData$trialNum,
                          cumEarnings = cumsum(thisTrialData$trialEarnings))
    
    if(plotTrialEarnings){
      p = ggplot(plotData, aes(trialNum, cumEarnings)) + geom_line()   
      print(p)
    }

    
    # wait for input before continuing, if individual plots were requested
    if (any(plotScheduledDelays, plotTrialwiseData, plotKMSC, plotRT, plotWTW, plotTimeEarnings, plotTrialEarnings)) {
      readline(prompt = paste('subject',thisID, "block", bkIdx, '(hit ENTER to continue)'))
      graphics.off()
    }
  } # loop over blocks
}

# organize groupdata 
groupData = data.frame(id = rep(allIDs, each = 2), blockNum = rep(c(1,2), n),
                       cbal = rep(hdrData$Cbal, each = 2), condition = factor(condByBlock, levels = c('HP', 'LP')),
                       trialFun = FunctionByBlock, AUC = grpAUC,
                       totalEarnings = earningsByBlock)

### plot wtw
meanValues = c(apply(wtw[,groupData$condition == 'HP'], MARGIN = 1, FUN = mean), 
               apply(wtw[,groupData$condition == 'LP'], MARGIN = 1, FUN = mean))
stdValues = c(apply(wtw[,groupData$condition == 'HP'], MARGIN = 1, FUN = sd), 
               apply(wtw[,groupData$condition == 'LP'], MARGIN = 1, FUN = sd))
plotData = data.frame(meanValues, stdValues,
                      time = rep(tGrid, time = 2),
                      condition = rep(c('HP', 'LP'), each = length(tGrid)),
                      minValues = meanValues - stdValues / sqrt(ncol(wtw) / 2),
                      maxValues = meanValues + stdValues / sqrt(ncol(wtw) / 2))

ggplot(plotData, aes(time, meanValues)) +
  geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
  geom_line(aes(color = condition), size = 1) + facet_wrap(~condition) + 
  xlab('Time in block / s') + ylab('WTW / s') + saveTheme + scale_color_manual(values=conditionColors)

ggsave("exp_figures/wtwTimeseries.pdf", width = 12, height = 8)


########### aucCompare and wtwCompare
plotData = data.frame(optimWaitTime = rep(c(16, 3.1), each = nrow(groupData) / 2),
                      AUC = groupData$AUC,
                      condition = groupData$condition,
                      totalEarnings = groupData$totalEarnings,
                      wtw = colSums(wtw) / nrow(wtw))

plotData = plotData %>% arrange(totalEarnings) %>%group_by(condition) %>%
  mutate(earningRank = rank(totalEarnings, ties.method = "first"))

ggplot(plotData, aes(condition, AUC)) + geom_jitter(aes(color =  earningRank ), size = 4) +
  scale_color_gradient(low="red", high="yellow", name = 'Earning ranking') +
  geom_segment(aes(x= 0.7, xend = 1.3, y=optimWaitTimes$HP,yend=optimWaitTimes$HP), size = 2) +
  geom_segment(aes(x= 1.7, xend = 2.3, y=optimWaitTimes$LP,yend=optimWaitTimes$LP), size = 2) + saveTheme
ggsave("exp_figures/acuCompare.pdf", width = 12, height = 8)

# wtw
ggplot(plotData, aes(condition, wtw)) + geom_jitter(aes(color =  earningRank ), size = 4) +
  scale_color_gradient(low="red", high="yellow", name = 'Earning ranking') +
  geom_segment(aes(x= 0.7, xend = 1.3, y=optimWaitTimes$HP,yend=optimWaitTimes$HP), size = 2) +
  geom_segment(aes(x= 1.7, xend = 2.3, y=optimWaitTimes$LP,yend=optimWaitTimes$LP), size = 2) + saveTheme
ggsave("exp_figures/wtwCompare.pdf", width = 12, height = 8)

# compare mean wtw



###### compare AUC for best earn and worst earn
nUse = n * 0.1
plotData1 = summarise(group_by(groupData, condition),
                      meanData = mean(AUC[order(totalEarnings, decreasing = T)[1:nUse ]]),
                      stdData = sd(AUC[order(totalEarnings, decreasing = T)[1:nUse ]])
)
plotData2 = summarise(group_by(groupData, condition),
                     meanData = mean(AUC[order(totalEarnings)[1:nUse ]]),
                     stdData = sd(AUC[order(totalEarnings)[1:nUse ]])
                     )

plotData = rbind(plotData1, plotData2)
plotData$minData = plotData$meanData - plotData$stdData / sqrt(nUse )
plotData$maxData = plotData$meanData + plotData$stdData / sqrt(nUse )
plotData$earnRank = rep(c('Top10%', 'Bottom10%'), each = 2)
plotData$earnRank = factor(plotData$earnRank, levels = c('Top10%', "Bottom10%"))
plotData$optimWaitTime = optimWaitTimes$HP
plotData$optimWaitTime[plotData$condition == 'LP'] = optimWaitTimes$LP

ggplot(plotData, aes(earnRank, meanData, fill = condition)) + geom_bar(stat = 'identity', position = "dodge") + 
  geom_errorbar(aes(ymin = minData, ymax = maxData), width=.2)  + geom_hline(aes(yintercept = optimWaitTime))+ 
  facet_wrap(~condition) + scale_fill_manual(values= conditionColors) + xlab("") + ylab("AUC / s")  + saveTheme
ggsave("exp_figures/acuEarn.pdf", width = 12, height = 8)


##### plot aucLP & totalEarnings
ggplot(groupData[groupData$condition == 'LP',], aes(AUC, totalEarnings)) + geom_point()



##### plot total earnings
ggplot(groupData, aes(groupData$totalEarnings)) + geom_histogram(bins = 10) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of blocks") + myTheme + xlim(c(0, 600))
dir.create('figures')
ggsave("figures/earningExp.pdf", width = 8, height = 4)


