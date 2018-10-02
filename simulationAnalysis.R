# this script is for 

# library
library("ggplot2")
library("dplyr")
library("tidyr")
library("Scale")
library('scales')
source("getPara.R")

# initialSpace
nValue = 5
nPara = 4
initialSpace = matrix(NA, 5^4, 4)
initialSpace[,1] = rep(seq(0.1, 1, 0.2), each = 5^3)
initialSpace[,2] = rep(rep(seq(0.1, 1, 0.2), each = 5^2), 5)
initialSpace[,3] = rep(rep(seq(0.1, 1, 0.2), each = 5), 5^2)
initialSpace[,4] = rep(seq(0.1, 1, 0.2), 5^3)

###### get hdrData
nMS = 10
stepDuration = 0.5
source("getPara.R")
for(c in 1: 2){
  cond = conditions[c]
  otherPara = getOtherPara(cond, stepDuration)
  MSPara = getMSPara(cond, stepDuration, nMS)  
  hdrData = c(otherPara, MSPara)
  hdrData$nTimeStep = hdrData$tMax / hdrData$stepDuration
  hdrData$traceValues = hdrData$traceDecay ^ ( 1 :   hdrData$nTimeStep - 1)
  if(cond == 'unif16') hdrHPData= hdrData else  hdrLPData= hdrData
}
dir.create("QLearnData")
save(hdrHPData, hdrLPData, file = 'QLearnData/hdrData.RData')


# load other data 
conditions = c("unif16", "log_1.75_32")
load("QFullSimul.Rdata")
for(c in 1:2){
  cond = conditions[c]
  if(cond == 'unif16'){
    earnings = colSums(HPTrialEarnings)
    ws = t(HPWs)
    hdrData = hdrHPData
  }else{
    earnings = colSums(LPTrialEarnings)
    ws = t(LPWs)
    hdrData = hdrLPData
  }
  
  #
  xs = apply(as.matrix(hdrData$traceValues), MARGIN  = 1, FUN = function(x)
    dnorm(x, hdrData$MSMus, hdrData$sigma)*hdrData$sigma * x) 
  vaWaits = t(ws %*% xs)    
  vaQuits = vaWaits[1,] * initialSpace[,3]^4
  
  temptVaQuits = matrix(rep(vaQuits, each = hdrData$nTimeStep), nrow =hdrData$nTimeStep) 
  taus = initialSpace[,3]
  taus = matrix(rep(taus, each = hdrData$nTimeStep), nrow = hdrData$nTimeStep)
  # wait probability 
  
  waitProbs =   1 / (1 + exp((temptVaQuits - vaWaits) * taus))
  waitProbs[1 : hdrData$holdOnSteps, ] = 1 # holdonsteps]
  survivalProbs = apply(waitProbs, MARGIN =  2, FUN= cumprod)
  
  # outpput
  outputData = list( "earnigs" = earnings, "ws" = ws,
                     "vaQuits" = vaQuits, "vaWaits" = vaWaits, 
                     "waitProbs" = waitProbs, "survivalProbs" = survivalProbs)
  if(cond == 'unif16') HPData = outputData  else  LPData = outputData 
}
dir.create('QStarData')
save(HPData, LPData, file = 'QStarData/data.RData')

# plot theme 
myTheme = theme(panel.background = element_rect(fill = "white",colour = "white"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 2),
        axis.line.y = element_line(color="black", size = 2))+ 
  theme(axis.title=element_text(size=25), title =element_text(size=30, face='bold'), 
        plot.title = element_text(hjust = 0.5)) + 
  theme(axis.title=element_text(size=25), title =element_text(size=25, face='bold'),
        axis.text = element_text(size=25), axis.line= element_line(color="black", size = 2)) +
  theme(strip.text = element_text(face="bold", size=20)) + 
  theme(legend.text=element_text(size= 25))


############ plot distribution of earnings 
########### using raw data
# load totalEarnings data
load('HPTotalEarnings.RData')
HPEarnings = totalEarnings
rm(totalEarnings)
load('LPTotalEarnings.RData')
LPEarnings = totalEarnings

meanHP = rowSums(HPEarnings) / ncol(HPEarnings)
meanLP = rowSums(LPEarnings) / ncol(LPEarnings)
plotData = data.frame(totalEarnings = c(manHP, meanLP),
                      condition = rep(c("HP", "LP"), each = 625), phi = initialSpace[,1],
                      tau = initialSpace[,2], gamma = initialSpace[,3],
                      lambda = initialSpace[,4]
                      )

ggplot(plotData, aes(totalEarnings)) + geom_histogram(bins = 15) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of simulations") + myTheme + xlim(c(0,600))
ggsave("figures/earningSml.pdf", width = 8, height = 4)

# calculate range
range(meanHP)
range(meanLP)

############ summarise effect of different parameters ###########
paraNames = c("phi", "tau", "gamma", "lambda")
paraValues = seq(0.1, 0.9, 0.2) 
summaryData = data.frame(condition = rep(c("HP", "LP"), each = 5, 4),
                         paraNames = rep(paraNames, each = 10),
                         paraValues = rep(paraValues, 8))
summaryData$paraNames = factor(summaryData$paraNames, levels = paraNames)

# summarise mu and sd
mu = rep(NA, nrow(summaryData))
std = rep(NA, nrow(summaryData))
tempt = summarise(group_by(plotData, condition, phi), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[1:10] = tempt$mu; std[1:10] = tempt$std
tempt = summarise(group_by(plotData, condition, tau), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[11:20] = tempt$mu; std[11:20] = tempt$std
tempt = summarise(group_by(plotData, condition, gamma), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[21:30] = tempt$mu; std[21:30] = tempt$std
tempt = summarise(group_by(plotData, condition, lambda), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[31:40] = tempt$mu; std[31:40] = tempt$std
summaryData$mu = mu
summaryData$std = std
se = std / sqrt((5 ^ 3))
summaryData$ymin = mu - std
summaryData$ymax = mu + std

# plot for HP
conditionNames = c("HP", "LP")
conditionColors = c("#7b3294", "#008837")
for(c in 1:2){
  cond = conditionNames[c]
  ggplot(summaryData[summaryData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + facet_wrap(~paraNames, nrow = 1)+
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.2) + myTheme +
    xlab("Parameter value") + ylab("Total Earnings") + ggtitle(cond) +
    coord_cartesian(ylim=c(100,500)) 
  fileName = sprintf("figures/paraEffect%s.pdf", cond)
  ggsave(fileName, width = 16, height = 8) 
}

############ look at ws group by totalEanrings #########
# para
source('getPara.R')
source('optimGoal.R')
# input
stepDuration = 0.5
nMS = 10

conds = c('unif16', 'log_1.75_32')
condNames = c('HP', 'LP')
for(c in 1 : 2){
  cond = conds[c]

  # get Para
  MSPara = getMSPara(cond, stepDuration, nMS)
  otherPara = getOtherPara(cond, stepDuration)
  
  nTimeStep = otherPara[['tMax']] / otherPara[['stepDuration']]
  traceValues =  MSPara[['traceDecay']] ^ ((1 : nTimeStep) - 1)
  MSMus = MSPara[['MSMus']]
  sigma = MSPara[['sigma']]
  
  # initial outputs
  trialEarnings = matrix(NA, 400, nrow(initialSpace))
  rewardDelays = matrix(NA, 400, nrow(initialSpace))
  timeWaited = matrix(NA, 400, nrow(initialSpace))
  ws = matrix(NA, nMS, nrow(initialSpace))
  #vaQuits =  matrix(NA, nTimeStep, nrow(initialSpace))
  #vaWaits = matrix(NA, nTimeStep, nrow(initialSpace))
  
  
  for(p in 1 : nrow(initialSpace)){
    
    thisPara = initialSpace[p, ]
    thisOutputs = optimGoal(thisPara,MSPara, otherPara, cond)
    thisWs = thisOutputs[['ws']]
    ws[,p] = thisWs;
    trialEarnings[,p] = thisOutputs[['trialEarnings']]
    rewardDelays[,p] = thisOutputs[['rewardDelays']]
    timeWaited[,p]  = thisOutputs[['timeWaited']]
    
    # thisXs = apply(as.matrix(traceValues), FUN = function(x) dnorm(x, MSMus, sigma) * sigma * x, MARGIN = 1)
    # vaWaits[,p] = as.vector(thisWs %*% thisXs)
    # vaQuits[,p] = rep(vaWaits[1,p] * thisPara[3], nTimeStep) 
  }
  
  # output
  if(cond == 'unif16'){
    HPTrialEarnings = trialEarnings
    HPRewardDelays = rewardDelays 
    HPTimeWaited = timeWaited
    HPWs = ws
    # HPVaQuits = vaQuits
    # HPVaWaits = vaWaits
  }else{
    LPTrialEarnings = trialEarnings
    LPRewardDelays = rewardDelays 
    LPTimeWaited = timeWaited
    LPWs = ws
    # LPVaQuits = vaQuits
    # LPVaWaits = vaWaits   
  }
}

fileName = 'QFullSimul.Rdata'
save(HPTrialEarnings, HPRewardDelays, HPTimeWaited, HPWs,
     LPTrialEarnings, LPRewardDelays, LPTimeWaited, LPWs,
    file = fileName)


######### plot action value
meanEarnings = ifelse(rep(cond == 'unif16', length(meanHP)), meanHP, meanLP)
perc = 0.1
nComb = nValue ^ nPara
nUse= floor(nComb * perc);
rankings = c('Top', 'Bottom')
conditionNames = c("HP", "LP")
conditionColors = c("#7b3294", "#008837")
for(c in 1 : 2){
  cond = conditions[c]
  condName = conditionNames[c]
  if(cond == 'unif16'){
    inputData = HPData
    hdrData = hdrHPData
  }else{
    inputData = LPData
    hdrData = hdrLPData
  }
  
  for(r in 1:2){
    ranking = rankings[r]
    tempt = order(inputData$earnigs, decreasing = (ranking == 'Top'))
    hist(inputData$earnigs[tempt[1:nUse]])
    UseVaWaits = inputData$vaWaits[,tempt[1:nUse]]
    UseVaQuits = matrix(rep(inputData$vaQuits[tempt[1:nUse]],
                            each = hdrData$nTimeStep), hdrData$nTimeStep)
    
    # plot
    vas = rbind(UseVaWaits, UseVaQuits)
    meanValues = apply(vas, FUN = mean, MARGIN = 1)
    stdValues = apply(vas, FUN = sd, MARGIN = 1)
    maxValues = meanValues + stdValues
    minValues = meanValues - stdValues
    actions = factor(rep(c('wait', 'quit'), each = hdrData$nTimeStep), levels = c('wait', 'quit'))
    plotData = data.frame(meanValues, stdValues, maxValues, minValues, 
                          timeSteps = rep(seq(1,hdrData$nTimeStep),2), actions  )
    graphics.off()
    titleText = sprintf("%s, %s%s total earnings",condName, ranking, percent(perc))
    ggplot(plotData[plotData$timeSteps > hdrData$holdOnSteps,], aes(timeSteps, meanValues, linetype = actions))+
      geom_ribbon(data = plotData[plotData$actions == 'wait' & plotData$timeSteps > hdrData$holdOnSteps,], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_ribbon(data = plotData[plotData$actions == 'quit'& plotData$timeSteps > hdrData$holdOnSteps,], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") + 
      geom_line(color = conditionColors[c], size = 1) + coord_cartesian(ylim=c(-3.5, 3.5)) + xlab('Time step') + ylab('Action value') + ggtitle(titleText)+ myTheme + scale_linetype_discrete(name = "Action") 
    
    fileName = sprintf('figures/actionValue%s%s.pdf', condName, ranking)
    ggsave(file = fileName, width = 10, height = 6)
  }
}

####### plot wait probility ########
perc = 0.1
nUse= floor(nComb* perc);
rankings = c('Top', 'Bottom')
for(c in 1 : 2){
  cond = conditions[c]
  condName = conditionNames[c]
  if(cond == 'unif16'){
    inputData = HPData
    hdrData = hdrHPData
  }else{
    inputData = LPData
    hdrData = hdrLPData
  }
  
  for(r in 1:2){
    ranking = rankings[r]
    tempt = order(inputData$earnigs, decreasing = (ranking == 'Top'))
    vas= inputData$waitProbs[,tempt[1:nUse]]
    
    # plot
    meanValues = apply(vas, FUN = mean, MARGIN = 1)
    stdValues = apply(vas, FUN = sd, MARGIN = 1)
    maxValues = meanValues + stdValues
    minValues = meanValues - stdValues
    plotData = data.frame(meanValues, stdValues, maxValues, minValues, 
                          timeSteps = seq(1,hdrData$nTimeStep), actions  )
    graphics.off()
    titleText = sprintf("%s, %s%s total earnings",condName, ranking, percent(perc))
    ggplot(plotData[plotData$timeSteps > hdrData$holdOnSteps,], aes(timeSteps, meanValues))+
      geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_line(color = conditionColors[c], size = 1) + 
      xlab('Time step') + ylab('Wait prob') + ggtitle(titleText)+ myTheme + coord_cartesian(ylim=c(0.3, 0.8)) 
    
    fileName = sprintf('figures/waitProb%s%s.pdf', condName, ranking)
    ggsave(file = fileName, width = 10, height = 6)
  }
}


####### survival 
perc = 0.1
nUse= floor(nComb* perc);
rankings = c('Top', 'Bottom')
for(c in 1 : 2){
  cond = conditions[c]
  condName = conditionNames[c]
  if(cond == 'unif16'){
    inputData = HPData
    hdrData = hdrHPData
  }else{
    inputData = LPData
    hdrData = hdrLPData
  }
  
  for(r in 1:2){
    ranking = rankings[r]
    tempt = order(inputData$earnigs, decreasing = (ranking == 'Top'))
    vas= inputData$survivalProbs[,tempt[1:nUse]]
    
    # plot
    meanValues = apply(vas, FUN = mean, MARGIN = 1)
    stdValues = apply(vas, FUN = sd, MARGIN = 1)
    maxValues = meanValues + stdValues
    minValues = meanValues - stdValues
    plotData = data.frame(meanValues, stdValues, maxValues, minValues, 
                          timeSteps = seq(1,hdrData$nTimeStep), actions  )
    graphics.off()
    titleText = sprintf("%s, %s%s total earnings",condName, ranking, percent(perc))
    ggplot(plotData[plotData$timeSteps > hdrData$holdOnSteps,], aes(timeSteps, meanValues))+
      geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_line(color = conditionColors[c], size = 1) + 
      xlab('Time step') + ylab('Survival prob') + ggtitle(titleText)+ myTheme +  coord_cartesian(ylim=c(0, 0.75)) 
    
    fileName = sprintf('figures/survialProb%s%s.pdf', condName, ranking)
    ggsave(file = fileName, width = 10, height = 6)
  }
}
