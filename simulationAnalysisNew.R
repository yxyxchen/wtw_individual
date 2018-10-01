# this script is for 

# library
library("ggplot2")
library("dplyr")
library("tidyr")
library("Scale")
library('scales')

# initialSpace
nPara = 5
nValue = 3
nComb = nValue ^ nPara
tMax = otherPara[['tMax']]
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.2, 0.8, 0.3), each = nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^2), nValue^(nPara - 3)) 
initialSpace[,4] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^3), nValue^(nPara - 4)) 
initialSpace[,5] = rep(rep(seq(2, 8, 3), each = nValue^4), nValue^(nPara - 5)) 

# load totalEarnings data
load('HPTotalEarningsWS.RData')
HPRewardDelays = RewardDelays
HPTimeWaited = TimeWaited
HPTrialEarnings = TrialEarnings
HPWs = Ws
rm("Ws", "RewardDelays", "TimeWaited", "TrialEarnings")
load('LPTotalEarningsWS.RData')
LPRewardDelays = RewardDelays
LPTimeWaited = TimeWaited
LPTrialEarnings = TrialEarnings
LPWs = Ws
rm("Ws", "RewardDelays", "TimeWaited", "TrialEarnings")

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
HPEarnings = apply(HPTrialEarnings, FUN = sum, MARGIN = c(1,2))
LPEarnings = apply(LPTrialEarnings, FUN = sum, MARGIN = c(1,2))
meanHP = rowSums(HPEarnings) / ncol(HPEarnings)
meanLP = rowSums(LPEarnings) / ncol(LPEarnings)

plotData = data.frame(totalEarnings = c(meanHP, meanLP),
                      condition = rep(c("HP", "LP"), each = nComb), phi = initialSpace[,1],
                      tau = initialSpace[,2], gamma = initialSpace[,3],
                      lambda = initialSpace[,4], wIni = initialSpace[,5]
)

ggplot(plotData, aes(totalEarnings)) + geom_histogram(bins = 15) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of simulations") + myTheme + xlim(c(0, 600)) +
  ylim(c(0, 55))
#dir.create("new_figures")
ggsave("new_figures/earningSml.pdf", width = 16, height = 8)

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
  vaQuits =  matrix(NA, nTimeStep, nrow(initialSpace))
  vaWaits = matrix(NA, nTimeStep, nrow(initialSpace))
  
  
  for(p in 1 : nrow(initialSpace)){
    thisPara = initialSpace[p, ]
    thisOutputs = optimGoal(thisPara,MSPara, otherPara, cond)
    
    thisWs = thisOutputs[['ws']]
    ws[,p] = thisWs;
    trialEarnings[,p] = thisOutputs[['TrialEarnings']]
    rewardDelays[,p] = thisOutputs[['rewardDelays']]
    timeWaited[,p]  = thisOutputs[['timeWaited']]
    
    thisXs = apply(as.matrix(traceValues), FUN = function(x) dnorm(x, MSMus, sigma) * sigma * x, MARGIN = 1)
    vaWaits[,p] = as.vector(thisWs %*% thisXs)
    vaQuits[,p] = rep(vaWaits[1,p] * thisPara[3], nTimeStep) 
  }
  
  # output
  if(cond == 'unif16'){
    HPTrialEarnings = trialEarnings
    HPRewardDelays = rewardDelays 
    HPTimeWaited = timeWaited
    HPWs = ws
    HPVaQuits = vaQuits
    HPVaWaits = vaWaits
  }else{
    LPTrialEarnings = trialEarnings
    LPRewardDelays = rewardDelays 
    LPTimeWaited = timeWaited
    LPWs = ws
    LPVaQuits = vaQuits
    LPVaWaits = vaWaits   
  }
}



# select para combs 
meanEarnings = ifelse(rep(cond == 'unif16', length(meanHP)), meanHP, meanLP)
nComb = floor(length(meanEarnings) * 0.1);

# plot
vas = rbind(vaWaits, vaQuits)
meanValues = apply(vas, FUN = mean, MARGIN = 1)
stdValues = apply(vas, FUN = sd, MARGIN = 1)
maxValues = meanValues + stdValues
minValues = meanValues - stdValues
actions = factor(rep(c('wait', 'quit'), each = nTimeStep), levels = c('wait', 'quit'))
plotData = data.frame(meanValues, stdValues, maxValues, minValues, 
                      timeSteps = seq(1,nTimeStep), actions  )
graphics.off()
titleText = sprintf("%s, %s",condName, paraCat)
ggplot(plotData, aes(timeSteps, meanValues, linetype = actions)) + 
  geom_ribbon(data = plotData[plotData$actions == 'wait',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
  geom_ribbon(data = plotData[plotData$actions == 'quit',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") + 
  geom_line(color = conditionColors[c], size = 1) + 
  xlab('Time step') + ylab('Action value') + ggtitle(titleText)+ myTheme + scale_linetype_discrete(name = "Action")

fileName = sprintf('figures/actionValue%s%s.pdf', condName, ranking)
ggsave(file = fileName, width = 10, height = 6)
