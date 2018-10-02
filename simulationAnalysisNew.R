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
rawHPData = list(rwardDelays = RewardDelays,
                    timeWaited = TimeWaited,
                    trialEarnings = TrialEarnings,
                    ws = Ws[,,1:10])
rm("Ws", "RewardDelays", "TimeWaited", "TrialEarnings")

load('LPTotalEarningsWS.RData')
rawLPData = list(rwardDelays = RewardDelays,
                    timeWaited = TimeWaited,
                    trialEarnings = TrialEarnings,
                    ws = Ws[,,1:10])
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

### analysis parameters 
conditions = c('unif16', "log_1.75_32")
conditionNames = c("HP", "LP")
conditionColors = c("#7b3294", "#008837")

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
save(hdrHPData, hdrLPData, file = 'QStarData/hdrData.RData')

####### get HPData and LPData
for(c in 1:2){
  cond = conditions[c]
  if(cond == 'unif16'){
    inputData = rawHPData 
    hdrData = hdrHPData
  }else{
    inputData  = rawLPData
    hdrData = hdrLPData
  }
 
  # earnings and ws
  tempt = apply(inputData$trialEarnings, FUN = sum, MARGIN = c(1,2))
  earnings = rowSums(tempt) / ncol(tempt)
  ws = apply(inputData$ws, c(1,3), mean)
  
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
  survivalProbs = apply(waitProbs, MARGIN =  2, FUN= cumprod)
  
  # outpput
  outputData = list( "earnigs" = earnings, "ws" = ws,
                     "vaQuits" = vaQuits, "vaWaits" = vaWaits, 
                     "waitProbs" = waitProbs, "survivalProbs" = survivalProbs)
  if(cond == 'unif16') HPData = outputData  else  LPData = outputData 
}
dir.create('QStarData')
save(HPData, LPData, file = 'QStarData/data.RData')

############ plot distribution of earnings 
plotData = data.frame(totalEarnings = c(HPData$earnigs, LPData$earnigs),
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
range(HPData$earnigs)
range(LPData$earnigs)

############ summarise effect of different parameters ###########
nPara = 5
nValue = 3
paraNames = c("phi", "tau", "gamma", "lambda", "wIni")
paraValues = seq(0.2, 0.8, 0.3) 
summaryData = data.frame(condition = rep(c("HP", "LP"), each = nValue, nPara),
                         paraNames = rep(paraNames, each = nValue * 2),
                         paraValues = rep(paraValues, nPara * 2))
summaryData$paraNames = factor(summaryData$paraNames, levels = paraNames)
# summarise mu and sd
mu = rep(NA, nrow(summaryData))
std = rep(NA, nrow(summaryData))
tempt = summarise(group_by(plotData, condition, phi), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[1:6] = tempt$mu; std[1:6] = tempt$std
tempt = summarise(group_by(plotData, condition, tau), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[7:12] = tempt$mu; std[7:12] = tempt$std
tempt = summarise(group_by(plotData, condition, gamma), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[13:18] = tempt$mu; std[13:18] = tempt$std
tempt = summarise(group_by(plotData, condition, lambda), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[19:24] = tempt$mu; std[19:24] = tempt$std
tempt = summarise(group_by(plotData, condition, wIni), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[25:30] = tempt$mu; std[25:30] = tempt$std
summaryData$mu = mu
summaryData$std = std
summaryData$ymin = mu - std
summaryData$ymax = mu + std

# plot for HP

for(c in 1:2){
  cond = conditionNames[c]
  ggplot(summaryData[summaryData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.2)+
    facet_wrap(~paraNames, nrow = 1)+ myTheme +
    xlab("Parameter value") + ylab("Total Earnings") + ggtitle(cond) 
  fileName = sprintf("new_figures/paraEffect%s.pdf", cond)
  ggsave(fileName, width = 16, height = 8) 
}


############ look at ws group by totalEanrings #########
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
    ggplot(plotData, aes(timeSteps, meanValues, linetype = actions))+
      geom_ribbon(data = plotData[plotData$actions == 'wait',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_ribbon(data = plotData[plotData$actions == 'quit',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") + 
      geom_line(color = conditionColors[c], size = 1) + 
      xlab('Time step') + ylab('Action value') + ggtitle(titleText)+ myTheme + scale_linetype_discrete(name = "Action") + 
      coord_cartesian(ylim=c(-2,5)) 
    
    fileName = sprintf('new_figures/actionValue%s%s.pdf', condName, ranking)
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
    ggplot(plotData, aes(timeSteps, meanValues))+
      geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_line(color = conditionColors[c], size = 1) + 
      xlab('Time step') + ylab('Wait prob') + ggtitle(titleText)+ myTheme +  coord_cartesian(ylim=c(0.3, 0.8)) 
    
    fileName = sprintf('new_figures/waitProb%s%s.pdf', condName, ranking)
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
    ggplot(plotData, aes(timeSteps, meanValues))+
      geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
      geom_line(color = conditionColors[c], size = 1) +  coord_cartesian(ylim=c(0,1)) +
      xlab('Time step') + ylab('Surivival Prob') + ggtitle(titleText)+ myTheme 
    
    fileName = sprintf('new_figures/survialProb%s%s.pdf', condName, ranking)
    ggsave(file = fileName, width = 10, height = 6)
  }
}


### look at trial earnings 
