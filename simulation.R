# this simulation using average payoff

### output file ####
outFile = 'outputs/QStarData'
################## read data #################
# library 
library('ggplot2')
library('dplyr')
library('tidyr')
source('model.R') # QStar model
source('subFxs/wtwSettings.R') # wtw settings for both HP and LP
                        # can't change
source('subFxs/paraFxs.R') # functions to get MSPara and otherPara from inputs and wtwSettings
                    # can change for different MS model, and 

################ input ################
# cond input
condIdx = 2
cond = conditions[condIdx];
condName = conditionNames[condIdx]
condColor = conditionColors[condIdx]
sprintf('Condition : %s %s', cond, condName)

# other input
stepDuration = 0.5
nMS = 10
traceDecay = 0.985
sigma = 0.2

# genrate
otherPara = getOtherPara(cond, stepDuration)
MSPara = getMSPara(cond, stepDuration, nMS, traceDecay, sigma)
############# simulate for the distribution of toalEarnings ##########
nPara = 5
paraNames = c('phi', 'tau', 'gamma', 'lambda', 'wIni')
nValue = 3
nComb = nValue ^ nPara
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(c(0.01, 0.05, 0.2), each = nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(8, 24, 8), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(rep(seq(0.90, 0.98, 0.04), each = nValue^2), nValue^(nPara - 3)) 
initialSpace[,4] = rep(rep(seq(0.90, 0.98, 0.04), each = nValue^3), nValue^(nPara - 4)) 
initialSpace[,5] = rep(rep(seq(2, 8, 3), each = nValue^4), nValue^(nPara - 5)) 
outFile = 'QStarData'
save('initialSpace', 'nValue', 'nPara', 'paraNames', 'nComb',
     file = sprintf('%s/initialSpace.RData', outFile))

# set seed
set.seed(123)

# simualte 
nRep = 5
tMax = otherPara[['tMax']]
TrialEarnings = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
RewardDelays = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
Ws = array(dim = c(nValue^nPara, nRep, nMS))
TimeWaited = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
vaQuits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, blockSecs / iti + 1))
vaWaits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, blockSecs / iti + 1))

for(i in 1 : 1:nrow(initialSpace)){
  para = initialSpace[i,]
  for(j in 1 : nRep ){
    tempt=  QStarModel(para,MSPara, otherPara, cond)
    TrialEarnings[i, j,] = tempt[['trialEarnings']]
    Ws[i, j,] = tempt[['ws']]
    RewardDelays[i, j,] = tempt[['rewardDelays']]
    TimeWaited[i, j, ] = tempt[['timeWaited']]
    vaQuits[i, j,  , ] = tempt[['vaQuits']]
    vaWaits[i, j, ,  ] = tempt[['vaWaits']]
  }  
}

# organize and save outputs 
outputData = list("ws" = Ws, "timeWaited" = TimeWaited,
                 "rewardDelays" = RewardDelays, "trialEarnings" = TrialEarnings,
                 "vaWaits" = vaWaits, "vaQuits" = vaQuits
                 )
outFile = 'QStarData'
if(cond == "unif16") rawHPData = outputData else rawLPData = outputData   
fileName = sprintf('outputs/%s/rawHPData.RData', outFile)
save(rawHPData,file = fileName) 

fileName = sprintf('outputs/%s/rawLPData.RData', outFile)
save(rawLPData,file = fileName) 
######## generate hdrData ######
# hdrData include otherPara, MSPara
# also nTimeStep and TraceValue 
# therefore, no need to call getPara in later analysis anymore
stepDuration = 0.5
source("getPara.R")
for(c in 1: 2){
  cond = conditions[c]
  otherPara = getOtherPara(cond, stepDuration)
  MSPara = getMSPara(cond, stepDuration, nMS, traceDecay, sigma)  
  hdrData = c(otherPara, MSPara)
  hdrData$nTimeStep = hdrData$tMax / hdrData$stepDuration
  hdrData$traceValues = hdrData$traceDecay ^ ( 1 :   hdrData$nTimeStep - 1)
  if(cond == 'unif16') hdrHPData= hdrData else  hdrLPData= hdrData
}
fileName = sprintf('outputs/%s/hdrData.RData', outFile)
save(hdrHPData, hdrLPData, file = fileName)

######## generate xsList ######
xsLists = list()
for(c in 1: 2){
  if(c == 1) hdrData = hdrHPData else hdrData = hdrLPData
  cond = conditions[c]
  nMS = hdrData$nMS
  traceValues = hdrData$traceValues
  MSMus = hdrData$MSMus
  sigma = hdrData$sigma
  nTimeStep = hdrData$nTimeStep
  
  output = matrix(NA, nTimeStep, nMS)
  for(i in 1 : nTimeStep){
    output[i,] = dnorm(traceValues[i], MSMus, sigma) * sigma * traceValues[i]
  }
  xsLists[[cond]]= output
}
fileName = sprintf('outputs/%s/xsLists.RData', outFile)
save('xsLists', file = fileName)
