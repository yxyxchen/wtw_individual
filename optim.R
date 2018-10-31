# this simulation using average payoff

### output file ####
outFile = 'QStarData'
################## read data #################
# library 
library('ggplot2')
library('dplyr')
library('tidyr')
source('simulate.R') # QStar model
source('wtwSettings.R') # wtw settings for both HP and LP
                        # can't change
source('getPara.R') # functions to get MSPara and otherPara from inputs and wtwSettings
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

# genrate
otherPara = getOtherPara(cond, stepDuration)
############# simulate for the distribution of toalEarnings ##########
nPara = 2
nValue = 6
nComb =  nValue^nPara
paraNames = c("tau", "gamma")
tMax = otherPara[['tMax']]
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(c(0.33, 1, 3, 9, 27, 81), each = nValue) # tau
initialSpace[,2] = rep(seq(0.05, 0.95, length.out = 6), nValue) # gamma
outFile = 'QStarData'
fileName = sprintf('%s/initialSpace.RData', outFile)

save(initialSpace,nPara, nValue, paraNames,nComb, file = fileName) 
# set seed
set.seed(123)

# simualte 
nRep = 5
TrialEarnings = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
RewardDelays = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
Ws = array(dim = c(nValue^nPara, nRep, tMax / stepDuration))
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

if(cond == "unif16") rawHPData = outputData else rawLPData = outputData   
fileName = sprintf('%s/rawData.RData', outFile)
save(rawHPData,rawLPData, file = fileName) 


######## generate hdrData ######
# hdrData include otherPara, MSPara
# also nTimeStep and TraceValue 
# therefore, no need to call getPara in later analysis anymore
stepDuration = 0.5
source("getPara.R")
for(c in 1: 2){
  cond = conditions[c]
  otherPara = getOtherPara(cond, stepDuration)
  hdrData = c(otherPara)
  hdrData$nTimeStep = hdrData$tMax / hdrData$stepDuration
  if(cond == 'unif16') hdrHPData= hdrData else  hdrLPData= hdrData
}
fileName = sprintf('%s/hdrData.RData', outFile)
save(hdrHPData, hdrLPData, file = fileName)


