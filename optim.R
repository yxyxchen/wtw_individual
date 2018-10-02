# this simulation using average payoff

################## read data #################
# library 
library('ggplot2')
library('dplyr')
library('tidyr')
source('simulate.R')
source('getPara.R')
############# game environments 
iti = 2
blockMins = 15
blockSecs = blockMins * 60
################ fixed Para ################
# condition
cond = "unif16"
#cond = "logspace_1.75_32"
sprintf('Condition : %s', cond)
condName = ifelse(cond == 'unif16', "HP", "LP")

# other input
stepDuration = 0.5
nMS = 10

# output
otherPara = getOtherPara(cond, stepDuration)
MSPara = getMSPara(cond, stepDuration, nMS)
############# simulate for the distribution of toalEarnings ##########
nPara = 5
nValue = 3
tMax = otherPara[['tMax']]
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.2, 0.8, 0.3), each = nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^2), nValue^(nPara - 3)) 
initialSpace[,4] = rep(rep(seq(0.2, 0.8, 0.3), each = nValue^3), nValue^(nPara - 4)) 
initialSpace[,5] = rep(rep(seq(2, 8, 3), each = nValue^4), nValue^(nPara - 5)) 

# set seed
set.seed(123)
# simualte 
nRep = 5
TrialEarnings = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
RewardDelays = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
Ws = array(dim = c(nValue^nPara, nRep, nMS))
TimeWaited = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
for(j in 1 : nRep){
  for(i in 1:nrow(initialSpace)){
    para = initialSpace[i,]
    tempt=  simulate(para,MSPara, otherPara, cond)
    TrialEarnings[i, j,] = tempt[['trialEarnings']]
    Ws[i, j,] = tempt[['ws']]
    RewardDelays[i, j,] = tempt[['rewardDelays']]
    TimeWaited[i, j, ] = tempt[['timeWaited']]
  }  
}

# organize outputs 
outputData = list("ws" = Ws, "timeWaited" = TimeWaited,
                 "rewardDelays" = RewardDelays, "trialEarnings" = TrialEarnings 
                 )
if(cond == "unif16"){
  rawHPData = outputData 
}else{
  rawLPData = outputData   
}

# save
fileName = sprintf('QStarData/raw%sData.Rdata', condName)
if(cond == "unif16"){
  save(rawHPData,file = fileName)
}else{
  save(rawLPData,file = fileName) 
}


TotalEarnings = matrix(NA, nValue^nPara, nRep)
for(j in 1 : nRep){
  for(i in 1:nrow(initialSpace)){
    TotalEarnings[i, j] = sum(TrialEarnings[i, j, ])
  }
}
############# find optimal paras using optim
# set a more sparse 
initialSpace = matrix(NA, 4^4, 4)
initialSpace[,1] = rep(seq(0.1, 1, 0.3), each = 5^3)
initialSpace[,2] = rep(rep(seq(0.1, 1, 0.2), each = 5^2), 5)
initialSpace[,3] = rep(rep(seq(0.1, 1, 0.2), each = 5), 5^2)
initialSpace[,4] = rep(seq(0.1, 1, 0.2), 5^3)
# optimResuts = vector("list", nrow(initialSpace))
for(i in 1:nrow(initialSpace)){
  optimResuts[[i]] = optim(initialSpace[i,], optimGoal, gr = NULL, MSPara = MSPara, otherPara = otherPara,
        cond = cond, lower = c(0, 0, 0, 0), upper = c(1, 1, 1, 1), method = 'L-BFGS-B')

}

system.time({
  optim(initialSpace[i,], optimGoal, gr = NULL, MSPara = MSPara, otherPara = otherPara, nRep = 5,
                           cond = cond, lower = c(0, 0, 0, 0), upper = c(1, 1, 1, 1), method = 'L-BFGS-B') 
}
)




