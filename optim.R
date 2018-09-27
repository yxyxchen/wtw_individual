# this simulation using average payoff

################## read data #################
# library 
library('ggplot2')
library('dplyr')
library('tidyr')
source('optimGoal.R')

######## determine condition #######
#cond = "unif16"
cond = "logspace_1.75_32"
sprintf('Condition : %s', cond)
condName = ifelse(cond == 'unif16', "HP", "LP")

########## setting for TD model with eligibility trace #########
# time steps
tMax = ifelse(cond == 'unif16', 16, 32)
stepDuration = 0.5;
nTimeStep = tMax / (stepDuration);
timeTicks = seq(0, tMax, length.out = nTimeStep + 1);

# don't quit for holdOnsetp
holdOnSteps = 5

# pass to otherPara
otherPara = list()
otherPara[['timeTicks']] = timeTicks
otherPara[['holdOnSteps']] = holdOnSteps
otherPara[['stepDuration']] = stepDuration

########### setting for MS representation ##########
# trace decay rate
traceDecay = 0.985 
traceValues = traceDecay ^ ((1 : nTimeStep) - 1)

# mu for MS
nMS = 10; # number of microstimuli
junk = seq(1, traceValues[length(traceValues)], length.out = nMS)# mean of the basis function for each MS
MSMus = vector(length = nMS);
MSTimeSteps = vector(length = nMS);
for(i in 1 : length(junk)){
  MSTimeSteps[i] = order(abs(traceValues - junk[i]))[1]
  MSMus[i] = traceValues[MSTimeSteps[i] ]
}

# temporal basis function
sigma = 0.2; # sd for basis function

# pass to MSPara
MSPara = list()
MSPara[['traceDecay']]  = traceDecay
MSPara[['MSMus']] = MSMus
MSPara[['sigma']]  = sigma

############# simulate for the distribution of toalEarnings ##########
initialSpace = matrix(NA, 5^4, 4)
initialSpace[,1] = rep(seq(0.1, 1, 0.2), each = 5^3)
initialSpace[,2] = rep(rep(seq(0.1, 1, 0.2), each = 5^2), 5)
initialSpace[,3] = rep(rep(seq(0.1, 1, 0.2), each = 5), 5^2)
initialSpace[,4] = rep(seq(0.1, 1, 0.2), 5^3)

# check optim.R output before use it  
nRep = 15
totalEarnings = matrix(NA, nrow(initialSpace), nRep)
for(j in 1 : nRep){
  for(i in 1:nrow(initialSpace)){
    para = initialSpace[i,]
    totalEarnings[i, j]  =  optimGoal(para,MSPara, otherPara, cond)
  }  
}
fileName = sprintf('%sTotalEarnings.Rdata', condName)
save(totalEarnings, file = fileName)

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




