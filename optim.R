# simulation for passive condition
# set action value of "quit" for all state as zero
# also direct the end of trials to the onset of new trials

################## read data #################
# library 
source('learningModel.R')
source('helperFxs.R')
source('load.R')
library('ggplot2')
library('dplyr')
library('tidyr')
source('optimGoal.R')
# load data 
allData = load()
trialData = allData$trialData
hdrData = allData$hdrData

# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
cat('Analyzing data for n','=',n,'subjects.\n')
nTrial = length(trialData$trialNum)

# use only one subject here
sIdx = 1
thisID = allIDs[sIdx]
thisTrialData = trialData[[thisID]]

# analyse only passive condition
thisTrialData = thisTrialData [thisTrialData$trial_function == 'passive',]
rewardTimes = thisTrialData$scheduledWait
nTrial = length(rewardTimes)

########## setting for TD model with eligibility trace #########
# time steps
tMax = 32;
nTimeStep = tMax * 2;
timeTicks = seq(0, tMax, length.out = nTimeStep + 1);

# don't quit for holdOnsetp
holdOnSteps = 5

# pass to otherPara
otherPara = list()
otherPara[['timeTicks']] = timeTicks
otherPara[['holdOnSteps']] = holdOnSteps

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

##### 
phi = 0.1
tau = 0.1 # increas with sensity 
gamma = 0.8
lambda = 0.5

optim(c(0.1, 2, 0.8, 0.5), optimGoal, gr = NULL, MSPara = MSPara, otherPara = otherPara,
      cond = 'unif16', lower = c(0, 0, 0, 0), upper = c(1, 1, 1, 1))



