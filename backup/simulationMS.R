# simulation for passive condition
# microstimuli stimulation

# library 
source('learningModel.R')
source('helperFxs.R')
source('load.R')
library('ggplot2')
library('dplyr')
library('tidyr')
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
# divide continous time into discrete time steps 
tMax = 50;# nearly all participants quit after 50
nTimeStep = 1000;
timeTicks = seq(0, tMax, length.out = nTimeStep + 1)
# on each time step t
# if reward occurs between timeTicks[t] to timeTicks[t+1]
# next reward = 5


# determine trace values for each time steps
traceDecay = 0.985 
# define trace values of step t as 0.95^(t - 1)
# we can also define it as 0.95^t, it is abitrary
traceValues = traceDecay ^ ((1 : nTimeStep) - 1)

# prepare microstimuli
nMS = 10; # number of microstimuli
junk = seq(1, traceValues[length(traceValues)], length.out = nMS)# mean of the basis function for each MS
MSMus = vector(length = nMS);
MSTimeSteps = vector(length = nMS);
for(i in 1 : length(junk)){
  MSTimeSteps[i] = order(abs(traceValues - junk[i]))[1]
  MSMus[i] = traceValues[MSTimeSteps[i] ]
}
  
# learning para
para = list()
para[['phi']] = 0.1
para[['tau']] = 2 # increas with sensity 
para[['gamma']] = 0.8
para[['timeTicks']] = timeTicks
para[['lambda']] = 0.8

# learning environment
actionList = c('wait', 'quit')
terminateXs = rep(0, nMS)
sigma = 0.2; # sd for basis function

# learning model 
phi = para[['phi']]# learning rate
tau = para[['tau']] # soft-max temperature para
gamma = para[['gamma']] # discounting rate
lambda = para[['lambda']] # decay rate
timeTicks = para[['timeTicks']] # begin timepoint of states

# initialize outputs 
trialEarnings = rep(0, nTrial)
timeWaited = rep(0, nTrial)



for(tIdx in 1 : nTrial) {
  # initialize 
  ws = matrix(0, nMS, 2) # weight vector
  ws[, actionList == 'wait'] = rep(100, nMS)
  es = rep(0, nMS);
  xs = dnorm(traceValues[1], MSMus, sigma) * sigma * traceValues[1];
  for(t in 1 : nTimeStep){
    # calculte action value 
    vaQuit = xs %*% ws[,actionList == 'quit'];
    vaWait = xs %*% ws[,actionList == 'wait'];
    # determine action
    waitRate = exp(vaWait * tau) / sum(exp(vaWait) * tau + exp(vaQuit) * tau)
    action = ifelse(runif(1) < waitRate, 'wait', 'quit')

    # next reward 
    # determine whether reward occurs in the step t
    rewardOccur = rewardTimes[tIdx] < timeTicks[t + 1] &&
      rewardTimes[tIdx] >= timeTicks[t] 
    # if rewarded and wait, 5; otherwise, 0
    nextReward = ifelse(action == 'wait' && rewardOccur, 5, 0) 
    
    # dertime next state
    # go to the terminate state if at the final step or quit or reward arrives
    if(action == 'wait' && timeTicks[t+1] < rewardTimes[tIdx] && t < nTimeStep){
      nextXs = dnorm(traceValues[t + 1], MSMus, sigma) * sigma * traceValues[t + 1]
    }else{
      nextXs  = terminateXs
    }

    # update es
    es = es + gamma * lambda * xs;
    # update ws
    delta = nextReward + gamma * max(nextXs %*% ws) - xs %*% ws[,actionList %in% action];
    ws[,actionList %in% action] = ws[,actionList %in% action] +
      phi * delta * es
    
    # update xs
    xs = nextXs
    
    # break if arrives terminateXs
    # return output
    if(sum(xs == terminateXs) == length(xs)){
      trialEarnings[tIdx] = ifelse(nextReward == 5, 0, 1);
      timeWaited[tIdx] = mean(timeTicks[t : (t + 1)])
      break
    }
    
  }  
}

input = thisTrialData
input$timeWaited = timeWaited
input$trialEarnings = trialEarnings
kmGrid = seq(0, 20, by=0.1) # grid on which to average survival curves.
kmscResults = kmsc(input,tMax,label,T,kmGrid)
label = sprintf('balabala)')
