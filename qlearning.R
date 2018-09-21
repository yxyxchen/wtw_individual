
# qlearning for the wtw task

# library
source('load.R')

# load data of subject 045
allData = load()
trialData = allData$trialData$`045`
trialData = trialData[trialData$trial_function == 'passive',]
nTrial = length(trialData$trialNum)

# para
phi = 0.5 # learning rate
tau = 2 # soft-max temperature para
gamma = 0.1 # discounting rate
tMax = 51 # 50.524 is the longest time subjects wait
nState = 20;
stateList = seq(0, tMax, length.out = nState) # change later
actionList = c('wait', 'quit')

# input 
rewardTime = trialData$scheduledWait

  
# initializing
qs = matrix(0, 20, 2)
quitStates = rep(0, nTrial)


# loop
for (trialIdx in 1 : nTrial){
  thisRewardTime = rewardTime[trialIdx]
  
  for (stateIdx in 1 : (nState - 1) ){ # nState -1 means never wait more than 51s
    # choose action
    waitRate = exp(qs[stateIdx, actionList == 'wait']) / sum(exp(qs[stateIdx, ]))
    
    # if wait 
    if(runif(1) < waitRate) {
      actionIdx = (actionList == 'wait')
      # maybe rewarded if wait
      if( thisRewardTime < stateList[stateIdx + 1] && thisRewardTime > stateList[stateIdx]){
        reward = 5     
      }else{
        reward = 0
      }
      # update state value
      qs[stateIdx, actionIdx] = qs[stateIdx, actionIdx] +
        phi * (reward + gamma * max(qs[stateIdx + 1, ]) - qs[stateIdx, actionIdx])
      # must quit in the final state
      quitStates[trialIdx] = nState
    }
    else{ 
      actionIdx = actionList == 'quit'
      # never rewarded if quit
      reward = 0
      # update state value
      qs[stateIdx, actionIdx] = qs[stateIdx, actionIdx] + phi * ( reward - qs[stateIdx, actionIdx])
      quitStates[trialIdx] = stateIdx
      break
      }
  }
}




