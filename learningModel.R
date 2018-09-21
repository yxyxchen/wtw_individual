
learningModel = function(para, rewardTime, nSimulation) {
  # para
  phi = para[['phi']]# learning rate
  tau = para[['tau']] # soft-max temperature para
  gamma = para[['gamma']] # discounting rate
  stateList = para[['stateList']] # begin timepoint of states
  actionList = c('wait', 'quit')
  
  # initialize output
  output = list()
  output[['qs']]  = matrix(0, 20, 2)
  nTrial = length(rewardTime)
  output[['quitStates']]  = rep(0, nTrial)
  
  # simulation
  for(s in 1 : nSimulation){
    # initializing
    qs = matrix(0, 20, 2)
    quitStates = rep(0, nTrial)
    
    # loop for all trials
    for (trialIdx in 1 : nTrial){
      thisRewardTime = rewardTime[trialIdx]
      
      # loop for all states before quit 
      for (stateIdx in 1 : (nState - 1) ){ # nState -1 means never wait more than 51s
        
        # choose action
        waitRate = exp(qs[stateIdx, actionList == 'wait']) / sum(exp(qs[stateIdx, ]))
        
        # if wait 
        if(runif(1) < waitRate) {
          actionIdx = (actionList == 'wait')
          stateList[stateIdx]
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
        
        # if quit
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
  }
  
  # update average qs and quitStates
  output[['qs']] = output[['qs']] + (qs - output[['qs']]) / s
  output[['quitState']] = output[['quitState']] + (quitStates - output[['quitState']]) / s
  return(output)
}


