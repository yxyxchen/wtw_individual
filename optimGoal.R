# add number of repeation
optimGoal = function(para, MSPara, otherPara, cond, nRep = 1){
  # 
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = para[4]

  source('taskFxs.R')
  # taskPara
  itiSecs = 2
  blockMins = 15
  blockSecs = blockMins * 60
  
  # read MSPara
  sigma = MSPara[['sigma']]
  MSMus = MSPara[['MSMus']]
  traceDecay = MSPara[['traceDecay']]
  # define trace values of step t as 0.95^(t - 1)
  # we can also define it as 0.95^t, it is abitrary
  traceValues = traceDecay ^ ((1 : nTimeStep) - 1)
  
  # read otherPara
  holdOnSteps = otherPara[['holdOnSteps']]
  stepDuration = otherPara[['stepDuration']]
  timeTicks = otherPara[['timeTicks']] # begin timepoint of states
  
  # actionList
  actionList = c('wait', 'quit')
  
  ########### simulation repeatedly ############
  # initialize outputs
  outputBuffer = rep(NA, nRep)
  s = 0
  while(s < nRep){
    s = s + 1
    # initialize action value, eligibility trace and stat
    ws = rep(0, nMS) # weight vector for "wait"
    es = rep(0, nMS); # es vector for "wait"
    onsetXs = dnorm(traceValues[1], MSMus, sigma) * sigma * traceValues[1]
    xs = onsetXs
    
    # initialize time and reward seq
    totalSecs = 0
    seq = c()
    rewardDelays = rep(0, 400)
    tIdx = 0
    
    # initialize outputs 
    trialEarnings = rep(0, 400)
    timeWaited = rep(0, 400)
    
    # loop until time runs out
    while (totalSecs < blockSecs) {
      tIdx = tIdx + 1
      # sample rewardDelay
      rewardOutputs = drawSample(cond, seq)
      rewardDelay = rewardOutputs[['delay']]
      seq = rewardOutputs[['seq']]
      for(t in 1 : nTimeStep){
        # calculte action value 
        vaQuit = onsetXs %*% ws * gamma # didn't consider iTi, to stop getting things to complex
        vaWait = xs %*% ws;
        if(t > holdOnSteps){
          # determine action
          waitRate = exp(vaWait * tau) / sum(exp(vaWait) * tau + exp(vaQuit) * tau)
          # when gamma is large, sometimes vaWait will be very large, sothat waitRate is NA
          if(is.na(waitRate)){
            waitRate = 1
          }
          action = ifelse(runif(1) < waitRate, 'wait', 'quit')
        }else{
          action = 'wait'  
        }
        
        # next reward 
        # determine whether reward occurs in the step t
        rewardOccur = rewardDelay < timeTicks[t + 1] &&
          rewardDelay >= timeTicks[t] 
        # if rewarded and wait, 5; otherwise, 0
        nextReward = ifelse(action == 'wait' && rewardOccur, 5, 0) 
        
        # dertime next state
        # go to the terminate state if at the final step or quit or reward arrives
        if(action == 'wait' && !rewardOccur && t < nTimeStep){
          nextXs = dnorm(traceValues[t + 1], MSMus, sigma) * sigma * traceValues[t + 1]
        }else{
          nextXs  = onsetXs
        }
        
        # update eligilibity trace
        es =  gamma * lambda * es + xs * (action == "wait")
        
        # update action value of quit and wait
        delta = nextReward + gamma * max(nextXs %*% ws, vaQuit) - ifelse(action == 'wait', vaWait, vaQuit)
        ws = ws + phi * delta * es
        
        # update xs
        xs = nextXs
        
        # break if arrives at onsetXs
        # return output
        if(action == 'quit' || rewardOccur || t == nTimeStep){
          trialEarnings[tIdx] = ifelse(nextReward == 5, 5, 0);
          timeWaited[tIdx] = ifelse(rewardOccur,  NA, timeTicks[t + 1])
          rewardDelays[tIdx] = rewardDelay
          break
        }
      }  # loop across timesteps
      # update totalTimes
      totalSecs = totalSecs + itiSecs + ifelse(rewardOccur, rewardDelay, timeWaited[tIdx])
    } # loop across trials
    # since time runs out at the last trial 
    outputBuffer[s] = sum(trialEarnings[2 : length(trialEarnings)]);
  }# end of simulations
  return(mean(outputBuffer))
  # outputs = list("ws" = ws, "totalEarnings" = sum(trialEarnings[2 : length(trialEarnings)]))
  # return(outputs)
  
}# end of the function
