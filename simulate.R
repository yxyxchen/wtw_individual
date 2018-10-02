# add number of repeation
simulate= function(para, MSPara, otherPara, cond){
  # 
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = para[4]
  wIni = para[5]

  source('taskFxs.R')
  # taskPara
  itiSecs = 2
  blockMins = 15
  blockSecs = blockMins * 60
  
  # read otherPara
  tMax= otherPara[['tMax']]
  stepDuration = otherPara[['stepDuration']]
  timeTicks = otherPara[['timeTicks']] # begin timepoint of states
  nTimeStep = tMax / stepDuration
  
  # read MSPara
  nMS = MSPara$nMS
  sigma = MSPara[['sigma']]
  MSMus = MSPara[['MSMus']]
  traceDecay = MSPara[['traceDecay']]
  # define trace values of step t as 0.95^(t - 1)
  # we can also define it as 0.95^t, it is abitrary
  traceValues = traceDecay ^ ((1 : nTimeStep) - 1)
  
  # actionList
  actionList = c('wait', 'quit') # wait means wait until t+1, quit means quit at t
  
  ########### simulation repeatedly ############
  # initialize action value, eligibility trace and stat
  ws = rep(0, nMS) # weight vector for "wait"
  ws[1] = wIni # encourage explore at first
  es = rep(0, nMS); # es vector for "wait"
  onsetXs = dnorm(traceValues[1], MSMus, sigma) * sigma * traceValues[1]
  xs = onsetXs
  
  # initialize time and reward seq
  totalSecs = 0
  seq = c()
  rewardDelays = rep(0, blockSecs / iti + 1)
  tIdx = 0
  stepGap = 1 # since es = 0 initially, so this value is abitratry
  
  # initialize outputs 
  trialEarnings = rep(0, blockSecs / iti + 1)
  timeWaited = rep(0, blockSecs / iti + 1)
  
  # loop until time runs out
      while(totalSecs <= blockSecs) {
        tIdx = tIdx + 1
        # sample rewardDelay
        rewardOutputs = drawSample(cond, seq)
        rewardDelay = rewardOutputs[['delay']]
        seq = rewardOutputs[['seq']]
        
        # calculaye available time steps
        # since we use floor there maybe 0.5 sec error 
        nAvaStep = min(floor((blockSecs - totalSecs) / stepDuration), nTimeStep)
        
        # if near the block end, the check total Secs everytime
        for(t in 1 : nAvaStep){
          # calculte action value 
          vaQuit = onsetXs %*% ws * gamma^(itiSecs / stepDuration) # didn't consider iTi, to stop getting things to complex
          vaWait = xs %*% ws;
          # determine action
          waitRate = exp(vaWait * tau) / sum(exp(vaWait) * tau + exp(vaQuit) * tau)
          # when gamma is large, sometimes vaWait will be very large, sothat waitRate is NA
          if(is.na(waitRate)){
            waitRate = 1
          }
          action = ifelse(runif(1) < waitRate, 'wait', 'quit')
          
          # next reward 
          # determine whether reward occurs in the step t
          # the previous code is wrong, since rewards happens on 16s seconds woudldn't be counted 
          rewardOccur = rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t] 
          
          # if rewarded and wait, 5; otherwise, 0
          nextReward = ifelse(action == 'wait' && rewardOccur, 5, 0) 
          
          # dertime next state
          # go to the terminate state if at the final step or quit or reward arrives
          trialGoOn= (action == 'wait' && !rewardOccur && t < nAvaStep)
          if(trialGoOn){
            nextXs = dnorm(traceValues[t + 1], MSMus, sigma) * sigma * traceValues[t + 1]
          }else{
            nextXs  = onsetXs
          }
          
          # update eligilibity trace
          # here stepGap meatured between At and At-1
          es =  gamma^stepGap * lambda * es + xs * (action == "wait")
          
          # update stepGap
          stepGap = ifelse(trialGoOn, 1, itiSecs / stepDuration)
          
          # update action value of quit and wait
          # here stepGap meatured between At and At+1
          delta = nextReward + gamma^(stepGap) * max(nextXs %*% ws, vaQuit) - ifelse(action == 'wait', vaWait, vaQuit)
          ws = ws + phi * delta * es
          
          # update xs and stepGap
          xs = nextXs
          
          # break the trial didn't continue
          # return output
          if(!trialGoOn){
            trialEarnings[tIdx] = ifelse(nextReward == 5, 5, 0);
            # if quit, quit at t, if wait, wait until t+1
            timeWaited[tIdx] = ifelse(rewardOccur,  NA, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
            rewardDelays[tIdx] = rewardDelay
            break
          }
        }  # one trial end
        totalSecs = totalSecs + itiSecs + ifelse(rewardOccur, rewardDelay, timeWaited[tIdx])
      } # simulation end
      outputs = list("ws" = ws,
                     "trialEarnings" = trialEarnings,
                     "timeWaited" = timeWaited,
                     "rewardDelays" = rewardDelays)
      return(outputs)
} #end of the function


