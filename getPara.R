
getOtherPara = function(cond, stepDuration){
  # time steps
  tMax = ifelse(cond == 'unif16', 16, 32)
  
  # check validity of step duration
  if(1 %% stepDuration != 0){
    return("stepDuration is not divisible")
  }
  
  # get nTimeStep and timeTicks
  nTimeStep = tMax / (stepDuration);
  timeTicks = seq(0, tMax, length.out = nTimeStep + 1);

  # pass to otherPara
  otherPara = list()
  otherPara[['tMax']] = tMax
  otherPara[['timeTicks']] = timeTicks
  otherPara[['stepDuration']] = stepDuration
  
  # return
  return(otherPara)
}

getMSPara = function(cond, stepDuration, nMS){
  tMax = ifelse(cond == 'unif16', 16, 32)
  nTimeStep = tMax / (stepDuration); 
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
  
  # return
  return(MSPara)
}


