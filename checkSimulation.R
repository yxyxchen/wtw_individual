source("getPara.R")
source("simulate.R")

###### simulate ############
cond = 'unif16'
MSPara = getMSPara(cond, 0.5, 10)
otherPara = getOtherPara(cond, 0.5)
para = c(0.5, 0.1, 0.3, 0.2, 2)
# simulate
outputs = simulate(para, MSPara, otherPara, cond)

######## organize outputs #######
check = data.frame(trialEarnings = outputs$trialEarnings,
                   timeWaited = outputs$timeWaited,
                   rewardDelays = outputs$rewardDelays)
# fill NAs in timeWaited 
check$timeWaitedFill = check$timeWaited
check$timeWaitedFill[check$trialEarnings == 5] =
  check$rewardDelays[check$trialEarnings == 5] 
# curtail the dataframe
check = check[1 : (match( 0, check$rewardDelays) - 1), ]
# get cumTimeWaited 
# time until the end of the time step
isi = rep(2, length(check$trialEarnings))
isi[1] = 0
check$cumTimeWaited = cumsum(check$timeWaitedFill + isi)


