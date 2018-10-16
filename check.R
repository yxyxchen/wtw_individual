checkData = data.frame(rewardDelay = rawLPData$rewardDelays[100,1,],
                       timeWaited = rawLPData$timeWaited[100,1,],
                       trialEarning = rawLPData$trialEarnings[100,1,])

# curtial
endTick = match(0, checkData$rewardDelay)
checkData = checkData[1 : (endTick - 1), ]

# quit immedietely 
tempt = checkData$timeWaited == 0
sum(tempt[!is.na(tempt)])
sum(tempt[!is.na(tempt)]) / (endTick - 1) # ratio

