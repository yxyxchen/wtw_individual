i = 100
j = 1
checkData = data.frame(rewardDelay = rawLPData$rewardDelays[i,j,],
                       timeWaited = rawLPData$timeWaited[i,j,],
                       trialEarning = rawLPData$trialEarnings[i,j,])


checkData = data.frame(rewardDelay = tempt$rewardDelays,
                       timeWaited = tempt$timeWaited,
                       trialEarning = tempt$trialEarnings)
# curtial
endTick = match(0, checkData$rewardDelay)
checkData = checkData[1 : (endTick - 1), ]

# quit immedietely 
tempt = checkData$timeWaited == 0
sum(tempt[!is.na(tempt)])
sum(tempt[!is.na(tempt)]) / (endTick - 1) # ratio

