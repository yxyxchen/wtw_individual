getEV = function(gamma, cIdx){
  source('wtwSettings.R')
  
  # get timing input
  stepDuration = 0.5
  condName = conditionNames[cIdx];
  timing = timings[[condName]]
  tMax = tMaxs[cIdx]
  trialTick = seq(0, tMax, by = stepDuration)
  nTimeStep = length( trialTick )
  
  
  outputs = vector(length = nTimeStep)
  for(i in 1 : nTimeStep){
    timePoint = trialTick[i]
    gammaExp =  (timing - timePoint) / 4
    outputs[i] = sum(tokenValue * gamma ^ (gammaExp[gammaExp >= 0])) / sum(timing > timePoint )
  }
  return(outputs)
}

HP = getEV(0.9, 1)

LP = getEV(0.9, 2)

p1 = data.frame(va = HP, time = 1 : length(HP))
p2 = data.frame(va = LP, time = 1 : length(LP))
plotData = rbind(p1, p2)
plotData$condition = c(rep("HP", length(HP)), rep("LP", length(LP)))
ggplot(plotData, aes(time, va)) + geom_point() + xlab('Time step') + ylab("vaWait") + facet_wrap(.~condition)

