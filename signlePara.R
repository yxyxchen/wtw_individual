library('ggplot2')
library('dplyr')
library('tidyr')
source('simulate.R') # QStar model
source('wtwSettings.R') # wtw settings for both HP and LP
source('getPara.R') # functions to get MSPara and otherPara from inputs and wtwSettings
source('helperFxs.R')
################ selec condition ################
# cond input
condIdx = 2
cond = conditions[condIdx];
condName = conditionNames[condIdx]
condColor = conditionColors[condIdx]
sprintf('Condition : %s %s', cond, condName)
stepDuration = 0.5
otherPara = getOtherPara(cond, stepDuration)
tMax = otherPara[['tMax']]
trialTick = trialTicks[[condName]]

# initial space
nPara = 5
nValue = 3
tMax = otherPara[['tMax']]
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.2, 0.8, 0.3), each = nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(8,24, 8), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(rep(seq(0.90, 0.98, 0.04), each = nValue^2), nValue^(nPara - 3)) 
initialSpace[,4] = rep(rep(seq(0.90, 0.98, 0.04), each = nValue^3), nValue^(nPara - 4)) 
initialSpace[,5] = rep(rep(seq(2, 8, 3), each = nValue^4), nValue^(nPara - 5)) 

########### simulate #############
combIdx = 160
rIdx = 2
stepDuration = 0.5;

#  [1] 160 192 195 196 197 198 213 215 217 218 219 220 221 222 223 224 225 226 227
# 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243
para = initialSpace[combIdx,] 
para[3] = 0.90
para[5] = 1
tempt = QStarModel(para, MSPara, otherPara, cond)

# summarise earnings, AUC, wtw 
totalEarnings = sum(tempt$trialEarnings)

waitDuration = tempt$timeWaited
rewardDelay = tempt$rewardDelays
quitIdx = (tempt$trialEarnings == 0)
waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
endTick = match(0,rewardDelay)
waitDuration = waitDuration[1 : (endTick - 1)]
quitIdx = quitIdx[1 : (endTick - 1)]
kmscResults =  kmscSimple(waitDuration, quitIdx, tMax, trialTick)
AUC = kmscResults$auc
label = sprintf('earn: %d, AUC: %.2f',
                totalEarnings,AUC)

# plot trialData
blockData = data.frame(trialEarnings = tempt$trialEarnings,
                       scheduledWait = tempt$rewardDelays,
                       timeWaited = tempt$timeWaited,
                       trialNum = 1 : length(tempt$timeWaited)
)
endTick = match(0, tempt$rewardDelays) - 1
blockData = blockData[1:endTick, ]
trialPlots(blockData, label)

# plot survival
plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks[[condName]])
ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
  ggtitle(label)

# check ws
# 
vaWaits = tempt$vaWaits
vaQuits = tempt$vaQuits

wIni = para[5]
gamma = para[3]
nTimeStep = otherPara$tMax / otherPara$stepDuration

vaWaits[is.na(vaWaits[,1]),1] = rep(wIni, sum(is.na(vaWaits[,1])))
vaQuits[is.na(vaQuits[,1]),1] = rep(wIni * gamma ^ (iti / stepDuration),
                                    sum(is.na(vaQuits[,1])))

for(i in 2 : endTick){
  vaWaits[is.na(vaWaits[,i]),i] = vaWaits[is.na(vaWaits[,i]),i-1]
  vaQuits[is.na(vaQuits[,i]),i] = vaQuits[is.na(vaQuits[,i]),i-1]
}

for(i in 2 : endTick){
  cIdx = i
  plotData = data.frame(va =c(vaWaits[,cIdx], vaQuits[,cIdx]),
                        time = rep( 1 : (otherPara$tMax / otherPara$stepDuration), 2),
                        action = rep(c('wait', 'quit'),
                                     each = otherPara$tMax / otherPara$stepDuration))
  label = sprintf('last, rwd = %d, tw = %.2f; rwd = %d, tw =%.2f',
                  tempt$trialEarnings[i-1], waitDuration[i-1],
                  tempt$trialEarnings[i], waitDuration[i])
  p = ggplot(plotData, aes(time, va, color = action)) + geom_line() + ggtitle(label) + xlab('step')
  # plotData$waitProb = waitProb[,i]
  # p = ggplot(plotData[plotData$action == 'wait',], aes(time, waitProb)) + geom_line()+
  #   ggtitle(label) + xlim(c(1,3)) + xlab('step')
  print(p)
  readline(prompt = paste(i, '(hit ENTER to continue)'))
}


# probability of wait
tau = para[2]
waitProb = exp(vaWaits * tau) / (exp(vaWaits*tau) + exp(vaQuits* tau))
plotData$waitProb = waitProb[,endTick]
ggplot(plotData[plotData$action == 'wait',], aes(time, waitProb)) + geom_line()

# quikQuit
endTick = match(0, tempt$rewardDelays) - 1
sum(tempt$timeWaited[1:endTick] < 1 & !is.na(tempt$timeWaited[1:endTick]))




