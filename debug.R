##### cond 
source('optimGoal.R')
cond = 'log_175_32'
#cond = 'unif16'

##### setting for TD model
# time steps
tMax = ifelse(cond == 'unif16', 16, 32)
stepDuration = 0.5;
nTimeStep = tMax / (stepDuration);
timeTicks = seq(0, tMax, length.out = nTimeStep + 1);

# don't quit for holdOnsetp
holdOnSteps = 5

# pass to otherPara
otherPara = list()
otherPara[['timeTicks']] = timeTicks
otherPara[['holdOnSteps']] = holdOnSteps
otherPara[['stepDuration']] = stepDuration

########### setting for MS representation ##########
# trace decay rate
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

#####
initialSpace = matrix(NA, 5^4, 4)
initialSpace[,1] = rep(seq(0.1, 1, 0.2), each = 5^3)
initialSpace[,3] = rep(rep(seq(0.1, 1, 0.2), each = 5), 5^2)
initialSpace[,4] = rep(seq(0.1, 1, 0.2), 5^3)

# select para
para = initialSpace[20,]
para = LPPara
  
# 20 is good for unif
# 100 is a good one for log 
# 
outputs = optimGoal(para,MSPara, otherPara, cond)
ws = outputs[['ws']]
totalEarnings = outputs[['totalEarnings']]

vaWaits = rep(NA, nTimeStep)
for(t in 1 : nTimeStep){
  xs = dnorm(traceValues[t], MSMus, sigma) * sigma * traceValues[t]
  vaWaits[t] = xs %*% ws
  
}
graphics.off()
vaQuits = rep(vaWaits[1] * para[3], nTimeStep)
plotData = data.frame(step = rep(1 : nTimeStep, 2), actionValue = c(vaWaits, vaQuits),
                      action = rep(c('wait', 'quit'), each = nTimeStep))
ggplot(plotData, aes(x = plotData$step, y = plotData$actionValue, color = action)) + geom_point()  +
  ylab('Action Value') + xlab("Time step") + myTheme + ggtitle('LP')
ggsave('figures/lp.pdf', width = 6, height = 4)




