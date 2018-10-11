conditions = c("unif16", "log_1.75_32")
conditionNames = c("HP", "LP")
tMaxs = c(16, 32)
blockMins = 15
blockSecs = blockMins * 60
iti = 2
tokenValue = 5

# timings 
# HP 
HPTimings = seq(2, 16, by = 2)
# LP
n = 8
m = 32
fac = 1.75
d1 = log(m/(fac^n - 1))
d2 = log(m + m/(fac^n - 1))
tempt = exp(seq(d1,d2,length.out = n+1))
LPTimings = tempt[2:length(tempt)] - tempt[1]
timings = list(
  HP = HPTimings,
  LP = LPTimings
)
rm(m, fac, d1, d2, tempt, HPTimings, LPTimings)

# sampling ticks across the trial
trialTicks = list(
  HP = round(seq(0, tMaxs[1], by = 0.1), 1),
  LP = round(seq(0, tMaxs[2], by = 0.1), 1)
)

# p(t_reward <= T)
rewardDelayCDF = list(
 HP = approx(c(0, timings$HP), seq(0, 1, 1/n), xout = trialTicks$HP, method = "constant")$y, 
 LP = approx(c(0, round(timings$LP,1)), seq(0, 1, 1/n), xout = trialTicks$LP, method = "constant")$y
)

rewardDelayCDF$LP[length(trialTicks$LP)] = 1

#  p(t_reward = T)
HP = rep(0, length(trialTicks$HP))
HP[trialTicks$HP %in% timings$HP] = 1 / n 
LP = rep(0, length(trialTicks$LP))
# can't find ticks exactly equal to the timings 
# find cloest one 
LP[trialTicks$LP %in% round(timings$LP, 1)] = 1 / n 
rewardDelayPDF = list(
  "HP" = HP,
  "LP" = LP
)

trialTicks$LP[trialTicks$LP %in% round(timings$LP, 1)] 
# E(t_reward | t_reward <= T)
HP = cumsum(trialTicks$HP * rewardDelayPDF$HP) / cumsum(rewardDelayPDF$HP)
LP = cumsum(trialTicks$LP * rewardDelayPDF$LP) / cumsum(rewardDelayPDF$LP)
# values before the first possible timing spot is NAN
HP[trialTicks$HP < timings$HP[1]] = 0
LP[trialTicks$LP < round(timings$LP[1], 1)] = 0
meanRewardDelay = list(HP = HP, LP = LP)

# 
# plotData = data.frame(t = trialTicks$LP, y = LP)
#   ggplot(plotData, aes(t , y )) + geom_point()

# rewardRate
HP = tokenValue * rewardDelayCDF$HP /
  (meanRewardDelay$HP * rewardDelayCDF$HP + trialTicks$HP * (1 - rewardDelayCDF$HP) + iti)
LP = tokenValue * rewardDelayCDF$LP /
  (meanRewardDelay$LP * rewardDelayCDF$LP + trialTicks$LP * (1 - rewardDelayCDF$LP) + iti)
rewardRate = list(HP, LP)



