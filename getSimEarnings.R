getSimEarnings = function(para, MSPara, otherPara, cond, nRep){
  library("simulate.R")
  EarningsList = rep(NA, nRep)
  for(r in 1 : nRep){
    tempt = simulate(para, MSPara, otherPara, cond);
    EarningsList[r] = tempt[["totalEarnings"]]
    
  }
}