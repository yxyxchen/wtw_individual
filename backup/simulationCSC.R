# simulation using CSC


# library 
source('learningModel.R')
source('load.R')

# load data 
allData = load()
trialData = allData$trialData
hdrData = allData$hdrData

# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
cat('Analyzing data for n','=',n,'subjects.\n')
nTrial = length(trialData$trialNum)

# analyse only passive condition
trialData = trialData[trialData$trial_function == 'passive',]

# initialize simulation results
simulateResults = list()

# search space
phiSpace = seq(0.1, 0.9, length.out = 5)
tauSpace = seq(1, 50, length.out = 10) # small means flat
gammaSpace = seq(0.1, 0.9, length.out = 5)

# statList
tMax = 51 # 50.524 is the longest time subjects wait
nState = 20;
stateList = seq(0, tMax, length.out = nState) # change later
# number of simulation
nSimulation = 10

for(sIdx in 1 : 2){
  thisID = allIDs[sIdx]
  thisTrialData = trialData[[thisID]]
  # only use passive condition
  thisTrialData = thisTrialData[thisTrialData$trial_function== 'passive',]
  thisCond = unique(trialData$condition)
  label = sprintf('Subject %s, Cond %d)',thisID, thisCond)
  cat(label)
  
  # initialize
  para = list()
  
  para[['stateList']] = stateList
  
  # search para space
  for(i in 1 : length(phiSpace)){
    para[['phi']] = phiSpace[i]
      for(j in 1 : length(tauSpace)){
        para[['tau']] = tauSpace[j]
        for(z in 1 : length(gammaSpace)){
          para[['gamma']] = gammaSpace[z]
          
          simulateResults[[thisID]] = learningModel(para, thisTrialData$scheduledWait, nSimulation)  
        
          
        }
      }
 
  }
}




