data = {
  // specific data
  int<lower=0> n; // number of trials
  real<lower = 0> rewardDelays[n]; // reward delays
  int<lower = 0> trialEarnings[n]; 
  real<lower = 0> Waitedtimes[n];
  
  // task setting
  real<lower = 0> tMax; // trial duration
  real<lower = 0> stepDuration; //step duration
  real timeTicks[tMax / stepDuration] // time tick vector
  int<lower = 0> holdOnSteps
   
  // MS setting
  int<lower=0> nMS; //number of microstimuli
  real<lower = 0, upper = 1> traceDecay; // decay trace
  real<lower = 0> MSMus[nMS]; // center of microstimli base functions
  real<lower = 0> sigma; // sigma of mcirostimuli
}

parameters {
  real<lower = 0, upper = 1> phi; // learning rate
  real<lower=0> tau; // temperature para
  real<lower = 0, upper = 1> gamma; // discouting rate
  real<lower = 0, upper = 1> lambda; // decay rate
  real<lower = 0> Wini; // initial value of W
  }
  

transformed parameters{
  nStep = tMax / stepDuration; // num of steps
  traceValue = traceDecay ^ ((1 : nStep) - 1); // trace value for each time step
  
  real evQuits[n]; // estimation of evQuits
  real evWaits[n]; // estimation of evWaits 
  



}

model {

  
  
  for(i in 1 : n){
    rewardDelay = rewardDelays[i];
    trialEarning = trialEarning[i];
    
    
  }
}