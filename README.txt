


General Info:
This project apples reinforcement learning models to capture individual differences in the wtw task. A TrialFun(Passive / Negtive) x Condition(HP / LP) wtw dataset is used.
Four branches implements 4 versions of models:
	•		master: Q-learning Model with Microstimuli(MS).
	•		simple: Q-learning Model with Complete Serial Compound(CSC).
	•		monteMaster: Monte Carlo Model with MS and a  1/ learning rate.
	•		monteSimple: Monte Carlo Model with CSC and a 1/n learning rate.

Main Functions: 
	•	expAnalysis.R: analysis for experimental data
	•	model.R: learning model
	•	simulation.R: simulation procedure
	•	simPreprocess.R: preprocessing of simulation data, calculating AUC and WTW and average results across repetitive simulations
	•	simGroupAnalysis.R: group analysis for simulation data
	•	simCaseAnalysis.R: case analysis for simulation data

Sub Functions:
	•	loadFx.R: functions for loading experimental data
	•	helpferFxs.R: supplementary analysis functions, like functions for, WTW, plotting trial data eta. 
	•	taskFxs.R: generate random delays of rewards for the learning model. 
	•	wtwSettings.R: setting parameters in the task, like the block duration, the token value, reward rates for quitting at each time point eta. 
	•	plotThems.R: customized plot themes for ggplot2 
	•	ParaFxs.R: generate arguments needed for the learning model 
	•	actionValueViewer.R: plot action values trial by trial

PS:
the simple branch is only different from master in three files:
model.R
simulation.R (different length of ws)
actionValueViewer.R (different ways to refill action values)