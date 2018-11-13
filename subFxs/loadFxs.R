
# loadData.R
# varying-magnitude WTW

# each subject has:
#  header file (*hdr.txt)
#  main data file (ending both in .mat and .txt)
#  empty *keytimes.txt file, a vestige of the effort key-pressing version. 


loadAllData = function() {

  ##### step 1: load all header files
  # look for header files in the data directory
  dataDir = './data'
  headerFileList = list.files(path=dataDir, pattern=glob2rx('wtw-work-3_*_1_hdr.txt'))
  nFiles = length(headerFileList)
  cat('Identified',nFiles,'header files.\n')
  
  # load the header files
  id = character(nFiles)
  cbal = numeric(nFiles)
  bk1_distrib = character(nFiles)
  bk2_distrib = character(nFiles)
  bk1_trial_function = character(nFiles)
  bk2_trial_function = character(nFiles)
  for (fIdx in 1:nFiles) {
    thisFile = file.path(dataDir,headerFileList[fIdx])
    d = read.table(thisFile, sep=":", as.is=TRUE, strip.white=TRUE, row.names=1)
    id[fIdx] = d['ID',1]
    cbal[fIdx] = as.numeric(d['cbal',1])
    bk1_distrib[fIdx] = d['bk1 distrib',1]
    bk2_distrib[fIdx] = d['bk2 distrib',1]
    if (cbal[fIdx] == 1 | cbal[fIdx] == 3){
      bk1_trial_function[fIdx] = 'passive'
      bk2_trial_function[fIdx] = 'active'
    }else{
      bk1_trial_function[fIdx] = 'active'
      bk2_trial_function[fIdx] = 'passive'      
    }
  }
  
  # store header info for all subjects in a data frame
  hdrData = data.frame(ID=id, Cbal=cbal, Condition1=bk1_distrib, Condition2=bk2_distrib,
                       Function1 = bk1_trial_function, Function2 = bk2_trial_function,
                       stringsAsFactors=FALSE)
  cat('Subjects per counterbalance condition:\n') # display the number per condition
  print(table(hdrData$Cbal))
  if (length(unique(hdrData$ID)) != nFiles) {  # ensure all IDs are unique
    cat('Possible duplicate subject IDs')
    browser()
  }
  
  ##### step 2: load all data files
  # define data column names
  colnames = c('blockNum', 'trialNum', 'trialStartTime', 'nKeyPresses', 'scheduledWait',
               'rewardTime', 'timeWaited', 'sellTime', 'trialEarnings','totalEarnings')
      # 'timeWaited' is from trial onset to keypress (includes RT)
      # 'sellTime' is the keypress time relative to block onset.
  
  # initialize
  trialData = list()

  # loop over individual subjects
  for (fIdx in 1:nFiles) {
    
    # extract the subject ID
    thisID = hdrData$ID[fIdx]
    thisCbal = hdrData$Cbal[fIdx]
    thisFile = list.files(path=dataDir, pattern=(sprintf('wtw-work-3_%s_1.txt',thisID)))
    if (length(thisFile) != 1) {
      cat('Could not identify a single data file for subject',thisID,'\n')
      browser()
    }
    
    # load the behavioral data file
    d = read.csv(file.path(dataDir,thisFile), header=FALSE, col.names=colnames)
    
    # add a column for subject ID. 
    d$id = thisID # this repeats the id in all rows
    
    # filter blockNum < 3
    d = d[d$blockNum < 3, ]
    
    # add a column for the condition
    # %   cb1: HP, passive-active
    # %   cb2: HP, active-passive
    # %   cb3: LP, passive-active
    # %   cb4: LP, active-passive
    if (thisCbal==1 | thisCbal == 2) {d$condition = 'HP'}
    else {    d$condition = 'LP'}
    
    if (thisCbal==1 | thisCbal == 3) {passiveIdx = (d$blockNum == 1)}
    else  {passiveIdx  = (d$blockNum == 2)}
    d$trial_function[passiveIdx] = 'passive'
    d$trial_function[!passiveIdx] = 'active'

    # add to the list of all subjects' data
    trialData[[thisID]] = d
  } # end of loop over subjects
  
  # return the 2 data frames in a named list
  outputData = list(hdrData=hdrData, trialData=trialData)
  return(outputData)
  
} # end of function

