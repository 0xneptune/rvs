###############################################################################
# Risky vs Safe Decision Making Game Data Analysis
#
# AUTHORS: Computational Affective Neuroscience (CAN) Lab, U of T
#
###############################################################################

# TODO: These should be read in from the CVS.
largeGainValue = 10
smallGainValue = 2
largeLossValue = -9
smallLossValue = -1 

workingDirectoryPath = getwd()
csvDirectoryPath = paste(workingDirectoryPath, "data", sep = "/")
rawDataInquisitPath = paste(csvDirectoryPath, "rvs_v4_rawdata_inquisit.csv",
                            sep = "/")

rawDataInquisit = read.csv(rawDataInquisitPath)
subjectIds = rawDataInquisit["subject"]
blockCodeValues = rawDataInquisit["blockcode"]

for (index in 1:lengths(blockCodeValues)) {
  if (!identical(blockCodeValues[index], "attentionQuiz")) {
    print("Skipping")
    next
  }
  
  print(blockCodeValues[index])
  subjectId = subjectIds[index]
}


