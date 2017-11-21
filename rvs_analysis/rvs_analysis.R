###############################################################################
# Risky vs Safe Decision Making Game Data Analysis
#
# AUTHORS: Computational Affective Neuroscience (CAN) Lab, U of T
#
###############################################################################


###############################################################################
# CONSTANTS
###############################################################################


# TODO: These should be read in from the CVS as were assuming these are the
#       values the Inquisit program was using.
largeGainValue = 10
smallGainValue = 2
largeLossValue = -9
smallLossValue = -1


###############################################################################
# FUNCTIONS
###############################################################################


valueForAttentionQuiz <- function(arg1) {
  if (identical(arg1, "attentionQuizLargeGain")) {
    return(largeGainValue)
  } else if (identical(arg1, "attentionQuizSmallGain")) {
    return(smallGainValue)
  } else if (identical(arg1, "attentionQuizLargeLoss")) {
    return(largeLossValue)
  } else if (identical(arg1, "attentionQuizSmallLoss")) {
    return(smallLossValue)
  } else {
    # TODO: Return an error?
    return(-100)
  }
}


###############################################################################
# PROGRAM
###############################################################################

# Load and read the CSV
workingDirectoryPath = getwd()
csvDirectoryPath = paste(workingDirectoryPath, "data", sep = "/")
rawDataInquisitPath = paste(csvDirectoryPath, "rvs_v4_rawdata_inquisit.csv",
                            sep = "/")
qualtricsDataPath = paste(csvDirectoryPath,
                          "rvs_v4_qualitativedata_qualtrics.csv",
                          sep = "/")

rawDataInquisit = read.csv(rawDataInquisitPath, stringsAsFactors = FALSE)
qualtricsData   = read.csv(qualtricsDataPath, stringsAsFactors = FALSE)

# Data
subjectIds       = rawDataInquisit["subject"]
subjectIdsUnique = unique(unlist(subjectIds, use.names = FALSE))
blockCodeValues  = rawDataInquisit["blockcode"]
trialCodeValues  = rawDataInquisit["trialcode"]
responseValues   = rawDataInquisit["response"]

# TODO: Filter out subject ID's that are not a number.

# Task 1
# Determine the % of participants that got the stamp memory test correct

memoryTestAnswers = data.frame("subjectIds" = subjectIdsUnique,
                               correctAnswerPercentage = 0.0,
                               stringsAsFactors = FALSE)

for (index in 1:lengths(blockCodeValues)) {
  if (!identical(blockCodeValues[index,], "attentionQuiz")) {
    next
  }

  subjectId = subjectIds[index,]
  trialCode = trialCodeValues[index,]
  correctAnswer = valueForAttentionQuiz(trialCode)
  userAnswer = responseValues[index,]

  memoryTestAnswersCurrentIndexRow =
    memoryTestAnswers[memoryTestAnswers["subjectIds"] == subjectId,]
  
  if (userAnswer == correctAnswer) {
    memoryTestAnswers[memoryTestAnswers["subjectIds"] == subjectId,] =
      c(subjectId, memoryTestAnswersCurrentIndexRow[2] + 1)
  }
}

memoryTestAnswers = transform(
  memoryTestAnswers,
  correctAnswerPercentage = (correctAnswerPercentage / 4.0) * 100
)

str(memoryTestAnswers)
mean(memoryTestAnswers$correctAnswerPercentage)


# Task 2
# Compute the risky answers percentage for each participant.

percentageRiskySafeAnswers = data.frame("subjectIds" = subjectIdsUnique,
                                        percentageRisky = NA,
                                        percentageSafe  = NA,
                                        selfAnswer = NA)

for (subjectId in subjectIdsUnique) {
  dataOfSubject = rawDataInquisit[rawDataInquisit$subject == subjectId,]
  
  riskyCount = nrow(dataOfSubject[dataOfSubject$values.selectedoption == 'risky1' | dataOfSubject$values.selectedoption == 'risky2',])
  safeCount  = nrow(dataOfSubject[dataOfSubject$values.selectedoption == 'safe1'  | dataOfSubject$values.selectedoption == 'safe2',])
  
  riskyPercentage = (riskyCount / (riskyCount + safeCount)) * 100
  safePercentage  = (safeCount / (riskyCount + safeCount)) * 100
  
  percentageRiskySafeAnswers[percentageRiskySafeAnswers$subjectIds == subjectId, 'percentageRisky'] = riskyPercentage
  percentageRiskySafeAnswers[percentageRiskySafeAnswers$subjectIds == subjectId, 'percentageSafe']  = safePercentage
}

str(percentageRiskySafeAnswers)

for (subjectId in subjectIdsUnique) {
  selfAnswer = qualtricsData[qualtricsData$subject == subjectId, "Q207_1"]
  if (is.null(selfAnswer) | length(selfAnswer) <= 0) {
    next
  }
  
  percentageRiskySafeAnswers[percentageRiskySafeAnswers$subjectIds == subjectId, 'selfAnswer'] = selfAnswer
}

str(percentageRiskySafeAnswers)


# Task 4
# Reason for safe vs risky choice.

riskySafeChoiceReasonPercentages = data.frame("subjectIds" = subjectIdsUnique,
                                              safeAvoidLargeLoss = NA,
                                              safeAvoidSmallLoss = NA,
                                              safeAvoidSmallGain = NA,
                                              safeAvoidLargeGain = NA,
                                              riskyAvoidLargeLoss = NA,
                                              riskyAvoidSmallLoss = NA,
                                              riskyAvoidSmallGain = NA,
                                              riskyAvoidLargeGain = NA)

for (subjectId in subjectIdsUnique) {
  # Get the data for the user
  
  dataOfSubject = rawDataInquisit[rawDataInquisit$subject == subjectId,]
  
  safeAnswerRows  = dataOfSubject[dataOfSubject$trialcode == 'safe1question' | dataOfSubject$trialcode == 'safe2question',]
  riskyAnswerRows = dataOfSubject[dataOfSubject$trialcode == 'risky1question' | dataOfSubject$trialcode == 'risky2question',]
  
  # Compute the percentages
  
  totalAnswers = nrow(safeAnswerRows) + nrow(riskyAnswerRows)
  
  safeAvoidLargeLossPercentage = (nrow(safeAnswerRows[safeAnswerRows$response == 2,]) / totalAnswers) * 100
  safeAvoidSmallLossPercentage = (nrow(safeAnswerRows[safeAnswerRows$response == 3,]) / totalAnswers) * 100
  safeAvoidSmallGainPercentage = (nrow(safeAnswerRows[safeAnswerRows$response == 4,]) / totalAnswers) * 100
  safeAvoidLargeGainPercentage = (nrow(safeAnswerRows[safeAnswerRows$response == 5,]) / totalAnswers) * 100
  
  riskyAvoidLargeLossPercentage = (nrow(riskyAnswerRows[riskyAnswerRows$response == 2,]) / totalAnswers) * 100
  riskyAvoidSmallLossPercentage = (nrow(riskyAnswerRows[riskyAnswerRows$response == 3,]) / totalAnswers) * 100
  riskyAvoidSmallGainPercentage = (nrow(riskyAnswerRows[riskyAnswerRows$response == 4,]) / totalAnswers) * 100
  riskyAvoidLargeGainPercentage = (nrow(riskyAnswerRows[riskyAnswerRows$response == 5,]) / totalAnswers) * 100
  
  # Update the data frame
  
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'safeAvoidLargeLoss'] = safeAvoidLargeLossPercentage
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'safeAvoidSmallLoss'] = safeAvoidSmallLossPercentage
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'safeAvoidSmallGain'] = safeAvoidSmallGainPercentage
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'safeAvoidLargeGain'] = safeAvoidLargeGainPercentage
  
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'riskyAvoidLargeLoss'] = riskyAvoidLargeLossPercentage
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'riskyAvoidSmallLoss'] = riskyAvoidSmallLossPercentage
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'riskyAvoidSmallGain'] = riskyAvoidSmallGainPercentage
  riskySafeChoiceReasonPercentages[riskySafeChoiceReasonPercentages$subjectIds == subjectId, 'riskyAvoidLargeGain'] = riskyAvoidLargeGainPercentage
}

str(riskySafeChoiceReasonPercentages)

