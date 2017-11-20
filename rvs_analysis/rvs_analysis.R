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

rawDataInquisit = read.csv(rawDataInquisitPath, stringsAsFactors = FALSE)

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


