###############################################################################
# MES (Moral Evaluation Study) Analysis
#
# AUTHORS: Neda Safaee-Rad, Computational Affective Neuroscience (CAN) Lab,
#          U of T
#
###############################################################################


###############################################################################
# FUNCTIONS
###############################################################################


# Placeholder


###############################################################################
# PROGRAM
###############################################################################


# Load and read the CSV

workingDirectoryPath = getwd()

csvDirectoryPath = paste(workingDirectoryPath, "data", sep = "/")

qualtricsDataPath = paste(
  csvDirectoryPath,
  "qualtrics_IN_LAB_Moral_Evaluation_January_8_2018_16_01_numeric_cleanedcolnames.csv",
  sep = "/"
)

rawDataPath = paste(
  csvDirectoryPath,
  "raw_evaluative_categorization_mar13_2017_p_m_18_01_09.csv",
  sep = "/"
)

summaryDataPath = paste(
  csvDirectoryPath,
  "summary_evaluative_categorization_mar13_2017_p_m_18_01_08-2.csv",
  sep = "/"
)

# Data

qualtricsData = read.csv(qualtricsDataPath, stringsAsFactors = FALSE)
rawData       = read.csv(rawDataPath, stringsAsFactors = FALSE)
summaryData   = read.csv(summaryDataPath, stringsAsFactors = FALSE)

# Task 1
# Filter out incomplete data.

summaryData = summaryData[summaryData$values.completed == '1',]

# Task 2
# Distinguish duplicate IDs.

duplicatedSubjectIds = duplicated(summaryData$script.subjectid)

distinguishedSummarySubjectIds = paste0(
  summaryData$script.subjectid[duplicatedSubjectIds],
  letters
)

summaryData$script.subjectid[duplicatedSubjectIds] =
  distinguishedSummarySubjectIds

# Task 3
# Merge raw and summary data.

# mergedSummaryAndRawData = merge(
#   summaryData,
#   rawData,
#   by=intersect("script.subjectid", "subject")
# )
