###############################################################################
# Risky vs Safe Decision Making Game Data Analysis
#
# AUTHORS: Computational Affective Neuroscience (CAN) Lab, U of T
#
###############################################################################

workingDirectoryPath = getwd()
csvDirectoryPath = paste(workingDirectoryPath, "data", sep = "/")
rawDataInquisitPath = paste(csvDirectoryPath, "rvs_v4_rawdata_inquisit.csv",
                            sep = "/")

rawDataInquisit=read.csv(rawDataInquisitPath)
print(rawDataInquisit)