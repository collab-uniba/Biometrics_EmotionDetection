ifelse(!dir.exists("./lib"), dir.create("./lib"), FALSE)

install.packages("caret", repos="http://cran.rstudio.com/", lib="./lib")

localLib <- "C:/emotion_detection/mc_nemar/lib/"
inputFoder <- "C:/emotion_detection/mc_nemar/input/"
outputFolder <- "C:emotion_detection/mc_nemar/output/"

dir.create(localLib, showWarnings = FALSE, recursive = TRUE)
dir.create(inputFoder, showWarnings = FALSE, recursive = TRUE)
dir.create(outputFolder, showWarnings = FALSE, recursive = TRUE)