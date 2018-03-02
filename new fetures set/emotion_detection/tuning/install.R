ifelse(!dir.exists("./lib"), dir.create("./lib"), FALSE)

install.packages("caret", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("e1071", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("RWeka", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("klaR", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("randomForest", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("caTools", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("RSNNS", repos="http://cran.rstudio.com/", lib="./lib")

localLib <- "C:/emotion_detection/tuning/lib/"
scriptPath <- "C:/emotion_detection/tuning/"
modelsFile <- "C:/emotion_detection/tuning/models/"
inputFoder <- "C:/emotion_detection/tuning/input/"
outputFolder <- "C:emotion_detection/tuning/output/"

dir.create(localLib, showWarnings = FALSE, recursive = TRUE)
dir.create(scriptPath, showWarnings = FALSE, recursive = TRUE)
dir.create(modelsFile, showWarnings = FALSE, recursive = TRUE)
dir.create(inputFoder, showWarnings = FALSE, recursive = TRUE)
dir.create(outputFolder, showWarnings = FALSE, recursive = TRUE)

