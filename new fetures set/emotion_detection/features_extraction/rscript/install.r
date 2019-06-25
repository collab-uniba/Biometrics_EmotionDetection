# Author: Daniela Girardi, Garofalo Antonio
# This script provides to install several library and to create correctly the directory needed
# NB in order to correctly install everything, run this code with administrator permissions

ifelse(!dir.exists("./lib"), dir.create("./lib"), FALSE)

install.packages("signal", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("pracma", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("wmtsa", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("ggplot2", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("oce", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("lubridate", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("Ryacas", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("pspline", repos="http://cran.rstudio.com/", lib="./lib")
install.packages("biosignalEMG", repos="http://cran.rstudio.com/", lib="./lib")

scriptPath <- "C:/emotion_detection/features_extraction/rscript/"
dataSubjectsPath <- "C:/emotion_detection/data_subjects/"
datasetPath <- "C:/emotion_detection/features_extraction/dataset/"

dir.create(scriptPath, showWarnings = FALSE, recursive = TRUE)
dir.create(dataSubjectsPath, showWarnings = FALSE, recursive = TRUE)
dir.create(datasetPath, showWarnings = FALSE, recursive = TRUE)
