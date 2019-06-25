# Author: Girardi Daniela, Garofalo Antonio
# This script provides to classify the features of dataset
# It uses several classification algorithms (nb, knn, ecc) 
# It built a list of files whith results (txt for each agorithm and a csv file with prediction value and outcome variable)

.libPaths( c( .libPaths(), "./lib") )
library(caret) # for param tuning
library(e1071) # for normality adjustment
library(RWeka)
library(klaR)

localLib <- "C:/emotion_detection/tuning/lib/"
modelsFile <- "C:/emotion_detection/tuning/models/models.txt"
seedsPath <- "C:/emotion_detection/tuning/" 
inputFolder <- "C:/emotion_detection/tuning/input/"
outputFolder <- "C:/emotion_detection/tuning/output/"

# Set the random seed, held constant for the current run
seeds <- readLines(paste(seedsPath, "seeds.txt", sep=""))
# seed <- ifelse(length(seeds[run]) == 0, sample(1:1000, 1), seeds[as.integer(run)])
seed <- set.seed(seeds)

# Acquires input
csv_file <- paste(inputFolder ,in_name, sep = "")
cat("File input: ", csv_file, "\n")

# Enables multicore parallel processing 
if(!exists("enable_parallel", mode="function")) 
  source(paste(localLib, "enable_parallel.R", sep=""))

data_ <- read.csv(csv_file, header = TRUE, sep=",")
dataframe <- read.csv(csv_file, header = TRUE, sep = ",")

# Name of outcome var to be predicted
cat("Prediction: ", outcomeName, "\n")

# List of predictor vars by name
data_ <- data_[ , !(names(data_) %in% excluded_predictors)]
predictorsNames <- names(data_[,!(names(data_)  %in% c(outcomeName))]) #  removes the var to be predicted from the test set

# Creation of current output directory for current execution
output_dir <- paste(outputFolder, out_name, sep="")
cat("Output directory: ", output_dir, "\n")
if(!dir.exists(output_dir))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

# Check NA value
cat("Checking NA value...", "\n")
for (i in 1:nrow(data_))
    for(j in 1:ncol(data_))
        if (is.na(data_[i, j])){
            cat("i: ", i, "\n", "j: ", j, "\n\n")
        }
 cat("Completed!", "\n\n")

# Creation of stratified training and test sets from  dataset: training and testing data are the same because we use LOOCV
training <- data_
testing <- data_

# Remove the large object
rm(data_)
gc()

#  10-fold CV repetitions
fitControl <- trainControl(
  method = "LOOCV",
  # number = 10,
  # repeated ten times, works only with method="repeatedcv", otherwise 1
  # repeats = 10,
  # verboseIter = TRUE,
  savePredictions = TRUE,
  # binary problem
  # summaryFunction=twoClassSummary,
  classProbs = TRUE,
  #  enable parallel computing if avail
  allowParallel = TRUE,
  returnData = FALSE
  # sampling = "smote"
)

# Load all the classifiers to tune
classifiers <- readLines(modelsFile)

cat("Classificatori: ", "\n")
print(classifiers)

for(i in 1:length(classifiers)){
	nline <- strsplit(classifiers[i], ":")[[1]]
	classifier <- nline[1]
	cpackage <- nline[2]
	#  RWeka packages do need parallel computing to be off
	fitControl$allowParallel <- ifelse(!is.na(cpackage) && cpackage == "RWeka", FALSE, TRUE)
	cat("Building model for classifier: ", classifier, "\n")
	if (outcomeName == "arousal"){
		model <- caret::train(arousal ~ ., 
	                          data = training,
	                          method = classifier,
	                          trControl = fitControl,
	                          # metric = "ROC",
							  metric = "Accuracy",
	                          preProcess = c("center", "scale"),
	                          tuneLength = 5 #  five values per param
		)	

	} else if (outcomeName == "valence"){
		model <- caret::train(valence ~ ., 
	                          data = training,
	                          method = classifier,
	                          trControl = fitControl,
	                          # metric = "ROC",
							  metric = "Accuracy",
	                          preProcess = c("center", "scale"),
	                          tuneLength = 5 #  five values per param
		)
	}
	  
	# Output file for the classifier at nad
	result <- paste(classifier, "txt", sep=".")
	output_file <- paste(output_dir, result, sep = "/")
	cat("===============================\n", file = output_file, sep="\n", append=TRUE)
	# cat("Seed:", seed, file=output_file, sep="\n", append=TRUE)
	out <- capture.output(model)
	cat("\nModel:\n", out, file=output_file, sep="\n", append=TRUE)

	# title = paste(classifier, run, sep = "_run#  ")
	cat(out, file=output_file, sep="\n", append=TRUE)

	# The highest roc val from train to save
	out <- capture.output(getTrainPerf(model))
	cat("\nHighest ROC value: ", out, file=output_file, sep="\n", append=TRUE)
	  
	  
	# Computes the scalar metrics
	predictions <- predict(object=model, testing[,predictorsNames], type='raw')
	
	# Output of predict value and reference var for each ID_subject
	output_predictions_path <- paste(output_dir, "output_predictions_", sep="/")
	output_predictions <- paste(output_predictions_path, classifier, ".csv", sep="")
	if(!file.exists(output_predictions)){
		output_predictions_header <- matrix(c("id", "prediction", "reference"), 1, 3)
		write.table(output_predictions_header, file = output_predictions, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	} 
	output_predictions_matrix <- matrix(c(dataframe[,"id"], as.character(predictions), as.character(dataframe[, outcomeName])),nrow(dataframe), 3)
	write.table(output_predictions_matrix, file = output_predictions, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	  
	# if(!exists("scalar_metrics", mode="function")) 
	source(paste(localLib, "scalar_metrics.R", sep=""))
	scalar_metrics(predictions=predictions, truth=testing[,outcomeName], outdir=".", outfile=output_file)

	# === cleanup ===
	
	# Deallocate large objects
	rm(model)
	rm(predictions)
	# Garbage collection
	gc()
	  
  
}
