# Author: Girardi Daniela, Garofalo Antonio
# This script provides to perform a Mc Nemar test between the classification results obtained with same algotithm and different features number 

localLib <- "C:/emotion_detection/mc_nemar/lib/"
inputFolder <- "C:/emotion_detection/mc_nemar/input/"
outputFolder <- "C:/emotion_detection/mc_nemar/output/"
classifier_file <- "C:/emotion_detection/mc_nemar/classifiers.txt"

source(paste(localLib, "mc_nemar.R", sep=""))

input_length <- 7
input_identifier_name <- c("output_predictions_")
input_format <- c(".csv")
output_format <- c(".txt")

# In the code below are declared the input/output directory for Mc Nemar test
input_old_arousal <- c("old/EEG_arousal/", "old/EMG_arousal/", "old/GSR_arousal/",
		"old/EEG_EMG_arousal/", "old/EEG_GSR_arousal/", "old/EMG_GSR_arousal/",
		"old/ALL_arousal/" )

input_old_valence <- c("old/EEG_valence/", "old/EMG_valence/", "old/GSR_valence/",
		"old/EEG_EMG_valence/", "old/EEG_GSR_valence/", "old/EMG_GSR_valence/",
		"old/ALL_valence/" )

input_new_arousal <- c("EEG_arousal/", "EMG_arousal/", "GSR_arousal/",
		"EEG_EMG_arousal/", "EEG_GSR_arousal/", "EMG_GSR_arousal/",
		"ALL_arousal/" )

input_new_valence <- c("EEG_valence/", "EMG_valence/", "GSR_valence/",
		"EEG_EMG_valence/", "EEG_GSR_valence/", "EMG_GSR_valence/",
		"ALL_valence/" )


output_arousal <- c("EEG_arousal/", "EMG_arousal/", "GSR_arousal/",
		"EEG_EMG_arousal/", "EEG_GSR_arousal/", "EMG_GSR_arousal/", 
		"ALL_arousal/")

output_valence <- c("EEG_valence/", "EMG_valence/", "GSR_valence/", 
		"EEG_EMG_valence/", "EEG_GSR_valence/", "EMG_GSR_valence/", 
		"ALL_valence/" )

# Enables multicore parallel processing 
if(!exists("enable_parallel", mode="function")) 
  source(paste(localLib, "enable_parallel.R", sep=""))

classifiers <- readLines(classifier_file)

# The code below perform a McNemar test in order to compare AROUSAL prediction
for(i in 1:input_length){
	cat("AROUSAL CYCLE ", i, "\n")
	for(j in 1:length(classifiers)){
		cat("Analisi in corso per: ", classifiers[j], "\n")
		
		in_file_old <- paste(inputFolder, input_old_arousal[i], input_identifier_name, classifiers[j], input_format , sep="")
		cat("Old input file: ", in_file_old, "\n")
		df_old <- read.csv(in_file_old, header = TRUE, sep = ",")
		predictions_old <- df_old$prediction

		in_file_new <- paste(inputFolder, input_new_arousal[i], input_identifier_name, classifiers[j], input_format, sep="")
		cat("New input file: ", in_file_new, "\n\n")
		df_new <- read.csv(in_file_new, header = TRUE, sep = ",")
		predictions_new <- df_new$prediction

		outdir <- paste(outputFolder, output_arousal[i])
		out_file <- paste(classifiers[j], output_format, sep="")
  		mc_nemar(predictions_old, predictions_new, outdir, out_file)
	}
}

# The code below perform a McNemar test in order to compare VALENCE prediction
for(i in 1:input_length){
	cat("VALENCE CYCLE ", i, "\n")
	for(j in 1:length(classifiers)){
		cat("Analisi in corso per: ", classifiers[j], "\n")
		
		in_file_old <- paste(inputFolder, input_old_valence[i], input_identifier_name, classifiers[j], input_format , sep="")
		cat("Old input file: ", in_file_old, "\n")
		df_old <- read.csv(in_file_old, header = TRUE, sep = ",")
		predictions_old <- df_old$prediction

		in_file_new <- paste(inputFolder, input_new_valence[i], input_identifier_name, classifiers[j], input_format, sep="")
		cat("New input file: ", in_file_new, "\n\n")
		df_new <- read.csv(in_file_new, header = TRUE, sep = ",")
		predictions_new <- df_new$prediction

		outdir <- paste(outputFolder, output_valence[i])
		out_file <- paste(classifiers[j], output_format, sep="")
  		mc_nemar(predictions_old, predictions_new, outdir, out_file)
	}
}


