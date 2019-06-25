# Author: Garofalo Antonio
# This script is the start point to execute the tuning of dataset
# NB the input series (so, their output) are already declared in this script
# NB in order to chose what input/output have to be classified, check the if(TRUE/FALSE) blocks to enable/disable each input/output

# In the code below are declared the input/output for classification
input_old <- c("dataset_old/EEG.csv", "dataset_old/EMG.csv", "dataset_old/GSR.csv",
		"dataset_old/EEG_EMG.csv", "dataset_old/EEG_GSR.csv", "dataset_old/EMG_GSR.csv",
		"dataset_old/all_rmHR.csv" )

output_old_arousal <- c("old/EEG_arousal", "old/EMG_arousal", "old/GSR_arousal",
		"old/EEG_EMG_arousal", "old/EEG_GSR_arousal", "old/EMG_GSR_arousal", 
		"old/ALL_arousal")

output_old_valence <- c("old/EEG_valence", "old/EMG_valence", "old/GSR_valence", 
		"old/EEG_EMG_valence", "old/EEG_GSR_valence", "old/EMG_GSR_valence", 
		"old/ALL_valence" )


input_new <- c("IEEG_dataset.csv", "EMG_dataset.csv", "GSR_dataset.csv",
		"IEEG_EMG_dataset.csv", "IEEG_GSR_dataset.csv", "EMG_GSR_dataset.csv",
		"IEEG_EMG_GSR_dataset.csv" )

output_arousal <- c("EEG_arousal", "EMG_arousal", "GSR_arousal",
		"EEG_EMG_arousal", "EEG_GSR_arousal", "EMG_GSR_arousal", 
		"ALL_arousal")

output_valence <- c("EEG_valence", "EMG_valence", "GSR_valence", 
		"EEG_EMG_valence", "EEG_GSR_valence", "EMG_GSR_valence", 
		"ALL_valence" )

time.start <- Sys.time()
cat("Ora di inizio: \n")
print(time.start)

# Set the condition TRUE if you need of input/output of older version of dataset, else set FALSE
if (FALSE){
	# The code below classify the dataset features in order to predict AROUSAL
	for(i in 1:length(input_old)){
		started <- Sys.time()
		in_name <- input_old[i]
		cat(i, " CICLO:", in_name, "\n")

		out_name <- output_old_arousal[i]
		outcomeName <- c("arousal")
		excluded_predictors <- c("id", "valence")
		source('C:/emotion_detection/tuning/tuning.R', encoding = 'UTF-8')
		out_name <- NA
		outcomeName <- NA
		excluded_predictors <- NA

		ended <- Sys.time()
		elapsed_time <- ended - started
		cat("Tempo impiegato per il tuning: \n")
		print(elapsed_time)
		cat("\n")
	}

	# The code below classify the dataset features in order to predict VALENCE
	for(i in 1:length(input_old)){
		started <- Sys.time()
		in_name <- input_old[i]
		cat(i, " CICLO:", in_name, "\n")

		out_name <- output_old_valence[i]
		outcomeName <- c("valence")
		excluded_predictors <- c("id", "arousal")
		source('C:/emotion_detection/tuning/tuning.R', encoding = 'UTF-8')
		out_name <- NA
		outcomeName <- NA
		excluded_predictors <- NA

		ended <- Sys.time()
		elapsed_time <- ended - started
		cat("Tempo impiegato per il tuning: \n")
		print(elapsed_time)
		cat("\n")
		
	}
}

# Set the condition TRUE if you need of input/output of most recent version of dataset, else set FALSE
if(TRUE){
	# The code below classify the dataset features in order to predict AROUSAL
	for(i in 1:length(input_new)){
		started <- Sys.time()
		in_name <- input_new[i]
		cat(i, " CICLO:", in_name, "\n")

		out_name <- output_arousal[i]
		outcomeName <- c("arousal")
		excluded_predictors <- c("id", "valence")
		source('C:/emotion_detection/tuning/tuning.R', encoding = 'UTF-8')
		out_name <- NA
		outcomeName <- NA
		excluded_predictors <- NA
		
		ended <- Sys.time()
		elapsed_time <- ended - started
		cat("Tempo impiegato per il tuning: \n")
		print(elapsed_time)
		cat("\n")
	}

	# The code below classify the dataset features in order to predict VALENCE
	for(i in 1:length(input_new)){
		started <- Sys.time()
		in_name <- input_new[i]
		cat(i, " CICLO:", in_name, "\n")

		out_name <- output_valence[i]
		outcomeName <- c("valence")
		excluded_predictors <- c("id", "arousal")
		source('C:/emotion_detection/tuning/tuning.R', encoding = 'UTF-8')
		out_name <- NA
		outcomeName <- NA
		excluded_predictors <- NA

		ended <- Sys.time()
		elapsed_time <- ended - started
		cat("Tempo impiegato per il tuning: \n")
		print(elapsed_time)
		cat("\n")
	}
}

time.end <- Sys.time()
cat("Terminato alle: ", "\n")
print(time.end)
time.elapsed <- time.end - time.start
cat("Tempo totale impiegato per il tuning: \n")
print(time.elapsed)

#NB predictions: arousal <- LLLL HHHH
#NB predictions: valence <- HHLL HHLL