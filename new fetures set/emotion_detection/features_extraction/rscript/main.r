# Author: Garofalo Antonio
# This script represents the start point to calculate the physiological signal features (EEG, EMG, GSR) and build the related dataset
# NB In order to execute this script, it's required the choice of signals to analyze. They have to be manually insert in code(find: signal_to_analyze)
# On launch, for each chosen signal, is executed the related main
# Then, for each signal, the functions to build the dataset are called
# End, if more signals are chosen, their features are merged and used to build combined dataset

scriptPath <- "C:/emotion_detection/features_extraction/rscript/"
dataSubjectsPath <- "C:/emotion_detection/data_subjects/"
datasetPath <- "C:/emotion_detection/features_extraction/dataset/"

dir.create(scriptPath, showWarnings = FALSE, recursive = TRUE)
dir.create(dataSubjectsPath, showWarnings = FALSE, recursive = TRUE)
dir.create(datasetPath, showWarnings = FALSE, recursive = TRUE)

source(paste(scriptPath, "utility.R", sep=""))
source(paste(scriptPath, "signal_preprocessing.R", sep=""))
source(paste(scriptPath, "features_extraction.R", sep=""))

# 
# 
# 
# ------------------------------------- SIGNAL SELECTION -------------------------------------
# 
# 
# 

# Select signals to analyze entering in signal_to_analyze declaration one of the constants below. (Example: signal_to_analyze <- c(EEG, GSR))
EEG <- "EEG"
EMG <- "EMG"
GSR <- "GSR"

signal_to_analyze <- c(EEG, EMG, GSR)
print("Segnali selezionati: ")
cat("Segnali selezionati: ", signal_to_analyze, "\n")

all_EEG_video_features <- NA
all_EMG_video_features <- NA
all_GSR_video_features <- NA

# The code below represents the data of subjects analyzed (id, surname, session, timestamp by EEG sensor, timestamp by EMG/GSR sensors)
num_subject <- c(1, 2, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
surname_subject <- c("Iaffaldano", "Donadio", "Giannico", "Laghezza", "Dibari", "Deserio", "Maiorano", "Ladisa", "Siciliani", "Decclesia", "Campione",
	"Gambacorta", "Grimaldi", "Nocera", "Novielli", "Basile", "Diomede", "Pugliese") 
session_subject <- c(2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
start_time_EEG <- c("15:55:43", "17:22:22", "09:44:42", "18:04:24", "15:15:58", "09:54:43", "15:41:01", "15:29:06", "11:58:53", "14:36:38", "12:52:38",
	"09:40:45", "14:36:13", "09:39:53", "13:40:40", "15:19:01", "15:27:27", "09:54:49")
start_time_EMG_GSR <- c("2017-03-06 15:55:43", "2017-03-06 17:22:22", "2017-03-10 09:44:42", "2017-03-09 18:04:24", "2017-03-14 15:15:58", "2017-03-13 09:54:43", 
	"2017-03-10 15:41:01", "2017-03-15 15:29:06", "2017-03-23 11:58:53", "2017-03-22 14:36:38", "2017-03-22 12:52:38", "2017-03-22 09:40:45",
	"2017-03-20 14:36:13", "2017-03-17 09:39:53", "2017-03-17 13:40:40", "2017-03-24 15:19:01", "2017-03-23 15:27:27", "2017-03-24 09:54:49")

time.start <- Sys.time()
cat("Ora di inizio: \n")
print(time.start)

# The code below launches the appropriate file main for each chosen signal
for (i in 1:length(num_subject)){
	num_partecipante <- num_subject[i]
	cat("ID Partecipante: ", num_partecipante, "\n")
	surname <- surname_subject[i]
	cat("Cognome Partecipante: ", surname, "\n")
	session <- session_subject[i]
	
	if(EEG %in% signal_to_analyze){
		cat("Analisi in corso: ", EEG, "\n")
		
		start_time <- start_time_EEG[i]
	  	source(paste(scriptPath, "main_EEG.R", sep=""))
	  	all_EEG_video_features <- return_all_video_features()

		cat("Analisi completata: ", EEG, "\n\n")
	} 

	if (EMG %in% signal_to_analyze){
	 	cat("Analisi in corso: ", EMG, "\n")
	 	
	 	start_time <- start_time_EMG_GSR[i]
	 	source(paste(scriptPath, "main_EMG.R", sep=""))
	 	all_EMG_video_features <- return_all_video_features()

	 	cat("Analisi completata: ", EMG, "\n\n")
	}

	if (GSR %in% signal_to_analyze){
	  	cat("Analisi in corso: ", GSR, "\n")
	  	
	  	start_time <- start_time_EMG_GSR[i]
	  	source(paste(scriptPath, "main_GSR.R", sep=""))
	  	all_GSR_video_features <- return_all_video_features()

		cat("Analisi completata: ", GSR, "\n\n")
	}

	# 
	# 
	# 
	# ------------------------------------- DATASET BUILDING -------------------------------------
	# 
	# 
	# 

	# The code below builds a single dataset for each chosen signal
	if (!is.na(all_EEG_video_features)){
		print("IEEG_dataset building...")
		add_to_EEG_dataset(all_EEG_video_features)
		cat("Completed!", "\n\n")
	}

	if (!is.na(all_EMG_video_features)){
		print("EMG_dataset building...")
		add_to_EMG_dataset(all_EMG_video_features)
		cat("Completed!", "\n\n")
	}

	if (!is.na(all_GSR_video_features)){
		print("GSR_dataset building...")
		add_to_GSR_dataset(all_GSR_video_features)
		cat("Completed!", "\n\n")
	}

	# The code below builds combined dataset 
	# NB In order to avoid duplicate columns, before merge, for each method, arousal and valence columns are excluded from each params except from the last one
	ncol_EEG <- ncol(all_EEG_video_features) - 2
	ncol_EMG <- ncol(all_EMG_video_features) - 2
	ncol_GSR <- ncol(all_GSR_video_features) - 2

	if (!is.na(all_EEG_video_features) && !is.na(all_EMG_video_features)){
		print("IEEG_EMG_dataset building...")
		add_to_EEG_EMG_dataset(all_EEG_video_features[, 1:ncol_EEG], all_EMG_video_features)
		cat("Completed!", "\n\n")
	}

	if (!is.na(all_EEG_video_features) && !is.na(all_GSR_video_features)){
		print("IEEG_GSR_dataset building...")
		add_to_EEG_GSR_dataset(all_EEG_video_features[, 1:ncol_EEG], all_GSR_video_features)
		cat("Completed!", "\n\n")
	}

	if (!is.na(all_EMG_video_features) && !is.na(all_GSR_video_features)){
		print("EMG_GSR_dataset building...")
		add_to_EMG_GSR_dataset(all_EMG_video_features[, 1:ncol_EMG], all_GSR_video_features)
		cat("Completed!", "\n\n")
	}

	if ((!is.na(all_EEG_video_features)) && (!is.na(all_EMG_video_features)) && (!is.na(all_GSR_video_features))){
		print("IEEG_EMG_GSR_dataset building...")
		add_to_EEG_EMG_GSR_dataset(all_EEG_video_features[, 1:ncol_EEG], all_EMG_video_features[, 1:ncol_EMG], all_GSR_video_features)
		cat("Completed!", "\n\n")
	}

}

time.end <- Sys.time()
cat("Terminato alle: ", "\n")
print(time.end)
time.elapsed <- time.end - time.start
cat("Tempo totale impiegato per il calcolo delle features: \n")
print(time.elapsed)