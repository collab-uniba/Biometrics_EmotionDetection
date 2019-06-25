# Author: Daniela Girardi, Garofalo Antonio
# This script provides to calculate the GSR features following this steps:
# 	- acquisition of information about the subject and related data of EEG sensor
# 	- useful raw data and baseline selection 
# 	- baseline filtering and mean baseline calculation
# 	- video filtering and baseline subtraction
# 	- features extraction
# NB The features extracted are calculated considering the last 30 seconds of each video
# Also, the script itself provides to acquire the useful data to work
# NB The result dataset has only features corrected with baseline

.libPaths( c( .libPaths(), "./lib") )
library(signal)
library(pracma)
library(wmtsa)
library(Ryacas)
library(lubridate)
library(pspline)

fileNameTemp <- paste(dataSubjectsPath, "Shimmer_GSR/", num_partecipante, sep= "")
fileNameTemp <- paste(fileNameTemp, "_", sep= "")
fileNameTemp <- paste(fileNameTemp, surname, sep= "")
fileNameTemp <- paste(fileNameTemp, "_Session",sep= "")
fileNameTemp <- paste(fileNameTemp, session, sep= "")
dataFilePath <- paste(fileNameTemp, "_Shimmer_GSK_Calibrated_PC.csv", sep = "")

# 
# 
# 
# ------------------------------------- RAW DATA AND BASELINE LINES SELECTION -------------------------------------
# 
# 
# 

# The code below provides to get the file with data sent by sensor and to search for the index of the useful lines for each video and for baseline
dataFile = read.csv(dataFilePath, header= TRUE);
useful_seconds <- find_useful_lines(start_time, dataFile, "GSR")

# Column with frequency data
col = 2;
data_baseline1 = dataFile[c(useful_seconds$start_baseline1:useful_seconds$end_baseline1) ,c(col)];
data_video1 = dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) ,c(col)];
data_video2 = dataFile[c(useful_seconds$start_video2:useful_seconds$end_video2) ,c(col)];
data_baseline2 = dataFile[c(useful_seconds$start_baseline2:useful_seconds$end_baseline2) ,c(col)];
data_video3 = dataFile[c(useful_seconds$start_video3:useful_seconds$end_video3) ,c(col)];
data_video4 = dataFile[c(useful_seconds$start_video4:useful_seconds$end_video4) ,c(col)];
data_baseline3 = dataFile[c(useful_seconds$start_baseline3:useful_seconds$end_baseline3) ,c(col)];
data_video5 = dataFile[c(useful_seconds$start_video5:useful_seconds$end_video5) ,c(col)];
data_video6 = dataFile[c(useful_seconds$start_video6:useful_seconds$end_video6) ,c(col)];
data_baseline4 = dataFile[c(useful_seconds$start_baseline4:useful_seconds$end_baseline4) ,c(col)];
data_video7 = dataFile[c(useful_seconds$start_video7:useful_seconds$end_video7) ,c(col)];
data_video8 = dataFile[c(useful_seconds$start_video8:useful_seconds$end_video8) ,c(col)];

# 
# 
# 
# ------------------------------------- TIMESTAMP -------------------------------------
# 
# 
# 

# Timestamp building for each video. The timestamp is useful for calculate some features
col = 1
time_video1 = dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) ,c(col)];
time_video2 = dataFile[c(useful_seconds$start_video2:useful_seconds$end_video2) ,c(col)];
time_video3 = dataFile[c(useful_seconds$start_video3:useful_seconds$end_video3) ,c(col)];
time_video4 = dataFile[c(useful_seconds$start_video4:useful_seconds$end_video4) ,c(col)];
time_video5 = dataFile[c(useful_seconds$start_video5:useful_seconds$end_video5) ,c(col)];
time_video6 = dataFile[c(useful_seconds$start_video6:useful_seconds$end_video6) ,c(col)];
time_video7 = dataFile[c(useful_seconds$start_video7:useful_seconds$end_video7) ,c(col)];
time_video8 = dataFile[c(useful_seconds$start_video8:useful_seconds$end_video8) ,c(col)];

edaSignal_baseline1 = ts(data=data_baseline1, frequency=1/5)
edaSignal_baseline2 = ts(data=data_baseline2, frequency=1/5)
edaSignal_baseline3 = ts(data=data_baseline3, frequency=1/5)
edaSignal_baseline4 = ts(data=data_baseline4, frequency=1/5)

edaSignal_video1 = ts(data=data_video1, frequency=1/5)
edaSignal_video2 = ts(data=data_video2, frequency=1/5)
edaSignal_video3 = ts(data=data_video3, frequency=1/5)
edaSignal_video4 = ts(data=data_video4, frequency=1/5)
edaSignal_video5 = ts(data=data_video5, frequency=1/5)
edaSignal_video6 = ts(data=data_video6, frequency=1/5)
edaSignal_video7 = ts(data=data_video7, frequency=1/5)
edaSignal_video8 = ts(data=data_video8, frequency=1/5)

# 
# 
# 
# ------------------------------------- BASELINE FILTERING AND MEAN BASELINE CALCULATION -------------------------------------
# 
# 
# 

# Construct GSR filter
filter_GSR<- construct_filter_GSR()

# Apply filter to baseline data
baseline1_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline1)
baseline2_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline2)
baseline3_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline3)
baseline4_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline4)

# Calculate mean GSR of each baseline
mean_baseline1 <-mean(baseline1_filtered$phasic, trim = 0)
mean_baseline2 <-mean(baseline2_filtered$phasic, trim = 0)
mean_baseline3 <-mean(baseline3_filtered$phasic, trim = 0)
mean_baseline4 <-mean(baseline4_filtered$phasic, trim = 0)

# 
# 
# 
# ------------------------------------- VIDEO FILTERING AND BASELINE SUBTRACTION -------------------------------------
# 
# 
# 

# Apply filter for last 30 seconds of each video
video1_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video1)
video2_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video2)
video3_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video3)
video4_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video4)
video5_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video5)
video6_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video6)
video7_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video7)
video8_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video8)

# Subract the mean GSR of baseline to each video
video1_corrected <- subtract_baseline(video1_filtered$phasic, mean_baseline1)
video2_corrected <- subtract_baseline(video2_filtered$phasic, mean_baseline1)
video3_corrected <- subtract_baseline(video3_filtered$phasic, mean_baseline2)
video4_corrected <- subtract_baseline(video4_filtered$phasic, mean_baseline2)
video5_corrected <- subtract_baseline(video5_filtered$phasic, mean_baseline3)
video6_corrected <- subtract_baseline(video6_filtered$phasic, mean_baseline3)
video7_corrected <- subtract_baseline(video7_filtered$phasic, mean_baseline4)
video8_corrected <- subtract_baseline(video8_filtered$phasic, mean_baseline4)

# 
# 
# 
# ------------------------------------- WAVELET TRANSFORM -------------------------------------
# 
# 
# 

# Get Continuous Wavelet Transform of eda signal
edaSignal_video1.cwt <- wavCWT(video1_filtered$phasic)
edaSignal_video2.cwt <- wavCWT(video2_filtered$phasic)
edaSignal_video3.cwt <- wavCWT(video3_filtered$phasic)
edaSignal_video4.cwt <- wavCWT(video4_filtered$phasic)
edaSignal_video5.cwt <- wavCWT(video5_filtered$phasic)
edaSignal_video6.cwt <- wavCWT(video6_filtered$phasic)
edaSignal_video7.cwt <- wavCWT(video7_filtered$phasic)
edaSignal_video8.cwt <- wavCWT(video8_filtered$phasic)

edaSignal_video1_corrected.cwt <- wavCWT(video1_corrected)
edaSignal_video2_corrected.cwt <- wavCWT(video2_corrected)
edaSignal_video3_corrected.cwt <- wavCWT(video3_corrected)
edaSignal_video4_corrected.cwt <- wavCWT(video4_corrected)
edaSignal_video5_corrected.cwt <- wavCWT(video5_corrected)
edaSignal_video6_corrected.cwt <- wavCWT(video6_corrected)
edaSignal_video7_corrected.cwt <- wavCWT(video7_corrected)
edaSignal_video8_corrected.cwt <- wavCWT(video8_corrected)

# 
# 
# 
# ------------------------------------- FEATURES EXTRACTION -------------------------------------
# 
# 
# 

features_number <- 25
temp_all_video_features <- matrix(, 1, features_number)

# 
# -----------------VIDEO 1 -------------------
# 

# Extract frequency features of video 1
# 5phasic_features <- calculate_features(video1_filtered$phasic)
phasic_features_corrected <- calculate_features(video1_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video1, video1_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video1, video1_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video1.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video1_corrected.cwt)

# Extract additional features of video 1 (baseline subtracted)
new_phasic_features_corrected <- calculate_new_GSR_features(video1_corrected, time_video1)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 1, sep = '.')
video1_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"Low" , "High"  )

# Append video 1 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video1_features)

# 
# -----------------VIDEO 2 -------------------
# 

# Extract frequency features of video 2
# phasic_features <- calculate_features(video2_filtered$phasic)
phasic_features_corrected <- calculate_features(video2_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video2, video2_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video2, video2_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video2.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video2_corrected.cwt)

# Extract additional features of video 2
new_phasic_features_corrected <- calculate_new_GSR_features(video2_corrected, time_video2)


# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 2, sep = '.')
video2_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"Low" , "High"  )

# Append video 2 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video2_features)

# 
# -----------------VIDEO 3 -------------------
# 

# Extract frequency features of video 3
# phasic_features <- calculate_features(video3_filtered$phasic)
phasic_features_corrected <- calculate_features(video3_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video3, video3_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video3, video3_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video3.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video3_corrected.cwt)

# Extract additional features of video 3
new_phasic_features_corrected <- calculate_new_GSR_features(video3_corrected, time_video3)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 3, sep = '.')
video3_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"Low" , "Low"  )

# Append video 3 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video3_features)

# 
# -----------------VIDEO 4 -------------------
# 

# Extract frequency features of video 4
# phasic_features <- calculate_features(video4_filtered$phasic)
phasic_features_corrected <- calculate_features(video4_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video4, video4_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video4, video4_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video4.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video4_corrected.cwt)

# Extract additional features of video 4
new_phasic_features_corrected <- calculate_new_GSR_features(video4_corrected, time_video4)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 4, sep = '.')
video4_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"Low" , "Low"  )

# Append video 4 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video4_features)

# 
# -----------------VIDEO 5 -------------------
# 

# Extract frequency features of video 5
# phasic_features <- calculate_features(video5_filtered$phasic)
phasic_features_corrected <- calculate_features(video5_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video5, video5_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video5, video5_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video5.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video5_corrected.cwt)

# Extract additional features of video 5
new_phasic_features_corrected <- calculate_new_GSR_features(video5_corrected, time_video5)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 5, sep = '.')
video5_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"High" , "High"  )

# Append video 5 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video5_features)

# 
# -----------------VIDEO 6 -------------------
# 

# Extract frequency features of video 6
# phasic_features <- calculate_features(video6_filtered$phasic)
phasic_features_corrected <- calculate_features(video6_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video6, video6_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video6, video6_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video6.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video6_corrected.cwt)

# Extract additional features of video 6 (baseline subtracted)
new_phasic_features_corrected <- calculate_new_GSR_features(video6_corrected, time_video6)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 6, sep = '.')
video6_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"High" , "High"  )

# Append video 6 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video6_features)

# 
# -----------------VIDEO 7 -------------------
# 

# Extract frequency features of video 7
# phasic_features <- calculate_features(video7_filtered$phasic)
phasic_features_corrected <- calculate_features(video7_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video7, video7_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video7, video7_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video7.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video7_corrected.cwt)

# Extract additional features of video 7 (baseline subtracted)
new_phasic_features_corrected <- calculate_new_GSR_features(video7_corrected, time_video7)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 7, sep = '.')
video7_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"High" , "Low"  )

# Append video 7 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video7_features)

# 
# -----------------VIDEO 8 -------------------
# 

# Extract frequency features of video 8
# phasic_features <- calculate_features(video8_filtered$phasic)
phasic_features_corrected <- calculate_features(video8_corrected)

# phasic_derivative_features <- calculate_derivative_features(time_video8, video8_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video8, video8_corrected) 

# phasic_peaks_features <- calculate_peaks_features(edaSignal_video8.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video8_corrected.cwt)

# Extract additional features of video 8 (baseline subtracted)
new_phasic_features_corrected <- calculate_new_GSR_features(video8_corrected, time_video8)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 8, sep = '.')
video8_features <- build_row_for_GSR_dataset(id, phasic_features_corrected, 
	phasic_derivative_features_corrected, 
	phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	"High" , "Low"  )

# Append video 8 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video8_features)


return_all_video_features = function(){	
	all_video_features <- temp_all_video_features[2:nrow(temp_all_video_features),]
	return(all_video_features)
}