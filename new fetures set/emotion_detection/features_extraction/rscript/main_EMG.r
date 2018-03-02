# Author: Daniela Girardi, Garofalo Antonio
# This script provides to calculate the EMG features following this steps:
# 	- acquisition of information about the subject and related data of EMG sensor
# 	- useful raw data and baseline selection 
# 	- baseline filtering
# 	- video filtering
# 	- video additional preprocessing (rectification)
# 	- mean baseline calculation and baseline subtraction
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
library(biosignalEMG)

fileNameTemp <- paste(dataSubjectsPath, "Shimmer_EMG/", num_partecipante,  sep= "")
fileNameTemp <- paste(fileNameTemp, "_", sep= "")
fileNameTemp <- paste(fileNameTemp, surname, sep= "")
fileNameTemp <- paste(fileNameTemp, "_Session",sep= "")
fileNameTemp <- paste(fileNameTemp, session, sep= "")
dataFilePath <- paste(fileNameTemp, "_Shimmer_EMG_Calibrated_PC.csv", sep = "")

# 
# 
# 
# ------------------------------------- RAW DATA AND BASELINE LINES SELECTION -------------------------------------
# 
# 
# 

# The code below provides to get the file with data sent by sensor and to search for the index of the useful lines for each video and for baseline
dataFile = read.csv(dataFilePath, header = TRUE);
useful_seconds <- find_useful_lines(start_time, dataFile, "EMG")

# Column with frequency data for channel 1
col = 3;
data_baseline1_ch1 = dataFile[c(useful_seconds$start_baseline1:useful_seconds$end_baseline1) ,c(col)];
data_video1_ch1 = dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) ,c(col)];
data_video2_ch1 = dataFile[c(useful_seconds$start_video2:useful_seconds$end_video2) ,c(col)];
data_baseline2_ch1 = dataFile[c(useful_seconds$start_baseline2:useful_seconds$end_baseline2) ,c(col)];
data_video3_ch1 = dataFile[c(useful_seconds$start_video3:useful_seconds$end_video3) ,c(col)];
data_video4_ch1 = dataFile[c(useful_seconds$start_video4:useful_seconds$end_video4) ,c(col)];
data_baseline3_ch1 = dataFile[c(useful_seconds$start_baseline3:useful_seconds$end_baseline3) ,c(col)];
data_video5_ch1 = dataFile[c(useful_seconds$start_video5:useful_seconds$end_video5) ,c(col)];
data_video6_ch1 = dataFile[c(useful_seconds$start_video6:useful_seconds$end_video6) ,c(col)];
data_baseline4_ch1 = dataFile[c(useful_seconds$start_baseline4:useful_seconds$end_baseline4) ,c(col)];
data_video7_ch1 = dataFile[c(useful_seconds$start_video7:useful_seconds$end_video7) ,c(col)];
data_video8_ch1 = dataFile[c(useful_seconds$start_video8:useful_seconds$end_video8) ,c(col)];

# Column with frequency data for channel 2
col = 4;
data_baseline1_ch2 = dataFile[c(useful_seconds$start_baseline1:useful_seconds$end_baseline1) ,c(col)];
data_video1_ch2 = dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) ,c(col)];
data_video2_ch2 = dataFile[c(useful_seconds$start_video2:useful_seconds$end_video2) ,c(col)];
data_baseline2_ch2 = dataFile[c(useful_seconds$start_baseline2:useful_seconds$end_baseline2) ,c(col)];
data_video3_ch2 = dataFile[c(useful_seconds$start_video3:useful_seconds$end_video3) ,c(col)];
data_video4_ch2 = dataFile[c(useful_seconds$start_video4:useful_seconds$end_video4) ,c(col)];
data_baseline3_ch2 = dataFile[c(useful_seconds$start_baseline3:useful_seconds$end_baseline3) ,c(col)];
data_video5_ch2 = dataFile[c(useful_seconds$start_video5:useful_seconds$end_video5) ,c(col)];
data_video6_ch2 = dataFile[c(useful_seconds$start_video6:useful_seconds$end_video6) ,c(col)];
data_baseline4_ch2 = dataFile[c(useful_seconds$start_baseline4:useful_seconds$end_baseline4) ,c(col)];
data_video7_ch2 = dataFile[c(useful_seconds$start_video7:useful_seconds$end_video7) ,c(col)];
data_video8_ch2 = dataFile[c(useful_seconds$start_video8:useful_seconds$end_video8) ,c(col)];

# 
# 
# 
# ------------------------------------- BASELINE FILTERING -------------------------------------
# 
# 
# 

# Construct EMG filter
filter_EMG<- construct_filter_EMG()

# Apply the filter to baseline data for channel 1
baseline1_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_baseline1_ch1)
baseline2_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_baseline2_ch1)
baseline3_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_baseline3_ch1)
baseline4_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_baseline4_ch1)

# Apply the filter to baseline data for channel 2
baseline1_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_baseline1_ch2)
baseline2_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_baseline2_ch2)
baseline3_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_baseline3_ch2)
baseline4_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_baseline4_ch2)

# 
# 
# 
# ------------------------------------- VIDEO FILTERING -------------------------------------
# 
# 
# 

# Apply filter to video data for channel 1
video1_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video1_ch1)
video2_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video2_ch1)
video3_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video3_ch1)
video4_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video4_ch1)
video5_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video5_ch1)
video6_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video6_ch1)
video7_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video7_ch1)
video8_filtered_ch1 <- apply_filter_EMG(filter_EMG, data_video8_ch1)


# Apply filter to video data for channel 2
video1_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video1_ch2)
video2_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video2_ch2)
video3_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video3_ch2)
video4_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video4_ch2)
video5_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video5_ch2)
video6_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video6_ch2)
video7_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video7_ch2)
video8_filtered_ch2 <- apply_filter_EMG(filter_EMG, data_video8_ch2)


# Transform data into emg object (baseline, channel 1)
baseline1_filtered_ch1_emg <- as.emg(as.numeric(baseline1_filtered_ch1),samplingrate = 512)
baseline2_filtered_ch1_emg <- as.emg(as.numeric(baseline2_filtered_ch1), samplingrate = 512)
baseline3_filtered_ch1_emg <- as.emg(as.numeric(baseline3_filtered_ch1),samplingrate = 512)
baseline4_filtered_ch1_emg <- as.emg(as.numeric(baseline4_filtered_ch1),samplingrate = 512)


# Transform data into emg object (baseline, channel 2)
baseline1_filtered_ch2_emg <- as.emg(as.numeric(baseline1_filtered_ch2),samplingrate = 512)
baseline2_filtered_ch2_emg <- as.emg(as.numeric(baseline2_filtered_ch2),samplingrate = 512)
baseline3_filtered_ch2_emg <- as.emg(as.numeric(baseline3_filtered_ch2),samplingrate = 512)
baseline4_filtered_ch2_emg <- as.emg(as.numeric(baseline4_filtered_ch2),samplingrate = 512)


# Transform data into emg object (video, channel 1)
video1_filtered_ch1_emg <- as.emg(as.numeric(video1_filtered_ch1),samplingrate = 512)
video2_filtered_ch1_emg <- as.emg(as.numeric(video2_filtered_ch1),samplingrate = 512)
video3_filtered_ch1_emg <- as.emg(as.numeric(video3_filtered_ch1),samplingrate = 512)
video4_filtered_ch1_emg <- as.emg(as.numeric(video4_filtered_ch1),samplingrate = 512)
video5_filtered_ch1_emg <- as.emg(as.numeric(video5_filtered_ch1),samplingrate = 512)
video6_filtered_ch1_emg <- as.emg(as.numeric(video6_filtered_ch1),samplingrate = 512)
video7_filtered_ch1_emg <- as.emg(as.numeric(video7_filtered_ch1),samplingrate = 512)
video8_filtered_ch1_emg <- as.emg(as.numeric(video8_filtered_ch1),samplingrate = 512)


# Transform data into emg object (video, channel 2)
video1_filtered_ch2_emg <- as.emg(as.numeric(video1_filtered_ch2),samplingrate = 512)
video2_filtered_ch2_emg <- as.emg(as.numeric(video2_filtered_ch2),samplingrate = 512)
video3_filtered_ch2_emg <- as.emg(as.numeric(video3_filtered_ch2),samplingrate = 512)
video4_filtered_ch2_emg <- as.emg(as.numeric(video4_filtered_ch2),samplingrate = 512)
video5_filtered_ch2_emg <- as.emg(as.numeric(video5_filtered_ch2),samplingrate = 512)
video6_filtered_ch2_emg <- as.emg(as.numeric(video6_filtered_ch2),samplingrate = 512)
video7_filtered_ch2_emg <- as.emg(as.numeric(video7_filtered_ch2),samplingrate = 512)
video8_filtered_ch2_emg <- as.emg(as.numeric(video8_filtered_ch2),samplingrate = 512)

# 
# 
# 
# ------------------------------------- VIDEO ADDITIONAL PREPROCESSING (RECTIFICATION) -------------------------------------
# 
# 
# 

# Additional preprocessing whit rectification (baseline channel 1 )
baseline1_ch1_rect <- rectification(baseline1_filtered_ch1_emg,  rtype = "fullwave")
baseline2_ch1_rect <- rectification(baseline2_filtered_ch1_emg,  rtype = "fullwave")
baseline3_ch1_rect <- rectification(baseline3_filtered_ch1_emg,  rtype = "fullwave")
baseline4_ch1_rect <- rectification(baseline4_filtered_ch1_emg,  rtype = "fullwave")


# Additional preprocessing whit rectification ( baseline channel 2)
baseline1_ch2_rect <- rectification(baseline1_filtered_ch2_emg,  rtype = "fullwave")
baseline2_ch2_rect <- rectification(baseline2_filtered_ch2_emg,  rtype = "fullwave")
baseline3_ch2_rect <- rectification(baseline3_filtered_ch2_emg,  rtype = "fullwave")
baseline4_ch2_rect <- rectification(baseline4_filtered_ch2_emg,  rtype = "fullwave")


# Additional preprocessing whit rectification (video channel 1 )
video1_filtered_ch1_rect <- rectification(video1_filtered_ch1_emg,  rtype = "fullwave")
video2_filtered_ch1_rect <- rectification(video2_filtered_ch1_emg,  rtype = "fullwave")
video3_filtered_ch1_rect <- rectification(video3_filtered_ch1_emg,  rtype = "fullwave")
video4_filtered_ch1_rect <- rectification(video4_filtered_ch1_emg,  rtype = "fullwave")
video5_filtered_ch1_rect <- rectification(video5_filtered_ch1_emg,  rtype = "fullwave")
video6_filtered_ch1_rect <- rectification(video6_filtered_ch1_emg,  rtype = "fullwave")
video7_filtered_ch1_rect <- rectification(video7_filtered_ch1_emg,  rtype = "fullwave")
video8_filtered_ch1_rect <- rectification(video8_filtered_ch1_emg,  rtype = "fullwave")


# Additional preprocessing whit rectification (video channel 2 )
video1_filtered_ch2_rect <- rectification(video1_filtered_ch2_emg,  rtype = "fullwave")
video2_filtered_ch2_rect <- rectification(video2_filtered_ch2_emg,  rtype = "fullwave")
video3_filtered_ch2_rect <- rectification(video3_filtered_ch2_emg,  rtype = "fullwave")
video4_filtered_ch2_rect <- rectification(video4_filtered_ch2_emg,  rtype = "fullwave")
video5_filtered_ch2_rect <- rectification(video5_filtered_ch2_emg,  rtype = "fullwave")
video6_filtered_ch2_rect <- rectification(video6_filtered_ch2_emg,  rtype = "fullwave")
video7_filtered_ch2_rect <- rectification(video7_filtered_ch2_emg,  rtype = "fullwave")
video8_filtered_ch2_rect <- rectification(video8_filtered_ch2_emg,  rtype = "fullwave")

# 
# 
# 
# ------------------------------------- MEAN BASELINE CALCULATION AND BASELINE SUBTRACTION -------------------------------------
# 
# 
# 

# Calculate mean EMG of each baseline (channel 1)
mean_baseline1_ch1 <- mean(baseline1_ch1_rect$val)
mean_baseline2_ch1 <- mean(baseline2_ch1_rect$val)
mean_baseline3_ch1 <- mean(baseline3_ch1_rect$val)
mean_baseline4_ch1 <- mean(baseline4_ch1_rect$val)


# Calculate mean EMG of each baseline (channel 2)
mean_baseline1_ch2 <- mean(baseline1_ch2_rect$val)
mean_baseline2_ch2 <- mean(baseline1_ch2_rect$val)
mean_baseline3_ch2 <- mean(baseline1_ch2_rect$val)
mean_baseline4_ch2 <- mean(baseline1_ch2_rect$val)


# Subract the mean EMG of baseline to video (channel 1)
video1_ch1_corrected <- subtract_baseline(video1_filtered_ch1_rect$val, mean_baseline1_ch1)
video2_ch1_corrected <- subtract_baseline(video2_filtered_ch1_rect$val, mean_baseline1_ch1)
video3_ch1_corrected <- subtract_baseline(video3_filtered_ch1_rect$val, mean_baseline2_ch1)
video4_ch1_corrected <- subtract_baseline(video4_filtered_ch1_rect$val, mean_baseline2_ch1)
video5_ch1_corrected <- subtract_baseline(video5_filtered_ch1_rect$val, mean_baseline3_ch1)
video6_ch1_corrected <- subtract_baseline(video6_filtered_ch1_rect$val, mean_baseline3_ch1)
video7_ch1_corrected <- subtract_baseline(video7_filtered_ch1_rect$val, mean_baseline4_ch1)
video8_ch1_corrected <- subtract_baseline(video8_filtered_ch1_rect$val, mean_baseline4_ch1)

# Subract the mean EMG of baseline to video (channel 2)
video1_ch2_corrected <- subtract_baseline(video1_filtered_ch2_rect$val, mean_baseline1_ch2)
video2_ch2_corrected <- subtract_baseline(video2_filtered_ch2_rect$val, mean_baseline1_ch2)
video3_ch2_corrected <- subtract_baseline(video3_filtered_ch2_rect$val, mean_baseline2_ch2)
video4_ch2_corrected <- subtract_baseline(video4_filtered_ch2_rect$val, mean_baseline2_ch2)
video5_ch2_corrected <- subtract_baseline(video5_filtered_ch2_rect$val, mean_baseline3_ch2)
video6_ch2_corrected <- subtract_baseline(video6_filtered_ch2_rect$val, mean_baseline3_ch2) 
video7_ch2_corrected <- subtract_baseline(video7_filtered_ch2_rect$val, mean_baseline4_ch2)
video8_ch2_corrected <- subtract_baseline(video8_filtered_ch2_rect$val, mean_baseline4_ch2)

# 
# 
# 
# ------------------------------------- FEATURES EXTRACTION -------------------------------------
# 
# 
# 

features_number <- 39
temp_all_video_features <- matrix(, 1, features_number)

# 
# -----------------VIDEO 1 -------------------
# 

# Extract features of video 1 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video1_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video1_ch2_corrected)

# Extract features of video 1 (rectified)
specific_features_ch1 <- calculate_EMG_features(video1_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video1_filtered_ch2_rect)

# Extract additional features of video 1 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video1_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video1_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 1, sep = '.')
video1_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"Low" , "High"  )

# Append video 1 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video1_features)

# 
# -----------------VIDEO 2 -------------------
# 

# Extract features of video 2 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video2_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video2_ch2_corrected)

# Extract features of video 2 (rectified)
specific_features_ch1 <- calculate_EMG_features(video2_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video2_filtered_ch2_rect)

# Extract additional features of video 2 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video2_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video2_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 2, sep = '.')
video2_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"Low" , "High"  )

# Append video 2 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video2_features)

# 
# -----------------VIDEO 3 -------------------
# 

# Extract features of video 3 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video3_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video3_ch2_corrected)

# Extract features of video 3 (rectified)
specific_features_ch1 <- calculate_EMG_features(video3_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video3_filtered_ch2_rect)

# Extract additional features of video 3 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video3_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video3_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 3, sep = '.')
video3_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"Low" , "Low"  )
	
# Append video 3 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video3_features)

# 
# -----------------VIDEO 4 -------------------
# 

# Extract features of video 4 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video4_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video4_ch2_corrected)

# Extract features of video 4 (rectified)
specific_features_ch1 <- calculate_EMG_features(video4_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video4_filtered_ch2_rect)

# Extract additional features of video 4 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video4_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video4_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 4, sep = '.')
video4_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"Low" , "Low"  )
	
# Append video 4 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video4_features)

# 
# -----------------VIDEO 5 -------------------
# 

# Extract features of video 5 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video5_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video5_ch2_corrected)

# Extract features of video 5 (rectified)
specific_features_ch1 <- calculate_EMG_features(video5_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video5_filtered_ch2_rect)

# Extract additional features of video 5 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video5_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video5_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 5, sep = '.')
video5_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"High" , "High"  )

# Append video 5 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video5_features)

# 
# -----------------VIDEO 6 -------------------
# 

# Extract features of video 6 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video6_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video6_ch2_corrected)

# Extract features of video 6 (rectified)
specific_features_ch1 <- calculate_EMG_features(video6_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video6_filtered_ch2_rect)

# Extract additional features of video 6 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video6_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video6_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 6, sep = '.')
video6_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"High" , "High"  )

# Append video 6 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video6_features)

# 
# -----------------VIDEO 7 -------------------
# 

# Extract features of video 7 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video7_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video7_ch2_corrected)

# Extract features of video 7 (rectified)
specific_features_ch1 <- calculate_EMG_features(video7_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video7_filtered_ch2_rect)

# Extract additional features of video 7 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video7_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video7_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 7, sep = '.')
video7_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"High" , "Low"  )
	
# Append video 7 features to a matrix
temp_all_video_features <- append_new_video_features(temp_all_video_features, video7_features)

# 
# -----------------VIDEO 8 -------------------
# 

# Extract features of video 8 (baseline subtracted)
emg_features_video_ch1_corrected <- calculate_features(video8_ch1_corrected)
emg_features_video_ch2_corrected <- calculate_features(video8_ch2_corrected)

# Extract features of video 8 (rectified)
specific_features_ch1 <- calculate_EMG_features(video8_filtered_ch1_rect)
specific_features_ch2 <- calculate_EMG_features(video8_filtered_ch2_rect)

# Extract additional features of video 8 (baseline subtracted)
new_emg_features_video_ch1_corrected <- calculate_new_EMG_features(video8_ch1_corrected)
new_emg_features_video_ch2_corrected <- calculate_new_EMG_features(video8_ch2_corrected)

# Build a matrix row which can be added to a dataset
id <- paste(num_partecipante, 8, sep = '.')
video8_features <- build_row_for_EMG_dataset(id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected,
	specific_features_ch1, specific_features_ch2,
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	"High" , "Low"  )

# Append video 8 features to a matrix	
temp_all_video_features <- append_new_video_features(temp_all_video_features, video8_features)

 return_all_video_features = function(){	
	all_video_features <- temp_all_video_features[2:nrow(temp_all_video_features),]
	return(all_video_features)
}
