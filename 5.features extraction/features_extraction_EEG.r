# Append local library
.libPaths( c( .libPaths(), "./lib") )
library(signal)
library(pracma)
library(wmtsa)
library(oce)
library(lubridate)
source("D:/features_extraction/functions.R")


#get information about participant and create dataFile path
num_partecipante <- readline(prompt= "Numero partecipante: ") 
surname <- readline(prompt= "Cognome partecipante: ") 
timestamp <- readline(prompt= "Ora di inizio(hh:mm:ss): ") 
time <- strptime(timestamp, format = "%H:%M:%S" )
start_time <- as.POSIXct(time) #time of video start 
fileNameTemp <- paste("D:/features_extraction/Mindwave/", num_partecipante,  sep= "")
fileNameTemp <- paste(fileNameTemp, "_", sep= "")
fileNameTemp <- paste(fileNameTemp, surname, sep= "")
dataFilePath <- paste(fileNameTemp, ".filtered.csv",sep= "")
dataFilePath_att_med <- paste(fileNameTemp, ".combined.csv",sep= "")

#read file and find the index of the useful lines for each video and for baseline
dataFile = read.csv(dataFilePath,header= TRUE);
useful_seconds <- find_useful_lines(start_time, dataFile,"EEG")


#get the data relative to EEG frequency of the last 30 seconds for each video and for baseline
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

#get the data relative to attention of the last 30 seconds for each video and for baseline
dataFile_att_med = read.csv(dataFilePath_att_med,header= TRUE);
useful_seconds_att_med <- find_useful_lines(start_time, dataFile_att_med,"EEG")
col = 3;

data_att_baseline1 = dataFile_att_med[c(useful_seconds_att_med$start_baseline1:useful_seconds_att_med$end_baseline1) ,c(col)];
data_att_video1 = dataFile_att_med[c(useful_seconds_att_med$start_video1:useful_seconds_att_med$end_video1) ,c(col)];
data_att_video2 = dataFile_att_med[c(useful_seconds_att_med$start_video2:useful_seconds_att_med$end_video2) ,c(col)];
data_att_baseline2 = dataFile_att_med[c(useful_seconds_att_med$start_baseline2:useful_seconds_att_med$end_baseline2) ,c(col)];
data_att_video3 = dataFile_att_med[c(useful_seconds_att_med$start_video3:useful_seconds_att_med$end_video3) ,c(col)];
data_att_video4 = dataFile_att_med[c(useful_seconds_att_med$start_video4:useful_seconds_att_med$end_video4) ,c(col)];
data_att_baseline3 = dataFile_att_med[c(useful_seconds_att_med$start_baseline3:useful_seconds_att_med$end_baseline3) ,c(col)];
data_att_video5 = dataFile_att_med[c(useful_seconds_att_med$start_video5:useful_seconds_att_med$end_video5) ,c(col)];
data_att_video6 = dataFile_att_med[c(useful_seconds_att_med$start_video6:useful_seconds_att_med$end_video6) ,c(col)];
data_att_baseline4 = dataFile_att_med[c(useful_seconds_att_med$start_baseline4:useful_seconds_att_med$end_baseline4) ,c(col)];
data_att_video7 = dataFile_att_med[c(useful_seconds_att_med$start_video7:useful_seconds_att_med$end_video7) ,c(col)];
data_att_video8 = dataFile_att_med[c(useful_seconds_att_med$start_video8:useful_seconds_att_med$end_video8) ,c(col)];


#get the data relative to meditation of the last 30 seconds for each video and for baseline
col = 4;
data_med_baseline1 = dataFile_att_med[c(useful_seconds_att_med$start_baseline1:useful_seconds_att_med$end_baseline1) ,c(col)];
data_med_video1 = dataFile_att_med[c(useful_seconds_att_med$start_video1:useful_seconds_att_med$end_video1) ,c(col)];
data_med_video2 = dataFile_att_med[c(useful_seconds_att_med$start_video2:useful_seconds_att_med$end_video2) ,c(col)];
data_med_baseline2 = dataFile_att_med[c(useful_seconds_att_med$start_baseline2:useful_seconds_att_med$end_baseline2) ,c(col)];
data_med_video3 = dataFile_att_med[c(useful_seconds_att_med$start_video3:useful_seconds_att_med$end_video3) ,c(col)];
data_med_video4 = dataFile_att_med[c(useful_seconds_att_med$start_video4:useful_seconds_att_med$end_video4) ,c(col)];
data_med_baseline3 = dataFile_att_med[c(useful_seconds_att_med$start_baseline3:useful_seconds_att_med$end_baseline3) ,c(col)];
data_med_video5 = dataFile_att_med[c(useful_seconds_att_med$start_video5:useful_seconds_att_med$end_video5) ,c(col)];
data_med_video6 = dataFile_att_med[c(useful_seconds_att_med$start_video6:useful_seconds_att_med$end_video6) ,c(col)];
data_med_baseline4 = dataFile_att_med[c(useful_seconds_att_med$start_baseline4:useful_seconds_att_med$end_baseline4) ,c(col)];
data_med_video7 = dataFile_att_med[c(useful_seconds_att_med$start_video7:useful_seconds_att_med$end_video7) ,c(col)];
data_med_video8 = dataFile_att_med[c(useful_seconds_att_med$start_video8:useful_seconds_att_med$end_video8) ,c(col)];


#construct EEG filter
filter_EEG<- construct_filter_EEG()

#Apply filter to baseline data

baseline1_filtered <- apply_filter_EEG(filter_EEG,data_baseline1)
baseline2_filtered <- apply_filter_EEG(filter_EEG,data_baseline2)
baseline3_filtered <- apply_filter_EEG(filter_EEG,data_baseline3)
baseline4_filtered <- apply_filter_EEG(filter_EEG,data_baseline4)


#calculate mean of the first baseline( for video 1 and 2)

mean_alpha_baseline1 <-mean(baseline1_filtered$alpha, trim = 0)
mean_beta_baseline1 <-mean(baseline1_filtered$beta, trim = 0)
mean_gamma_baseline1 <-mean(baseline1_filtered$gamma, trim = 0)
mean_delta_baseline1 <-mean(baseline1_filtered$delta, trim = 0)
mean_theta_baseline1 <-mean(baseline1_filtered$theta, trim = 0)

#print("mean_alpha_baseline1")
#print(mean_alpha_baseline1)
#print("mean_beta_baseline1")
#print(mean_beta_baseline1)
#print("mean_gamma_baseline1")
#print(mean_gamma_baseline1)
#print("mean_delta_baseline1")
#print(mean_delta_baseline1)
#print("mean_theta_baseline1")
#print(mean_theta_baseline1)


#calculate mean of the second baseline( for video 3 and 4)
mean_alpha_baseline2 <-mean(baseline2_filtered$alpha, trim = 0)
mean_beta_baseline2 <-mean(baseline2_filtered$beta, trim = 0)
mean_gamma_baseline2 <-mean(baseline2_filtered$gamma, trim = 0)
mean_delta_baseline2 <-mean(baseline2_filtered$delta, trim = 0)
mean_theta_baseline2 <-mean(baseline2_filtered$theta, trim = 0)

#calculate mean of the third baseline( for video 5 and 6)
mean_alpha_baseline3 <-mean(baseline3_filtered$alpha, trim = 0)
mean_beta_baseline3 <-mean(baseline3_filtered$beta, trim = 0)
mean_gamma_baseline3 <-mean(baseline3_filtered$gamma, trim = 0)
mean_delta_baseline3 <-mean(baseline3_filtered$delta, trim = 0)
mean_theta_baseline3 <-mean(baseline3_filtered$theta, trim = 0)

#calculate mean of the fourth baseline( for video 7 and 8)
mean_alpha_baseline4 <-mean(baseline4_filtered$alpha, trim = 0)
mean_beta_baseline4 <-mean(baseline4_filtered$beta, trim = 0)
mean_gamma_baseline4 <-mean(baseline4_filtered$gamma, trim = 0)
mean_delta_baseline4 <-mean(baseline4_filtered$delta, trim = 0)
mean_theta_baseline4 <-mean(baseline4_filtered$theta, trim = 0)

#calculate mean of the attention and meditation baseline
mean_att_baseline1 <- mean(data_att_baseline1, trim = 0)
mean_att_baseline2 <- mean(data_att_baseline2, trim = 0)
mean_att_baseline3 <- mean(data_att_baseline3, trim = 0)
mean_att_baseline4 <- mean(data_att_baseline4, trim = 0)

mean_med_baseline1 <- mean(data_med_baseline1, trim = 0)
mean_med_baseline2 <- mean(data_med_baseline2, trim = 0)
mean_med_baseline3 <- mean(data_med_baseline3, trim = 0)
mean_med_baseline4 <- mean(data_med_baseline4, trim = 0)

#Apply filter for last 30seconds of VIDEO
video1_filtered <- apply_filter_EEG(filter_EEG, data_video1)
video2_filtered <- apply_filter_EEG(filter_EEG, data_video2)
video3_filtered <- apply_filter_EEG(filter_EEG, data_video3)
video4_filtered <- apply_filter_EEG(filter_EEG, data_video4)
video5_filtered <- apply_filter_EEG(filter_EEG, data_video5)
video6_filtered <- apply_filter_EEG(filter_EEG, data_video6)
video7_filtered <- apply_filter_EEG(filter_EEG, data_video7)
video8_filtered <- apply_filter_EEG(filter_EEG, data_video8)


#Subract the mean of baseline 1 to video 1
alpha_video1_corrected <- subtract_baseline(video1_filtered$alpha, mean_alpha_baseline1)
beta_video1_corrected <- subtract_baseline(video1_filtered$beta, mean_beta_baseline1)
gamma_video1_corrected <- subtract_baseline(video1_filtered$gamma, mean_gamma_baseline1)
delta_video1_corrected <- subtract_baseline(video1_filtered$delta, mean_delta_baseline1)
theta_video1_corrected <- subtract_baseline(video1_filtered$theta, mean_theta_baseline1)

att_video1_corrected <- subtract_baseline(data_att_video1, mean_att_baseline1)
med_video1_corrected <- subtract_baseline(data_med_video1, mean_med_baseline1)


#Subract the mean of baseline 1 to video 2
alpha_video2_corrected <- subtract_baseline(video2_filtered$alpha, mean_alpha_baseline1)
beta_video2_corrected <- subtract_baseline(video2_filtered$beta, mean_beta_baseline1)
gamma_video2_corrected <- subtract_baseline(video2_filtered$gamma, mean_gamma_baseline1)
delta_video2_corrected <- subtract_baseline(video2_filtered$delta, mean_delta_baseline1)
theta_video2_corrected <- subtract_baseline(video2_filtered$theta, mean_theta_baseline1)

att_video2_corrected <- subtract_baseline(data_att_video2, mean_att_baseline1)
med_video2_corrected <- subtract_baseline(data_med_video2, mean_med_baseline1)


#Subract the mean of baseline 2 to video 3
alpha_video3_corrected <- subtract_baseline(video3_filtered$alpha, mean_alpha_baseline2)
beta_video3_corrected <- subtract_baseline(video3_filtered$beta, mean_beta_baseline2)
gamma_video3_corrected <- subtract_baseline(video3_filtered$gamma, mean_gamma_baseline2)
delta_video3_corrected <- subtract_baseline(video3_filtered$delta, mean_delta_baseline2)
theta_video3_corrected <- subtract_baseline(video3_filtered$theta, mean_theta_baseline2)

att_video3_corrected <- subtract_baseline(data_att_video3, mean_att_baseline2)
med_video3_corrected <- subtract_baseline(data_med_video3, mean_med_baseline2)

#Subract the mean of baseline 2 to video 4
alpha_video4_corrected <- subtract_baseline(video4_filtered$alpha, mean_alpha_baseline2)
beta_video4_corrected <- subtract_baseline(video4_filtered$beta, mean_beta_baseline2)
gamma_video4_corrected <- subtract_baseline(video4_filtered$gamma, mean_gamma_baseline2)
delta_video4_corrected <- subtract_baseline(video4_filtered$delta, mean_delta_baseline2)
theta_video4_corrected <- subtract_baseline(video4_filtered$theta, mean_theta_baseline2)

att_video4_corrected <- subtract_baseline(data_att_video4, mean_att_baseline2)
med_video4_corrected <- subtract_baseline(data_med_video4, mean_med_baseline2)

#Subract the mean of baseline 3 to video 5
alpha_video5_corrected <- subtract_baseline(video5_filtered$alpha, mean_alpha_baseline3)
beta_video5_corrected <- subtract_baseline(video5_filtered$beta, mean_beta_baseline3)
gamma_video5_corrected <- subtract_baseline(video5_filtered$gamma, mean_gamma_baseline3)
delta_video5_corrected <- subtract_baseline(video5_filtered$delta, mean_delta_baseline3)
theta_video5_corrected <- subtract_baseline(video5_filtered$theta, mean_theta_baseline3)

att_video5_corrected <- subtract_baseline(data_att_video5, mean_att_baseline3)
med_video5_corrected <- subtract_baseline(data_med_video5, mean_med_baseline3)

#Subract the mean of baseline 3 to video 6
alpha_video6_corrected <- subtract_baseline(video6_filtered$alpha, mean_alpha_baseline3)
beta_video6_corrected <- subtract_baseline(video6_filtered$beta, mean_beta_baseline3)
gamma_video6_corrected <- subtract_baseline(video6_filtered$gamma, mean_gamma_baseline3)
delta_video6_corrected <- subtract_baseline(video6_filtered$delta, mean_delta_baseline3)
theta_video6_corrected <- subtract_baseline(video6_filtered$theta, mean_theta_baseline3)

att_video6_corrected <- subtract_baseline(data_att_video6, mean_att_baseline3)
med_video6_corrected <- subtract_baseline(data_med_video6, mean_med_baseline3)

#Subract the mean of baseline 4 to video 7
alpha_video7_corrected <- subtract_baseline(video7_filtered$alpha, mean_alpha_baseline4)
beta_video7_corrected <- subtract_baseline(video7_filtered$beta, mean_beta_baseline4)
gamma_video7_corrected <- subtract_baseline(video7_filtered$gamma, mean_gamma_baseline4)
delta_video7_corrected <- subtract_baseline(video7_filtered$delta, mean_delta_baseline4)
theta_video7_corrected <- subtract_baseline(video7_filtered$theta, mean_theta_baseline4)

att_video7_corrected <- subtract_baseline(data_att_video7, mean_att_baseline4)
med_video7_corrected <- subtract_baseline(data_med_video7, mean_med_baseline4)

#Subract the mean of baseline 4 to video 8
alpha_video8_corrected <- subtract_baseline(video8_filtered$alpha, mean_alpha_baseline4)
beta_video8_corrected <- subtract_baseline(video8_filtered$beta, mean_beta_baseline4)
gamma_video8_corrected <- subtract_baseline(video8_filtered$gamma, mean_gamma_baseline4)
delta_video8_corrected <- subtract_baseline(video8_filtered$delta, mean_delta_baseline4)
theta_video8_corrected <- subtract_baseline(video8_filtered$theta, mean_theta_baseline4)

att_video8_corrected <- subtract_baseline(data_att_video8, mean_att_baseline4)
med_video8_corrected <- subtract_baseline(data_med_video8, mean_med_baseline4)


#Extract frequency features of video 1
alpha_features <- calculate_features(video1_filtered$alpha)
beta_features <- calculate_features(video1_filtered$beta)
gamma_features <- calculate_features(video1_filtered$gamma)
delta_features <- calculate_features(video1_filtered$delta)
theta_features <- calculate_features(video1_filtered$theta)

#Extract attention and meditation features of video 1

attention <- calculate_features(data_att_video1)
meditation <- calculate_features(data_med_video1)

#Extract attention and meditation features of video 1 ( baseline subtracted)

alpha_features_corrected <- calculate_features(alpha_video1_corrected)
beta_features_corrected <- calculate_features( beta_video1_corrected)
gamma_features_corrected <- calculate_features(gamma_video1_corrected)
delta_features_corrected <- calculate_features(delta_video1_corrected)
theta_features_corrected <- calculate_features( theta_video1_corrected)

#Extract attention and meditation features of video 1 ( baseline subtracted)

attention_corrected <- calculate_features(att_video1_corrected)
meditation_corrected <- calculate_features(med_video1_corrected)

ratio_frequencies_baseline1 <- calculate_ratio_frequencies(baseline1_filtered)
ratio_frequencies_video1 <- calculate_ratio_frequencies(video1_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video1, ratio_frequencies_baseline1)


#Add all the features of video 1 to dataset

id <- paste(num_partecipante, 1, sep = '.')

add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected,  diff_frequencies,
 "Low", "High" )



#Extract  features for video 2 
alpha_features <- calculate_features(video2_filtered$alpha)
beta_features <- calculate_features(video2_filtered$beta)
gamma_features <- calculate_features(video2_filtered$gamma)
delta_features <- calculate_features(video2_filtered$delta)
theta_features <- calculate_features(video2_filtered$theta)


attention <- calculate_features(data_att_video2)
meditation <- calculate_features(data_med_video2)

alpha_features_corrected <- calculate_features(alpha_video2_corrected)
beta_features_corrected <- calculate_features(beta_video2_corrected)
gamma_features_corrected <- calculate_features(gamma_video2_corrected)
delta_features_corrected <- calculate_features(delta_video2_corrected)
theta_features_corrected <- calculate_features(theta_video2_corrected)

attention_corrected <- calculate_features(att_video2_corrected)
meditation_corrected <- calculate_features(med_video2_corrected)

#ratio_frequencies_baseline1 <- calculate_ratio_frequencies(baseline1_filtered)
ratio_frequencies_video2 <- calculate_ratio_frequencies(video2_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video2, ratio_frequencies_baseline1)

#Add all the features for video to dataset
id <- paste(num_partecipante, 2, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies, "Low", "High" )


#Extract  features for video 3 
alpha_features <- calculate_features(video3_filtered$alpha)
beta_features <- calculate_features(video3_filtered$beta)
gamma_features <- calculate_features(video3_filtered$gamma)
delta_features <- calculate_features(video3_filtered$delta)
theta_features <- calculate_features(video3_filtered$theta)

attention <- calculate_features(data_att_video3)
meditation <- calculate_features(data_med_video3)

alpha_features_corrected <- calculate_features(alpha_video3_corrected)
beta_features_corrected <- calculate_features(beta_video3_corrected)
gamma_features_corrected <- calculate_features(gamma_video3_corrected)
delta_features_corrected <- calculate_features(delta_video3_corrected)
theta_features_corrected <- calculate_features(theta_video3_corrected)

attention_corrected <- calculate_features(att_video3_corrected)
meditation_corrected <- calculate_features(med_video3_corrected)

ratio_frequencies_baseline2 <- calculate_ratio_frequencies(baseline2_filtered)
ratio_frequencies_video3 <- calculate_ratio_frequencies(video3_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video3, ratio_frequencies_baseline2)

#Add all the features of video 3 to dataset
id <- paste(num_partecipante, 3, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies, "Low", "Low" )



#Extract  features for video 4 
alpha_features <- calculate_features(video4_filtered$alpha)
beta_features <- calculate_features(video4_filtered$beta)
gamma_features <- calculate_features(video4_filtered$gamma)
delta_features <- calculate_features(video4_filtered$delta)
theta_features <- calculate_features(video4_filtered$theta)

attention <- calculate_features(data_att_video4)
meditation <- calculate_features(data_med_video4)

alpha_features_corrected <- calculate_features(alpha_video4_corrected)
beta_features_corrected <- calculate_features(beta_video4_corrected)
gamma_features_corrected <- calculate_features(gamma_video4_corrected)
delta_features_corrected <- calculate_features(delta_video4_corrected)
theta_features_corrected <- calculate_features(theta_video4_corrected)

attention_corrected <- calculate_features(att_video4_corrected)
meditation_corrected <- calculate_features(med_video4_corrected)

#ratio_frequencies_baseline2 <- calculate_ratio_frequencies(baseline2_filtered)
ratio_frequencies_video4 <- calculate_ratio_frequencies(video4_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video4, ratio_frequencies_baseline2)

#Add all the features of video 4 to dataset
id <- paste(num_partecipante, 4, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies ,"Low", "Low" )


#Extract  features for video 5 
alpha_features <- calculate_features(video5_filtered$alpha)
beta_features <- calculate_features(video5_filtered$beta)
gamma_features <- calculate_features(video5_filtered$gamma)
delta_features <- calculate_features(video5_filtered$delta)
theta_features <- calculate_features(video5_filtered$theta)

attention <- calculate_features(data_att_video5)
meditation <- calculate_features(data_med_video5)

alpha_features_corrected <- calculate_features(alpha_video5_corrected)
beta_features_corrected <- calculate_features(beta_video5_corrected)
gamma_features_corrected <- calculate_features(gamma_video5_corrected)
delta_features_corrected <- calculate_features(delta_video5_corrected)
theta_features_corrected <- calculate_features(theta_video5_corrected)

attention_corrected <- calculate_features(att_video5_corrected)
meditation_corrected <- calculate_features(med_video5_corrected)


ratio_frequencies_baseline3 <- calculate_ratio_frequencies(baseline3_filtered)
ratio_frequencies_video5 <- calculate_ratio_frequencies(video5_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video5, ratio_frequencies_baseline3)


#Add all the features of video 5 to dataset
id <- paste(num_partecipante, 5, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies , "High", "High" )


#Extract  features for video 6 
alpha_features <- calculate_features(video6_filtered$alpha)
beta_features <- calculate_features(video6_filtered$beta)
gamma_features <- calculate_features(video6_filtered$gamma)
delta_features <- calculate_features(video6_filtered$delta)
theta_features <- calculate_features(video6_filtered$theta)

attention <- calculate_features(data_att_video6)
meditation <- calculate_features(data_med_video6)

alpha_features_corrected <- calculate_features(alpha_video6_corrected)
beta_features_corrected <- calculate_features(beta_video6_corrected)
gamma_features_corrected <- calculate_features(gamma_video6_corrected)
delta_features_corrected <- calculate_features(delta_video6_corrected)
theta_features_corrected <- calculate_features(theta_video6_corrected)

attention_corrected <- calculate_features(att_video6_corrected)
meditation_corrected <- calculate_features(med_video6_corrected)

#ratio_frequencies_baseline3 <- calculate_ratio_frequencies(baseline3_filtered)
ratio_frequencies_video6 <- calculate_ratio_frequencies(video6_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video6, ratio_frequencies_baseline3)

#Add all the features of video 6 to dataset
id <- paste(num_partecipante, 6, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies ,"High", "High" )


#Extract  features for video to dataset
alpha_features <- calculate_features(video7_filtered$alpha)
beta_features <- calculate_features(video7_filtered$beta)
gamma_features <- calculate_features(video7_filtered$gamma)
delta_features <- calculate_features(video7_filtered$delta)
theta_features <- calculate_features(video7_filtered$theta)

attention <- calculate_features(data_att_video7)
meditation <- calculate_features(data_med_video7)

alpha_features_corrected <- calculate_features(alpha_video7_corrected)
beta_features_corrected <- calculate_features(beta_video7_corrected)
gamma_features_corrected <- calculate_features(gamma_video7_corrected)
delta_features_corrected <- calculate_features(delta_video7_corrected)
theta_features_corrected <- calculate_features(theta_video7_corrected)

attention_corrected <- calculate_features(att_video7_corrected)
meditation_corrected <- calculate_features(med_video7_corrected)

ratio_frequencies_baseline4 <- calculate_ratio_frequencies(baseline4_filtered)
ratio_frequencies_video7 <- calculate_ratio_frequencies(video7_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video7, ratio_frequencies_baseline4)

#Add all the features of video 7 to dataset
id <- paste(num_partecipante, 7, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies, "High", "Low" )

#Extract  features for video 8 
alpha_features <- calculate_features(video8_filtered$alpha)
beta_features <- calculate_features(video8_filtered$beta)
gamma_features <- calculate_features(video8_filtered$gamma)
delta_features <- calculate_features(video8_filtered$delta)
theta_features <- calculate_features(video8_filtered$theta)

attention <- calculate_features(data_att_video8)
meditation <- calculate_features(data_med_video8)

alpha_features_corrected <- calculate_features(alpha_video8_corrected)
beta_features_corrected <- calculate_features(beta_video8_corrected)
gamma_features_corrected <- calculate_features(gamma_video8_corrected)
delta_features_corrected <- calculate_features(delta_video8_corrected)
theta_features_corrected <- calculate_features(theta_video8_corrected)

attention_corrected <- calculate_features(att_video8_corrected)
meditation_corrected <- calculate_features(med_video8_corrected)

#ratio_frequencies_baseline4 <- calculate_ratio_frequencies(baseline3_filtered)
ratio_frequencies_video8 <- calculate_ratio_frequencies(video8_filtered)
diff_frequencies <- calculate_diff_ratio_frequencies(ratio_frequencies_video8, ratio_frequencies_baseline4)

#Add all the features of video 8 to dataset
id <- paste(num_partecipante, 8, sep = '.')
add_to_EEG_dataset(id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frequencies, "High", "Low" )

