# Append local library
.libPaths( c( .libPaths(), "./lib") )
library(signal)
library(pracma)
library(wmtsa)
library(oce)
library(lubridate)
source("C:/Users/Dany/Desktop/sperimentazione/functions.R")


#get information about participant and create dataFile path
surname <- readline(prompt= "Cognome partecipante: ") 
num_partecipante <- readline(prompt= "Numero partecipante: ") 
timestamp <- readline(prompt= "Ora di inizio: ") 
time <- strptime(timestamp, format = "%H:%M:%S" )
start_time <- as.POSIXct(time) #time of video start 
fileNameTemp <- paste("D:/Mindwave/", surname, sep= "")
dataFilePath <- paste(fileNameTemp, ".filtered.csv",sep= "")
dataFilePath_att_med <- paste(fileNameTemp, ".combined.csv",sep= "")

#read file and find the index of the useful lines for each video and for baseline
dataFile = read.csv(dataFilePath,header= TRUE);
useful_seconds <- find_useful_lines(start_time, dataFile)


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
useful_seconds_att_med <- find_useful_lines(start_time, dataFile_att_med)
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
filter_EEG<- construct_filter()

#Apply filter to baseline data

baseline1_filtered <- apply_filter(filter_EEG,data_baseline1)
baseline2_filtered <- apply_filter(filter_EEG,data_baseline2)
baseline3_filtered <- apply_filter(filter_EEG,data_baseline3)
baseline4_filtered <- apply_filter(filter_EEG,data_baseline4)


#calculate mean of the first baseline( for video 1 and 2)
mean_alpha_baseline1 <-mean(baseline1_filtered$alpha, trim = 0)
mean_beta_baseline1 <-mean(baseline1_filtered$beta, trim = 0)
mean_gamma_baseline1 <-mean(baseline1_filtered$gamma, trim = 0)
mean_delta_baseline1 <-mean(baseline1_filtered$delta, trim = 0)
mean_theta_baseline1 <-mean(baseline1_filtered$theta, trim = 0)

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
video1_filtered <- apply_filter(filter_EEG, data_video1)
video2_filtered <- apply_filter(filter_EEG, data_video2)
video3_filtered <- apply_filter(filter_EEG, data_video3)
video4_filtered <- apply_filter(filter_EEG, data_video4)
video5_filtered <- apply_filter(filter_EEG, data_video5)
video6_filtered <- apply_filter(filter_EEG, data_video6)
video7_filtered <- apply_filter(filter_EEG, data_video7)
video8_filtered <- apply_filter(filter_EEG, data_video8)


#Subract the mean of baseline 1 to video 1
alpha_video1 <- subtract_baseline(video1_filtered$alpha, mean_alpha_baseline1)
beta_video1 <- subtract_baseline(video1_filtered$beta, mean_beta_baseline1)
gamma_video1 <- subtract_baseline(video1_filtered$gamma, mean_gamma_baseline1)
delta_video1 <- subtract_baseline(video1_filtered$delta, mean_delta_baseline1)
theta_video1 <- subtract_baseline(video1_filtered$theta, mean_theta_baseline1)

att_video1 <- subtract_baseline(data_att_video1, mean_att_baseline1)
med_video1 <- subtract_baseline(data_med_video1, mean_med_baseline1)


#Subract the mean of baseline 1 to video 2
alpha_video2 <- subtract_baseline(video2_filtered$alpha, mean_alpha_baseline1)
beta_video2 <- subtract_baseline(video2_filtered$beta, mean_beta_baseline1)
gamma_video2 <- subtract_baseline(video2_filtered$gamma, mean_gamma_baseline1)
delta_video2 <- subtract_baseline(video2_filtered$delta, mean_delta_baseline1)
theta_video2 <- subtract_baseline(video2_filtered$theta, mean_theta_baseline1)

att_video2 <- subtract_baseline(data_att_video2, mean_att_baseline1)
med_video2 <- subtract_baseline(data_med_video2, mean_med_baseline1)


#Subract the mean of baseline 2 to video 3
alpha_video3 <- subtract_baseline(video3_filtered$alpha, mean_alpha_baseline2)
beta_video3 <- subtract_baseline(video3_filtered$beta, mean_beta_baseline2)
gamma_video3 <- subtract_baseline(video3_filtered$gamma, mean_gamma_baseline2)
delta_video3 <- subtract_baseline(video3_filtered$delta, mean_delta_baseline2)
theta_video3 <- subtract_baseline(video3_filtered$theta, mean_theta_baseline2)

att_video3 <- subtract_baseline(data_att_video3, mean_att_baseline2)
med_video3 <- subtract_baseline(data_med_video3, mean_med_baseline2)

#Subract the mean of baseline 2 to video 4
alpha_video4 <- subtract_baseline(video4_filtered$alpha, mean_alpha_baseline2)
beta_video4 <- subtract_baseline(video4_filtered$beta, mean_beta_baseline2)
gamma_video4 <- subtract_baseline(video4_filtered$gamma, mean_gamma_baseline2)
delta_video4 <- subtract_baseline(video4_filtered$delta, mean_delta_baseline2)
theta_video4 <- subtract_baseline(video4_filtered$theta, mean_theta_baseline2)

att_video4 <- subtract_baseline(data_att_video4, mean_att_baseline2)
med_video4 <- subtract_baseline(data_med_video4, mean_med_baseline2)

#Subract the mean of baseline 3 to video 5
alpha_video5 <- subtract_baseline(video5_filtered$alpha, mean_alpha_baseline3)
beta_video5 <- subtract_baseline(video5_filtered$beta, mean_beta_baseline3)
gamma_video5 <- subtract_baseline(video5_filtered$gamma, mean_gamma_baseline3)
delta_video5 <- subtract_baseline(video5_filtered$delta, mean_delta_baseline3)
theta_video5 <- subtract_baseline(video5_filtered$theta, mean_theta_baseline3)

att_video5 <- subtract_baseline(data_att_video5, mean_att_baseline3)
med_video5 <- subtract_baseline(data_med_video5, mean_med_baseline3)

#Subract the mean of baseline 3 to video 6
alpha_video6 <- subtract_baseline(video6_filtered$alpha, mean_alpha_baseline3)
beta_video6 <- subtract_baseline(video6_filtered$beta, mean_beta_baseline3)
gamma_video6 <- subtract_baseline(video6_filtered$gamma, mean_gamma_baseline3)
delta_video6 <- subtract_baseline(video6_filtered$delta, mean_delta_baseline3)
theta_video6 <- subtract_baseline(video6_filtered$theta, mean_theta_baseline3)

att_video6 <- subtract_baseline(data_att_video6, mean_att_baseline3)
med_video6 <- subtract_baseline(data_med_video6, mean_med_baseline3)

#Subract the mean of baseline 4 to video 7
alpha_video7 <- subtract_baseline(video7_filtered$alpha, mean_alpha_baseline4)
beta_video7 <- subtract_baseline(video7_filtered$beta, mean_beta_baseline4)
gamma_video7 <- subtract_baseline(video7_filtered$gamma, mean_gamma_baseline4)
delta_video7 <- subtract_baseline(video7_filtered$delta, mean_delta_baseline4)
theta_video7 <- subtract_baseline(video7_filtered$theta, mean_theta_baseline4)

att_video7 <- subtract_baseline(data_att_video7, mean_att_baseline4)
med_video7 <- subtract_baseline(data_med_video7, mean_med_baseline4)

#Subract the mean of baseline 4 to video 8
alpha_video8 <- subtract_baseline(video8_filtered$alpha, mean_alpha_baseline4)
beta_video8 <- subtract_baseline(video8_filtered$beta, mean_beta_baseline4)
gamma_video8 <- subtract_baseline(video8_filtered$gamma, mean_gamma_baseline4)
delta_video8 <- subtract_baseline(video8_filtered$delta, mean_delta_baseline4)
theta_video8 <- subtract_baseline(video8_filtered$theta, mean_theta_baseline4)

att_video8 <- subtract_baseline(data_att_video8, mean_att_baseline4)
med_video8 <- subtract_baseline(data_med_video8, mean_med_baseline4)


#Extract frequency features of video 1
alpha_features <- calculate_features(alpha_video1)
beta_features <- calculate_features(beta_video1)
gamma_features <- calculate_features(gamma_video1)
delta_features <- calculate_features(delta_video1)
theta_features <- calculate_features(theta_video1)

attention <- calculate_features(att_video1)
meditation <- calculate_features(med_video1)


#Add all the features of video 1 in a vector

id <- paste(num_partecipante, 1, sep = '.')
feature_video1 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd,  
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "Low", "High")
features_video1 <- as.matrix(t(feature_video1))


#Extract  features for video 2 
alpha_features <- calculate_features(alpha_video2)
beta_features <- calculate_features(beta_video2)
gamma_features <- calculate_features(gamma_video2)
delta_features <- calculate_features(delta_video2)
theta_features <- calculate_features(theta_video2)

attention <- calculate_features(att_video2)
meditation <- calculate_features(med_video2)

#Add all the features for video 2 in a vector
id <- paste(num_partecipante, 2, sep = '.')
feature_video2 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "Low", "High" )
features_video2 <- as.matrix(t(feature_video2))

#Extract  features for video 3 
alpha_features <- calculate_features(alpha_video3)
beta_features <- calculate_features(beta_video3)
gamma_features <- calculate_features(gamma_video3)
delta_features <- calculate_features(delta_video3)
theta_features <- calculate_features(theta_video3)

attention <- calculate_features(att_video3)
meditation <- calculate_features(med_video3)

#Add all the features of video 3 in a vector
id <- paste(num_partecipante, 3, sep = '.')
feature_video3 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "Low", "Low")
features_video3 <- as.matrix(t(feature_video3))


#Extract  features for video 4 
alpha_features <- calculate_features(alpha_video4)
beta_features <- calculate_features(beta_video4)
gamma_features <- calculate_features(gamma_video4)
delta_features <- calculate_features(delta_video4)
theta_features <- calculate_features(theta_video4)

attention <- calculate_features(att_video4)
meditation <- calculate_features(med_video4)

#Add all the features of video 4 in a vector
id <- paste(num_partecipante, 4, sep = '.')
feature_video4 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "Low", "Low")
features_video4 <- as.matrix(t(feature_video4))

#Extract  features for video 5 
alpha_features <- calculate_features(alpha_video5)
beta_features <- calculate_features(beta_video5)
gamma_features <- calculate_features(gamma_video5)
delta_features <- calculate_features(delta_video5)
theta_features <- calculate_features(theta_video5)

attention <- calculate_features(att_video5)
meditation <- calculate_features(med_video5)

#Add all the features of video 5 in a vector
id <- paste(num_partecipante, 5, sep = '.')
feature_video5 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "High", "High")
features_video5 <- as.matrix(t(feature_video5))

#Extract  features for video 6 
alpha_features <- calculate_features(alpha_video6)
beta_features <- calculate_features(beta_video6)
gamma_features <- calculate_features(gamma_video6)
delta_features <- calculate_features(delta_video6)
theta_features <- calculate_features(theta_video6)

attention <- calculate_features(att_video6)
meditation <- calculate_features(med_video6)

#Add all the features of video 6 in a vector
id <- paste(num_partecipante, 6, sep = '.')
feature_video6 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "High", "High")
features_video6 <- as.matrix(t(feature_video6))

#Extract  features for video 7 
alpha_features <- calculate_features(alpha_video7)
beta_features <- calculate_features(beta_video7)
gamma_features <- calculate_features(gamma_video7)
delta_features <- calculate_features(delta_video7)
theta_features <- calculate_features(theta_video7)

attention <- calculate_features(att_video7)
meditation <- calculate_features(med_video7)

#Add all the features of video 7 in a vector
id <- paste(num_partecipante, 7, sep = '.')
feature_video7 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "High", "Low")
features_video7 <- as.matrix(t(feature_video7))

#Extract  features for video 8 
alpha_features <- calculate_features(alpha_video8)
beta_features <- calculate_features(beta_video8)
gamma_features <- calculate_features(gamma_video8)
delta_features <- calculate_features(delta_video8)
theta_features <- calculate_features(theta_video8)

attention <- calculate_features(att_video8)
meditation <- calculate_features(med_video8)

#Add all the features of video 8 in a vector
id <- paste(num_partecipante, 8, sep = '.')
feature_video8 <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$sd, 
attention$mean, attention$min, attention$max, attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$sd, "High", "Low")
features_video8 <- as.matrix(t(feature_video8))


#append the features on a file
EEG_dataset <- "D:/MindWave/dataset/EEG_dataset.arff"

write.table(features_video1, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
write.table(features_video2, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)
write.table(features_video3, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)
write.table(features_video4, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)
write.table(features_video5, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)
write.table(features_video6, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)
write.table(features_video7, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)
write.table(features_video8, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, append=TRUE)

