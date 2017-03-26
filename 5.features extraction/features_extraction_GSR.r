.libPaths( c( .libPaths(), "./lib") )
library(signal)
library(pracma)
library(wmtsa)
library(Ryacas)
library(lubridate)
library(pspline)
source("D:/features_extraction/functions.R")

num_partecipante <- readline(prompt= "Numero partecipante: ") 
surname <- readline(prompt= "Cognome partecipante: ") 
sessione <- readline(prompt= "Sessione: ")
timestamp <- readline(prompt= "Data e ora di inizio(aaaa-mm-gg hh:mm:ss) : ") 


time <- strftime(timestamp, format = "%Y-%m-%d %H:%M:%OS" )
start_time <- as.POSIXct(time) #time of video start 



fileNameTemp <- paste("D:/features_extraction/Shimmer_GSR/", num_partecipante,sep= "")
fileNameTemp <- paste(fileNameTemp, "_", sep= "")
fileNameTemp <- paste(fileNameTemp, surname, sep= "")
fileNameTemp <- paste(fileNameTemp, "_Session",sep= "")
fileNameTemp <- paste(fileNameTemp, sessione, sep= "")
dataFilePath <- paste(fileNameTemp, "_Shimmer_GSR_Calibrated_PC.csv", sep = "")

#read file and find the index of the useful lines for each video and for baseline
dataFile = read.csv(dataFilePath, header= TRUE);
useful_seconds <- find_useful_lines(start_time, dataFile,"GSR")

#get the data relative to GSR frequency of the last 30 seconds for each video and for baseline
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

col = 1
time_video1 = dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) ,c(col)];
time_video2 = dataFile[c(useful_seconds$start_video2:useful_seconds$end_video2) ,c(col)];
time_video3 = dataFile[c(useful_seconds$start_video3:useful_seconds$end_video3) ,c(col)];
time_video4 = dataFile[c(useful_seconds$start_video4:useful_seconds$end_video4) ,c(col)];
time_video5 = dataFile[c(useful_seconds$start_video5:useful_seconds$end_video5) ,c(col)];
time_video6 = dataFile[c(useful_seconds$start_video6:useful_seconds$end_video6) ,c(col)];
time_video7 = dataFile[c(useful_seconds$start_video7:useful_seconds$end_video7) ,c(col)];
time_video8 = dataFile[c(useful_seconds$start_video8:useful_seconds$end_video8) ,c(col)];


#get the data relative to HR of the last 30 seconds for each video and for baseline
col = 4;
data_baseline1_HR = dataFile[c(useful_seconds$start_baseline1:useful_seconds$end_baseline1) ,c(col)];
data_video1_HR = dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) ,c(col)];
data_video2_HR = dataFile[c(useful_seconds$start_video2:useful_seconds$end_video2) ,c(col)];
data_baseline2_HR = dataFile[c(useful_seconds$start_baseline2:useful_seconds$end_baseline2) ,c(col)];
data_video3_HR = dataFile[c(useful_seconds$start_video3:useful_seconds$end_video3) ,c(col)];
data_video4_HR = dataFile[c(useful_seconds$start_video4:useful_seconds$end_video4) ,c(col)];
data_baseline3_HR = dataFile[c(useful_seconds$start_baseline3:useful_seconds$end_baseline3) ,c(col)];
data_video5_HR = dataFile[c(useful_seconds$start_video5:useful_seconds$end_video5) ,c(col)];
data_video6_HR = dataFile[c(useful_seconds$start_video6:useful_seconds$end_video6) ,c(col)];
data_baseline4_HR = dataFile[c(useful_seconds$start_baseline4:useful_seconds$end_baseline4) ,c(col)];
data_video7_HR = dataFile[c(useful_seconds$start_video7:useful_seconds$end_video7) ,c(col)];
data_video8_HR = dataFile[c(useful_seconds$start_video8:useful_seconds$end_video8) ,c(col)];



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



# Construct a low and high-pass filter using a butterworth filter design.
#construct EEG filter
filter_GSR<- construct_filter_GSR()



#Apply filter to baseline data
baseline1_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline1)
baseline2_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline2)
baseline3_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline3)
baseline4_filtered <- apply_filter_GSR(filter_GSR, edaSignal_baseline4)


#calculate mean GSR of each baseline
mean_baseline1 <-mean(baseline1_filtered$phasic, trim = 0)
mean_baseline2 <-mean(baseline2_filtered$phasic, trim = 0)
mean_baseline3 <-mean(baseline3_filtered$phasic, trim = 0)
mean_baseline4 <-mean(baseline4_filtered$phasic, trim = 0)



#Apply filter for last 30seconds of VIDEO
video1_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video1)
video2_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video2)
video3_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video3)
video4_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video4)
video5_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video5)
video6_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video6)
video7_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video7)
video8_filtered <- apply_filter_GSR(filter_GSR, edaSignal_video8)



#Subract the mean GSR of baseline to video

video1_corrected <- subtract_baseline(video1_filtered$phasic, mean_baseline1)
video2_corrected <- subtract_baseline(video2_filtered$phasic, mean_baseline1)
video3_corrected <- subtract_baseline(video3_filtered$phasic, mean_baseline2)
video4_corrected <- subtract_baseline(video4_filtered$phasic, mean_baseline2)
video5_corrected <- subtract_baseline(video5_filtered$phasic, mean_baseline3)
video6_corrected <- subtract_baseline(video6_filtered$phasic, mean_baseline3)
video7_corrected <- subtract_baseline(video7_filtered$phasic, mean_baseline4)
video8_corrected <- subtract_baseline(video8_filtered$phasic, mean_baseline4)




# Get Continuous Wavelet Transform of eda signal.
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


#Extract frequency features of video 1
phasic_features <- calculate_features(video1_filtered$phasic)
phasic_features_corrected <- calculate_features(video1_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video1, video1_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video1, video1_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video1.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video1_corrected.cwt)
HR_features <- calculate_HR_features(data_video1_HR, data_baseline1_HR)

print("end")

id <- paste(num_partecipante, 1, sep = '.')

add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected, HR_features, "Low" , "High"  )

#Extract frequency features of video 2
phasic_features <- calculate_features(video2_filtered$phasic)
phasic_features_corrected <- calculate_features(video2_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video2, video2_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video2, video2_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video2.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video2_corrected.cwt)
HR_features <- calculate_HR_features(data_video2_HR, data_baseline1_HR)

id <- paste(num_partecipante, 2, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features, "Low" , "High"  )


#Extract frequency features of video 3
phasic_features <- calculate_features(video3_filtered$phasic)
phasic_features_corrected <- calculate_features(video3_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video3, video3_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video3, video3_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video3.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video3_corrected.cwt)
HR_features <- calculate_HR_features(data_video3_HR, data_baseline2_HR)

id <- paste(num_partecipante, 3, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features, "Low" , "Low"  )


#Extract frequency features of video 4
phasic_features <- calculate_features(video4_filtered$phasic)
phasic_features_corrected <- calculate_features(video4_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video4, video4_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video4, video4_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video4.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video4_corrected.cwt)
HR_features <- calculate_HR_features(data_video4_HR, data_baseline2_HR)

id <- paste(num_partecipante, 4, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features, "Low" , "Low"  )

#Extract frequency features of video 5
phasic_features <- calculate_features(video5_filtered$phasic)
phasic_features_corrected <- calculate_features(video5_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video5, video5_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video5, video5_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video5.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video5_corrected.cwt)
HR_features <- calculate_HR_features(data_video5_HR, data_baseline3_HR)

id <- paste(num_partecipante, 5, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features,  "High" , "High"  )

#Extract frequency features of video 6
phasic_features <- calculate_features(video6_filtered$phasic)
phasic_features_corrected <- calculate_features(video6_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video6, video6_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video6, video6_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video6.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video6_corrected.cwt)
HR_features <- calculate_HR_features(data_video6_HR, data_baseline3_HR)

id <- paste(num_partecipante, 6, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features, "High" , "High"  )


#Extract frequency features of video 7
phasic_features <- calculate_features(video7_filtered$phasic)
phasic_features_corrected <- calculate_features(video7_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video7, video7_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video7, video7_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video7.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video7_corrected.cwt)
HR_features <- calculate_HR_features(data_video7_HR, data_baseline4_HR)

id <- paste(num_partecipante, 7, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features, "High" , "Low"  )


#Extract frequency features of video 8
phasic_features <- calculate_features(video8_filtered$phasic)
phasic_features_corrected <- calculate_features(video8_corrected)
phasic_derivative_features <- calculate_derivative_features(time_video8, video8_filtered$phasic)
phasic_derivative_features_corrected <- calculate_derivative_features(time_video8, video8_corrected) 
phasic_peaks_features <- calculate_peaks_features(edaSignal_video8.cwt)
phasic_peaks_features_corrected <- calculate_peaks_features(edaSignal_video8_corrected.cwt)
HR_features <- calculate_HR_features(data_video8_HR, data_baseline4_HR)

id <- paste(num_partecipante, 8, sep = '.')
add_to_GSR_dataset(id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features,  phasic_peaks_features_corrected,  HR_features,"High" , "Low"  )


