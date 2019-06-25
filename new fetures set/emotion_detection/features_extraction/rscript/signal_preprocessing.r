# Author: Daniela Girardi, Garofalo Antonio
# This script provides to apply a preprocessing to the signal:
# 	- it calculates the useful line starting from raw data of the signal 
# 	- it builds several butterworth filters to apply on each signal
# 	- it has the task to subtract the signal baseline from data signal

# 
# 
# 
# ------------------------------------- USEFUL LINES SELECTION -------------------------------------
# 
# 
# 

# It return the row index of temp, starting scan from time_pos
find_row_index = function(time_pos, i, temp){
	n <- length(time_pos)
	while(time_pos[i] != temp){
		i <- i+1
	}

	return(i)
}

# It returns a list of index which matches with the useful data of last 30 seconds of each video and the relative baseline
# args:
# 	- start_time: string which indicates the start timestamp in one of the following format: "%H:%M:%S", "%Y-%m-%d %H:%M:%S"
# 	- data_file: the data frame in which the useful lines have to be found
# 	- physiological_measure: a string "EEG", "EMG, "GSR" which indicates the physiological measure, necessary to define the data_file timestamp
find_useful_lines = function(start_time, data_file, physiological_measure){

	timestamp <- data_file[,c(1)];

	if(physiological_measure == "EEG"){
		start_time_format <- strptime(start_time, format = "%H:%M:%S" )
		time <- strptime(timestamp, format = "%H:%M:%S" )
	} else{
		start_time_format <- strptime(start_time, format = "%Y-%m-%d %H:%M:%S" )
		time <- strptime(timestamp, format = "%Y-%m-%d %H:%M:%S" )
	} 

	start_time_data <- as.POSIXct(start_time_format)
	time_pos <- as.POSIXct(time)


	i <- 1

	temp <- start_time_data + seconds(2)
	start_baseline1 <- find_row_index(time_pos, i, temp) + 1
	# cat("BASELINE 1 START ROW: ", start_baseline1, "\n")

	temp <- start_time_data + seconds(32)
	end_baseline1 <- find_row_index(time_pos, start_baseline1 - 1, temp)
	# cat("BASELINE 1 END ROW: ", end_baseline1, "\n")

	temp <- start_time_data + minutes(1) + seconds(5)
	start_video1 <- find_row_index(time_pos, end_baseline1, temp) + 1 
	# cat("VIDEO 1 START ROW: ", start_video1, "\n")

	temp <- start_time_data + minutes(1) + seconds(35)
	end_video1 <- find_row_index(time_pos, start_video1 - 1, temp)	
	# cat("VIDEO 1 END ROW: ", end_video1, "\n")

	temp <- start_time_data + minutes(2) + seconds(18)
	start_video2 <- find_row_index(time_pos, end_video1, temp) +1 
	# cat("VIDEO 2 START ROW: ", start_video2, "\n")

	temp <- start_time_data + minutes(2) + seconds(48)
	end_video2 <- find_row_index(time_pos, start_video2 - 1, temp) 
	# cat("VIDEO 2 END ROW: ", end_video2, "\n\n")



	temp <- start_time_data + minutes(3)
	start_baseline2 <- find_row_index(time_pos,end_video1,temp) +1 
	# cat("BASELINE 2 START ROW: ", start_baseline2, "\n")

	temp <- start_time_data + minutes(3) + seconds(30)
	end_baseline2 <- find_row_index(time_pos,start_baseline2 -1, temp)  
	# cat("BASELINE 2 END ROW: ", end_baseline2, "\n")

	temp <- start_time_data + minutes(4) + seconds(3)
	start_video3 <- find_row_index(time_pos,end_baseline2, temp)   +1 
	# cat("VIDEO 3 START ROW: ", start_video3, "\n")
	 
	temp <- start_time_data + minutes(4) + seconds(33)
	end_video3 <- find_row_index(time_pos,start_video3 -1, temp) 
	# cat("VIDEO 3 END ROW: ", end_video3, "\n")

	temp <- start_time_data + minutes(5) + seconds(16)
	start_video4 <- find_row_index(time_pos,end_video3 ,temp) +1 
	# cat("VIDEO 4 START ROW: ", start_video4, "\n")

	temp <- start_time_data + minutes(5) + seconds(46)
	end_video4 <- find_row_index(time_pos, start_video4 - 1, temp) 
	# cat("VIDEO 4 END ROW: ", end_video4, "\n\n")



	temp <- start_time_data + minutes(5) + seconds(58)
	start_baseline3 <- find_row_index(time_pos,end_video4 ,temp) +1 
	# cat("BASELINE 3 START ROW: ", start_baseline3, "\n")

	temp <- start_time_data + minutes(6) + seconds(28)
	end_baseline3 <- find_row_index(time_pos,start_baseline3 -1, temp)  
	# cat("BASELINE 3 END ROW: ", end_baseline3, "\n")

	temp <- start_time_data + minutes(7) + seconds(1)
	start_video5 <- find_row_index(time_pos,end_baseline3, temp) + 	1
	# cat("VIDEO 5 START ROW: ", start_video5, "\n")

	temp <- start_time_data + minutes(7) + seconds(31)
	end_video5 <- find_row_index(time_pos, start_video5 - 1, temp)
	# cat("VIDEO 5 END ROW: ", end_video5, "\n")

	temp <- start_time_data + minutes(8) + seconds(14)
	start_video6 <- find_row_index(time_pos, end_video5, temp) +1 
	# cat("VIDEO 6 START ROW: ", start_video6, "\n")

	temp <- start_time_data + minutes(8) + seconds(44)
	end_video6 <- find_row_index(time_pos, start_video6 - 1, temp)
	# cat("VIDEO 6 END ROW: ", end_video6, "\n\n")



	temp <- start_time_data + minutes(8) + seconds(56)
	start_baseline4 <- find_row_index(time_pos, end_video6, temp) + 1
	# cat("BASELINE 4 START ROW: ", start_baseline4, "\n")

	temp <- start_time_data + minutes(9) + seconds(26)
	end_baseline4 <- find_row_index(time_pos, start_baseline4 - 1, temp)
	# cat("BASELINE 4 END ROW: ", start_baseline4, "\n")

	temp <- start_time_data + minutes(9) + seconds(59) 
	start_video7 <- find_row_index(time_pos, end_baseline4, temp) + 1
	# cat("VIDEO 7 START ROW: ", start_video7, "\n")

	temp <- start_time_data + minutes(10) + seconds(29)
	end_video7 <- find_row_index(time_pos, start_video7 - 1, temp) 
	# cat("VIDEO 7 END ROW: ", start_video7, "\n")

	temp <- start_time_data + minutes(11) + seconds(12)
	start_video8 <- find_row_index(time_pos, end_video7, temp) +1
	# cat("VIDEO 8 START ROW: ", start_video8, "\n")

	temp <- start_time_data + minutes(11) + seconds(42)	
	end_video8 <- find_row_index(time_pos, start_video8 - 1, temp) +1
	# cat("VIDEO 8 END ROW: ", end_video8, "\n\n")



	useful_seconds <- list( "start_baseline1" = start_baseline1, "end_baseline1" = end_baseline1, "start_video1" = start_video1, "end_video1" = end_video1, 
							"start_video2" = start_video2, "end_video2" = end_video2, "start_baseline2" = start_baseline2, "end_baseline2" = end_baseline2,
						"start_video3" = start_video3, "end_video3" = end_video3, "start_video4" = start_video4, "end_video4" = end_video4, 
							"start_baseline3" = start_baseline3, "end_baseline3" = end_baseline3, "start_video5" = start_video5, "end_video5" = end_video5, 
						"start_video6" = start_video6, "end_video6" = end_video6, "start_baseline4" = start_baseline4, "end_baseline4" = end_baseline4, 
							"start_video7" = start_video7, "end_video7" = end_video7, "start_video8" = start_video8, "end_video8" = end_video8
							 )

	return(useful_seconds)

}

# 
# 
# 
# ------------------------------------- FILTERS BUILDER -------------------------------------
# 
# 
# 

# Construct a band-pass filter using a butterworth filter design
#  (alpha: 8-12hz, beta 12-30 hz, gamma 30-80hz, delta 0-4 hz, theta 4-8 hz)
construct_filter_EEG = function(){

	bf_alpha <- butter(2, c(8/1000,12/1000), type="pass")
	bf_beta <- butter(2, c(12/1000,30/1000), type="pass")
	bf_gamma <- butter(2, c(30/1000,80/1000), type="pass")
	bf_delta <- butter(2, c(0,4/1000), type="pass")
	bf_theta <- butter(2, c(4/1000,8/1000), type="pass")

	filter_EEG <- list( "bf_alpha" = bf_alpha, "bf_beta" = bf_beta, 
						"bf_gamma" = bf_gamma, "bf_delta" = bf_delta, "bf_theta" = bf_theta )

	return(filter_EEG)
}

# Construct a low-pass filter and an high-pass filter using a butterworth filter design
#  (cut-off = 1-20 Hz)
construct_filter_GSR = function(){

	bf_low <- butter(2, 1/20, type="low")
	bf_high <- butter(2, 1/20, type="high")

	filter_GSR <- list("bf_low" = bf_low, "bf_high" = bf_high )

	return(filter_GSR)

}

# Construct a band-pass filter using a butterworth filter design
#  (cut-off = 1-125 Hz)
construct_filter_EMG = function(){

	bf_pass <- butter(2, c(20/1000, 125/1000), type="pass")

	return (bf_pass)

}

# It applies a butterworth filter on EEG signal
apply_filter_EEG = function(filter_EEG, EEG_signal){

	alpha <- signal:::filter(filter_EEG$bf_alpha, EEG_signal)
	beta <- signal:::filter(filter_EEG$bf_beta, EEG_signal)
	gamma <- signal:::filter(filter_EEG$bf_gamma, EEG_signal)
	delta <- signal:::filter(filter_EEG$bf_delta, EEG_signal)
	theta <- signal:::filter(filter_EEG$bf_theta, EEG_signal)

	data_filtered <- list( "alpha" = alpha, "beta" = beta, "gamma" = gamma, 
							"delta" = delta, "theta" = theta )

	return(data_filtered) 
}

# It applies a butterworth filter on GSR signal
apply_filter_GSR = function(filter_GSR, GSR_signal){

	phasic <- signal:::filter(filter_GSR$bf_high, GSR_signal)
	tonic <- signal:::filter(filter_GSR$bf_low, GSR_signal)

	data_filtered <- list("phasic" = phasic, "tonic" = tonic) 

	return(data_filtered)

}

# It applies a butterworth filter on EMG signal
apply_filter_EMG = function(filter_EMG, EMG_signal){

	emg_filtered <- signal:::filter(filter_EMG, EMG_signal)

	return(emg_filtered)

}


# It subtracts the mean baseline for each value of attribute
subtract_baseline = function (attribute, baseline){

	num_values <- length(attribute)
	for(j in 0: num_values)
	    attribute[j] <- attribute[j] - baseline
		
		return(attribute) 
}
