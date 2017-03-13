
find_row_index = function( time_pos, i, temp){

while(time_pos[i] != temp){
	i <- i+1
	}
return(i)
}

find_useful_lines = function(start_time, data_file){


timestamp <- data_file[,c(1)];

time <- strptime(timestamp, format = "%H:%M:%S" )
time_pos <- as.POSIXct(time)


i <-1
temp <- start_time + seconds(2)
start_baseline1 <- find_row_index(time_pos,i,temp) + 1
print(start_baseline1)

temp <- start_time + seconds(32)
end_baseline1 <- find_row_index(time_pos,start_baseline1 - 1,temp)
print(end_baseline1)

temp <- start_time + minutes(1) + seconds(5)
start_video1 <- find_row_index(time_pos,end_baseline1,temp) + 1 
print(start_video1)

temp <- start_time + minutes(1) + seconds(35)
end_video1 <- find_row_index(time_pos,start_video1 - 1,temp)	
print(end_video1)

temp <- start_time + minutes(2) + seconds(18)
start_video2 <- find_row_index(time_pos,end_video1,temp) +1 
print(start_video2)

temp <- start_time + minutes(2) + seconds(48)
end_video2 <- find_row_index(time_pos,start_video2 - 1,temp) 
print(end_video2)

temp <- start_time + minutes(3)
start_baseline2 <- find_row_index(time_pos,end_video1,temp) +1 
print(start_baseline2)

temp <- start_time + minutes(3) + seconds(30)
end_baseline2 <- find_row_index(time_pos,start_baseline2 -1, temp)  
print(end_baseline2)

temp <- start_time + minutes(4) + seconds(3)
start_video3 <- find_row_index(time_pos,end_baseline2, temp)   +1 
print(start_video3)
 
temp <- start_time + minutes(4) + seconds(33)
end_video3 <- find_row_index(time_pos,start_video3 -1, temp) 
print(end_video3)

temp <- start_time + minutes(5) + seconds(16)
start_video4 <- find_row_index(time_pos,end_video3 ,temp) +1 
print(start_video4)

temp <- start_time + minutes(5) + seconds(46)
end_video4 <- find_row_index(time_pos, start_video4 - 1, temp) 
print(end_video4)

temp <- start_time + minutes(5) + seconds(58)
start_baseline3 <- find_row_index(time_pos,end_video4 ,temp) +1 
print(start_baseline3)

temp <- start_time + minutes(6) + seconds(28)
end_baseline3 <- find_row_index(time_pos,start_baseline3 -1, temp)  
print(end_baseline3)


temp <- start_time + minutes(7) + seconds(1)
start_video5 <- find_row_index(time_pos,end_baseline3, temp) + 	1
print(start_video5)

temp <- start_time + minutes(7) + seconds(31)
end_video5 <- find_row_index(time_pos, start_video5 - 1, temp)
print(end_video5)

temp <- start_time + minutes(8) + seconds(14)
start_video6 <- find_row_index(time_pos, end_video5, temp) +1 
print(start_video6)

temp <- start_time + minutes(8) + seconds(44)
end_video6 <- find_row_index(time_pos, start_video6 - 1, temp)
print(end_video6)  

temp <- start_time + minutes(8) + seconds(56)
start_baseline4 <- find_row_index(time_pos, end_video6, temp) + 1
print(start_baseline4)  

temp <- start_time + minutes(9) + seconds(26)
end_baseline4 <- find_row_index(time_pos, start_baseline4 - 1, temp)
print(end_baseline4)

temp <- start_time + minutes(9) + seconds(59) 
start_video7 <- find_row_index(time_pos, end_baseline4, temp) + 1
print(start_video7)

temp <- start_time + minutes(10) + seconds(29)
end_video7 <- find_row_index(time_pos, start_video7 - 1, temp) 
print(end_video7)

temp <- start_time + minutes(11) + seconds(12)
start_video8 <- find_row_index(time_pos, end_video7, temp) +1
print(start_video8) 

temp <- start_time + minutes(11) + seconds(42)	
end_video8 <- find_row_index(time_pos, start_video8 - 1, temp) +1
print(end_video8)


useful_seconds <- list( "start_baseline1" = start_baseline1, "end_baseline1" = end_baseline1, "start_video1" = start_video1, "end_video1" = end_video1, 
						"start_video2" = start_video2, "end_video2" = end_video2, "start_baseline2" = start_baseline2, "end_baseline2" = end_baseline2,
					"start_video3" = start_video3, "end_video3" = end_video3, "start_video4" = start_video4, "end_video4" = end_video4, 
						"start_baseline3" = start_baseline3, "end_baseline3" = end_baseline3, "start_video5" = start_video5, "end_video5" = end_video5, 
					"start_video6" = start_video6, "end_video6" = end_video6, "start_baseline4" = start_baseline4, "end_baseline4" = end_baseline4, 
						"start_video7" = start_video7, "end_video7" = end_video7, "start_video8" = start_video8, "end_video8" = end_video8
						 )

return(useful_seconds) 

}

# Construct a band-pass filter using a butterworth filter design.
# (alpha: 8-12hz, beta 12-30 hz, gamma 30-80hz, delta 0-4 hz, theta 4-8 hz)
construct_filter = function(){

bf_alpha <- butter(2, c(8/1000,12/1000), type="pass")
bf_beta <- butter(2, c(12/1000,30/1000), type="pass")
bf_gamma <- butter(2, c(30/1000,80/1000), type="pass")
bf_delta <- butter(2, c(0,4/1000), type="pass")
bf_theta <- butter(2, c(4/1000,8/1000), type="pass")

filter_EEG <- list( "bf_alpha" = bf_alpha, "bf_beta" = bf_beta, "bf_gamma" = bf_gamma, "bf_delta" = bf_delta, "bf_theta" = bf_theta )

return(filter_EEG)
}

#filter the frequency
apply_filter = function(filter_EEG,data_to_filter){

alpha <- signal:::filter(filter_EEG$bf_alpha, data_to_filter)
beta <- signal:::filter(filter_EEG$bf_beta, data_to_filter)
gamma <- signal:::filter(filter_EEG$bf_gamma, data_to_filter)
delta <- signal:::filter(filter_EEG$bf_delta, data_to_filter)
theta <- signal:::filter(filter_EEG$bf_theta, data_to_filter)

data_filtered <- list( "alpha" = alpha, "beta" = beta, "gamma" = gamma, "delta" = delta, "theta" = theta )

return(data_filtered) 
}

#subtract the mean baseline for each value of attribute
subtract_baseline = function ( attribute, baseline){

for(j in 1:length(attribute))
    attribute[j] <- attribute[j] - baseline
	
	return(attribute) 
}

calculate_features = function (attribute) {

mean <- mean(attribute, trim = 0) 
min <- min(attribute, trim = 0)
max <- max(attribute, trim = 0)
sd <- sd(attribute)

features_extracted <- list( "mean" = mean, "min" = min, "max" = max, "sd" = sd)

return(features_extracted)
}

