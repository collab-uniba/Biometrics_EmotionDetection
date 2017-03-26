
find_row_index = function( time_pos, i, temp){

n<- length(time_pos)


while(time_pos[i] != temp)
	i <- i+1

return(i)
}

find_useful_lines = function(start_time, data_file, physiological_measure){


timestamp <- data_file[,c(1)];

if(physiological_measure == "EEG")
	time <- strptime(timestamp, format = "%H:%M:%S" )
else 
	time <- strptime(timestamp, format = "%Y-%m-%d %H:%M:%S" )

	time_pos <- as.POSIXct(time)

i <-1

temp <- start_time + seconds(2)
start_baseline1 <- find_row_index(time_pos,i,temp) + 1
#start_baseline1 <- find_row_index(time_pos,i,temp) + 1
#print(start_baseline1)

temp <- start_time + seconds(32)
end_baseline1 <- find_row_index(time_pos,start_baseline1 - 1,temp)
#print(end_baseline1)

temp <- start_time + minutes(1) + seconds(5)
start_video1 <- find_row_index(time_pos,end_baseline1,temp) + 1 
#print(start_video1)

temp <- start_time + minutes(1) + seconds(35)
end_video1 <- find_row_index(time_pos,start_video1 - 1,temp)	
#print(end_video1)

temp <- start_time + minutes(2) + seconds(18)
start_video2 <- find_row_index(time_pos,end_video1,temp) +1 
#print(start_video2)

temp <- start_time + minutes(2) + seconds(48)
end_video2 <- find_row_index(time_pos,start_video2 - 1,temp) 
#print(end_video2)

temp <- start_time + minutes(3)
start_baseline2 <- find_row_index(time_pos,end_video1,temp) +1 
#print(start_baseline2)

temp <- start_time + minutes(3) + seconds(30)
end_baseline2 <- find_row_index(time_pos,start_baseline2 -1, temp)  
#print(end_baseline2)

temp <- start_time + minutes(4) + seconds(3)
start_video3 <- find_row_index(time_pos,end_baseline2, temp)   +1 
#print(start_video3)
 
temp <- start_time + minutes(4) + seconds(33)
end_video3 <- find_row_index(time_pos,start_video3 -1, temp) 
#print(end_video3)

temp <- start_time + minutes(5) + seconds(16)
start_video4 <- find_row_index(time_pos,end_video3 ,temp) +1 
#print(start_video4)

temp <- start_time + minutes(5) + seconds(46)
end_video4 <- find_row_index(time_pos, start_video4 - 1, temp) 
#print(end_video4)

temp <- start_time + minutes(5) + seconds(58)
start_baseline3 <- find_row_index(time_pos,end_video4 ,temp) +1 
#print(start_baseline3)

temp <- start_time + minutes(6) + seconds(28)
end_baseline3 <- find_row_index(time_pos,start_baseline3 -1, temp)  
#print(end_baseline3)


temp <- start_time + minutes(7) + seconds(1)
start_video5 <- find_row_index(time_pos,end_baseline3, temp) + 	1
#print(start_video5)

temp <- start_time + minutes(7) + seconds(31)
end_video5 <- find_row_index(time_pos, start_video5 - 1, temp)
#print(end_video5)

temp <- start_time + minutes(8) + seconds(14)
start_video6 <- find_row_index(time_pos, end_video5, temp) +1 
#print(start_video6)

temp <- start_time + minutes(8) + seconds(44)
end_video6 <- find_row_index(time_pos, start_video6 - 1, temp)
#print(end_video6)  

temp <- start_time + minutes(8) + seconds(56)
start_baseline4 <- find_row_index(time_pos, end_video6, temp) + 1
#print(start_baseline4)  

temp <- start_time + minutes(9) + seconds(26)
end_baseline4 <- find_row_index(time_pos, start_baseline4 - 1, temp)
#print(end_baseline4)

temp <- start_time + minutes(9) + seconds(59) 
start_video7 <- find_row_index(time_pos, end_baseline4, temp) + 1
#print(start_video7)

temp <- start_time + minutes(10) + seconds(29)
end_video7 <- find_row_index(time_pos, start_video7 - 1, temp) 
#print(end_video7)

temp <- start_time + minutes(11) + seconds(12)
start_video8 <- find_row_index(time_pos, end_video7, temp) +1
#print(start_video8) 

temp <- start_time + minutes(11) + seconds(42)	
end_video8 <- find_row_index(time_pos, start_video8 - 1, temp) +1
#print(end_video8)


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
construct_filter_EEG = function(){

bf_alpha <- butter(2, c(8/1000,12/1000), type="pass")
bf_beta <- butter(2, c(12/1000,30/1000), type="pass")
bf_gamma <- butter(2, c(30/1000,80/1000), type="pass")
bf_delta <- butter(2, c(0,4/1000), type="pass")
bf_theta <- butter(2, c(4/1000,8/1000), type="pass")

filter_EEG <- list( "bf_alpha" = bf_alpha, "bf_beta" = bf_beta, "bf_gamma" = bf_gamma, "bf_delta" = bf_delta, "bf_theta" = bf_theta )

return(filter_EEG)
}

construct_filter_GSR = function(){

bf_low <- butter(2, 1/20, type="low")
bf_high <- butter(2, 1/20, type="high")

filter_GSR <- list("bf_low" = bf_low, "bf_high" = bf_high )

}

#filter the frequency
apply_filter_EEG = function(filter_EEG, EEG_signal){

alpha <- signal:::filter(filter_EEG$bf_alpha, EEG_signal)
beta <- signal:::filter(filter_EEG$bf_beta, EEG_signal)
gamma <- signal:::filter(filter_EEG$bf_gamma, EEG_signal)
delta <- signal:::filter(filter_EEG$bf_delta, EEG_signal)
theta <- signal:::filter(filter_EEG$bf_theta, EEG_signal)

data_filtered <- list( "alpha" = alpha, "beta" = beta, "gamma" = gamma, "delta" = delta, "theta" = theta )

return(data_filtered) 
}

apply_filter_GSR = function(filter_GSR, GSR_signal){

phasic <- signal:::filter(filter_GSR$bf_high, GSR_signal)
tonic <- signal:::filter(filter_GSR$bf_low, GSR_signal)

data_filtered <- list("phasic" = phasic, "tonic" = tonic) 

}


#subtract the mean baseline for each value of attribute
subtract_baseline = function (attribute, baseline){

num_values <- length(attribute)
for(j in 0: num_values)
    attribute[j] <- attribute[j] - baseline
	
	return(attribute) 
}

calculate_features = function (attribute) {

mean <- mean(attribute, trim = 0) 
min <- min(attribute, trim = 0)
max <- max(attribute, trim = 0)
var <- var(attribute)
sd <- sd(attribute)
#mean_abs <- mean( abs(attribute), trim = 0) 
#max_abs <- max(abs(attribute), trim = 0) 
#median <- median(attribute)

features_extracted <- list( "mean" = mean, "min" = min, "max" = max, "var" = var, "sd" = sd)

return(features_extracted)
}

calculate_derivative_features  = function(time_v, data_filtered_video){

time_video <- as.numeric(time_v)
video_filtered_deriv <- predict(smooth.spline(time_video, data_filtered_video), time_video, 1)
mean_deriv <- mean(video_filtered_deriv$y)

number_all_samples <- length(video_filtered_deriv$y)
samples_neg_deriv <- video_filtered_deriv[video_filtered_deriv$y < 0]
number_samples_neg_deriv <-  length(samples_neg_deriv)

prop_neg_samples <- number_all_samples/number_samples_neg_deriv

data_video_df <- data.frame(time_video, data_filtered_video)
data_only_negative <- data_video_df[ !(data_video_df$data_filtered_video > 0),]
#print(data_only_negative)
video_filtered_deriv_neg <- predict(smooth.spline(data_only_negative$time_video, data_only_negative$data_filtered_video), data_only_negative$time_video, 1)
mean_deriv_only_neg <- mean(video_filtered_deriv_neg$y)


features_extracted_deriv <- list("mean_deriv" = mean_deriv, "mean_deriv_only_neg" = mean_deriv_only_neg, "prop_neg_samples" = prop_neg_samples )
return(features_extracted_deriv)

}


calculate_peaks_features = function (edaSignal_video.cwt){

W <- edaSignal_video.cwt
z <- wavCWTTree(W)
p <- wavCWTPeaks(z,snr.min=0.1)
#print(p[,peaks])
coefficients <- p$y #frequency 

min_peaks_ampl <- min(coefficients)
max_peaks_ampl <- max(coefficients)
mean_peaks_ampl <- mean(coefficients)

num_phasic_peaks_divided_min <- length(coefficients)/min_peaks_ampl
sum_phasic_peaks_divided_min <- sum(coefficients)/min_peaks_ampl

features_extracted_peaks <- list("mean_peaks_ampl" = mean_peaks_ampl, "min_peaks_ampl" = min_peaks_ampl, "max_peaks_ampl" = max_peaks_ampl, 
"num_phasic_peaks_divided_min" = num_phasic_peaks_divided_min, "sum_phasic_peaks_divided_min" = sum_phasic_peaks_divided_min )

}



calculate_ratio_frequencies = function(frequencies){  

ratio_alpha_beta <- frequencies$alpha/frequencies$beta
ratio_alpha_gamma <- frequencies$alpha/frequencies$gamma
ratio_alpha_delta <- frequencies$alpha/frequencies$delta
ratio_alpha_theta <- frequencies$alpha/frequencies$theta

ratio_beta_alpha <- frequencies$beta/frequencies$alpha
ratio_beta_gamma <- frequencies$beta/frequencies$gamma
ratio_beta_delta <- frequencies$beta/frequencies$delta
ratio_beta_theta <- frequencies$beta/frequencies$theta

ratio_gamma_alpha <- frequencies$gamma/frequencies$alpha
ratio_gamma_beta <- frequencies$gamma/frequencies$beta
ratio_gamma_delta <- frequencies$gamma/frequencies$delta
ratio_gamma_theta <- frequencies$gamma/frequencies$theta

ratio_delta_alpha <- frequencies$delta/frequencies$alpha
ratio_delta_beta <- frequencies$delta/frequencies$beta
ratio_delta_gamma <- frequencies$delta/frequencies$gamma
ratio_delta_theta <- frequencies$delta/frequencies$theta

ratio_theta_alpha <- frequencies$theta/frequencies$alpha
ratio_theta_beta <- frequencies$theta/frequencies$beta
ratio_theta_gamma <- frequencies$theta/frequencies$gamma
ratio_theta_delta <- frequencies$theta/frequencies$delta


ratio_theta_alpha_plus_beta <- frequencies$theta /(frequencies$alpha + frequencies$beta)
ratio_beta_alpha_plus_theta <-frequencies$beta /(frequencies$alpha + frequencies$theta)



ratio_frequencies <- list("ratio_alpha_beta" = ratio_alpha_beta, "ratio_alpha_gamma"= ratio_alpha_gamma, "ratio_alpha_delta" = ratio_alpha_delta, "ratio_alpha_theta"  = ratio_alpha_theta,
"ratio_beta_alpha" = ratio_beta_alpha, "ratio_beta_gamma"= ratio_beta_gamma, "ratio_beta_delta" = ratio_beta_delta, "ratio_beta_theta"  = ratio_beta_theta, 
"ratio_gamma_alpha" = ratio_gamma_alpha, "ratio_gamma_beta"= ratio_gamma_beta, "ratio_gamma_delta" = ratio_gamma_delta, "ratio_gamma_theta"  = ratio_gamma_theta, 
"ratio_delta_alpha" = ratio_delta_alpha, "ratio_delta_beta"= ratio_delta_beta, "ratio_delta_gamma" = ratio_delta_gamma, "ratio_delta_theta"  = ratio_delta_theta, 
"ratio_theta_alpha" = ratio_theta_alpha, "ratio_theta_beta"= ratio_theta_beta, "ratio_theta_gamma" = ratio_theta_gamma, "ratio_theta_delta"  = ratio_theta_delta, 
"ratio_theta_alpha_plus_beta" = ratio_theta_alpha_plus_beta, "ratio_beta_alpha_plus_theta" = ratio_beta_alpha_plus_theta) 

 return(ratio_frequencies)
 
}


calculate_diff_ratio_frequencies = function(ratio_frequencies_video, ratio_frequencies_baseline){  

diff_alpha_beta <- ratio_frequencies_video$ratio_alpha_beta - ratio_frequencies_baseline$ratio_alpha_beta
diff_alpha_gamma <- ratio_frequencies_video$ratio_alpha_gamma - ratio_frequencies_baseline$ratio_alpha_gamma
diff_alpha_delta <- ratio_frequencies_video$ratio_alpha_delta - ratio_frequencies_baseline$ratio_alpha_delta
diff_alpha_theta <- ratio_frequencies_video$ratio_alpha_theta - ratio_frequencies_baseline$ratio_alpha_theta

diff_beta_alpha <- ratio_frequencies_video$ratio_beta_alpha - ratio_frequencies_baseline$ratio_beta_alpha
diff_beta_gamma <- ratio_frequencies_video$ratio_beta_gamma - ratio_frequencies_baseline$ratio_beta_gamma
diff_beta_delta <- ratio_frequencies_video$ratio_beta_delta - ratio_frequencies_baseline$ratio_beta_delta
diff_beta_theta <- ratio_frequencies_video$ratio_beta_theta - ratio_frequencies_baseline$ratio_beta_theta

diff_gamma_alpha <- ratio_frequencies_video$ratio_gamma_alpha - ratio_frequencies_baseline$ratio_gamma_alpha
diff_gamma_beta <- ratio_frequencies_video$ratio_gamma_beta - ratio_frequencies_baseline$ratio_gamma_beta
diff_gamma_delta <- ratio_frequencies_video$ratio_gamma_delta - ratio_frequencies_baseline$ratio_gamma_delta
diff_gamma_theta <- ratio_frequencies_video$ratio_gamma_theta - ratio_frequencies_baseline$ratio_gamma_theta


diff_delta_alpha <- ratio_frequencies_video$ratio_delta_alpha - ratio_frequencies_baseline$ratio_delta_alpha
diff_delta_beta <- ratio_frequencies_video$ratio_delta_beta - ratio_frequencies_baseline$ratio_delta_beta
diff_delta_gamma <- ratio_frequencies_video$ratio_delta_gamma - ratio_frequencies_baseline$ratio_delta_gamma
diff_delta_theta <- ratio_frequencies_video$ratio_delta_theta - ratio_frequencies_baseline$ratio_delta_theta


diff_theta_alpha <- ratio_frequencies_video$ratio_theta_alpha - ratio_frequencies_baseline$ratio_theta_alpha
diff_theta_beta <- ratio_frequencies_video$ratio_theta_beta - ratio_frequencies_baseline$ratio_theta_beta
diff_theta_gamma <- ratio_frequencies_video$ratio_theta_gamma - ratio_frequencies_baseline$ratio_theta_gamma
diff_theta_delta <- ratio_frequencies_video$ratio_theta_delta - ratio_frequencies_baseline$ratio_theta_delta



diff_theta_alpha_plus_beta <- ratio_frequencies_video$ratio_theta_alpha_plus_beta - ratio_frequencies_baseline$ratio_theta_alpha_plus_beta
diff_beta_alpha_plus_theta <- ratio_frequencies_video$ratio_beta_alpha_plus_theta - ratio_frequencies_baseline$ratio_beta_alpha_plus_theta

diff_frequencies <- list("diff_alpha_beta" = diff_alpha_beta, "diff_alpha_gamma" = diff_alpha_gamma, "diff_alpha_delta"= diff_alpha_delta, "diff_alpha_theta"= diff_alpha_theta, 
"diff_beta_alpha"= diff_beta_alpha, "diff_beta_gamma"= diff_beta_gamma, "diff_beta_delta"= diff_beta_delta, "diff_beta_theta"= diff_beta_theta, 
"diff_gamma_alpha"= diff_gamma_alpha, "diff_gamma_beta"= diff_gamma_beta, "diff_gamma_delta"= diff_gamma_delta, "diff_gamma_theta"= diff_gamma_theta,  "diff_delta_alpha"= diff_delta_alpha, 
"diff_delta_beta"= diff_delta_beta, "diff_delta_gamma"= diff_delta_gamma, "diff_delta_theta"= diff_delta_theta, "diff_theta_alpha"= diff_theta_alpha, "diff_theta_beta"= diff_theta_beta, 
"diff_theta_gamma"= diff_theta_gamma, "diff_theta_delta "= diff_theta_delta, "diff_theta_alpha_plus_beta" = diff_theta_alpha_plus_beta, "diff_beta_alpha_plus_theta" = diff_beta_alpha_plus_theta )



return(diff_frequencies)

}


calculate_HR_features = function(data_video_HR, baseline_HR){

if('-1' %in% data_video_HR ){
	mean_HR = '?'
	sd_HR = '?'
	}
else{

	mean_HR = mean(data_video_HR)
	sd_HR = sd(data_video_HR)
}

#Subract the mean HR of baseline to video

if( '-1' %in% data_video_HR | '-1' %in% baseline_HR){
	mean_HR_corrected = '?'
	sd_HR_corrected = '?' 
}
else{
	mean_baseline <-mean( baseline_HR, trim = 0) 
	video_HR_corrected <- subtract_baseline(data_video_HR, mean_baseline)
	mean_HR_corrected = mean(video_HR_corrected)
	sd_HR_corrected = sd(video_HR_corrected)
}

	HR_features <- list("mean_HR" = mean_HR, "sd_HR" = sd_HR, "mean_HR_corrected" = mean_HR_corrected, "sd_HR_corrected" = sd_HR_corrected )
 
 
 return(HR_features)
}

add_to_GSR_dataset = function( id, phasic_features, phasic_features_corrected, phasic_derivative_features, phasic_derivative_features_corrected, phasic_peaks_features, phasic_peaks_features_corrected,  HR_features, arousal, valence ){


feature_video <- c(id, phasic_features$mean, phasic_features$min, phasic_features$max, phasic_features$var ,phasic_features$sd,
phasic_features_corrected$mean, phasic_features_corrected$min, phasic_features_corrected$max, phasic_features_corrected$var, phasic_features_corrected$sd,
phasic_derivative_features$mean_deriv, phasic_derivative_features$mean_deriv_only_neg, phasic_derivative_features$prop_neg_samples, 
phasic_derivative_features_corrected$mean_deriv, phasic_derivative_features_corrected$mean_deriv_only_neg, phasic_derivative_features_corrected$prop_neg_samples, 
phasic_peaks_features$mean_peaks_ampl, phasic_peaks_features$min_peaks_ampl, phasic_peaks_features$max_peaks_ampl, phasic_peaks_features$num_phasic_peaks_divided_min, 
phasic_peaks_features$sum_phasic_peaks_divided_min,
phasic_peaks_features_corrected$mean_peaks_ampl, phasic_peaks_features_corrected$min_peaks_ampl, phasic_peaks_features_corrected$max_peaks_ampl, phasic_peaks_features_corrected$num_phasic_peaks_divided_min, 
phasic_peaks_features_corrected$sum_phasic_peaks_divided_min,  HR_features$mean_HR,  HR_features$sd_HR, HR_features$mean_HR_corrected,  HR_features$sd_HR_corrected, arousal, valence)
features_video <- as.matrix(t(feature_video))

GSR_dataset <- "D:/features_extraction/dataset/GSR_dataset.arff"
write.table(features_video, file = GSR_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)

} 


add_to_EEG_dataset = function (id, alpha_features, beta_features, gamma_features, delta_features, theta_features, attention, meditation, alpha_features_corrected, 
beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected, diff_frquencies ,arousal, valence
){

feature_video <- c(id, alpha_features$mean, alpha_features$min, alpha_features$max, alpha_features$var ,alpha_features$sd, 
beta_features$mean, beta_features$min, beta_features$max, beta_features$var, beta_features$sd, 
gamma_features$mean, gamma_features$min, gamma_features$max, gamma_features$var,gamma_features$sd,
delta_features$mean, delta_features$min, delta_features$max, delta_features$var, delta_features$sd,
theta_features$mean, theta_features$min, theta_features$max, theta_features$var, theta_features$sd,  
attention$mean, attention$min, attention$max, attention$var,attention$sd, 
meditation$mean, meditation$min, meditation$max, meditation$var ,meditation$sd,
alpha_features_corrected$mean, alpha_features_corrected$min, alpha_features_corrected$max, alpha_features_corrected$var,alpha_features_corrected$sd, 
beta_features_corrected$mean, beta_features_corrected$min, beta_features_corrected$max, beta_features_corrected$var,beta_features_corrected$sd, 
gamma_features_corrected$mean, gamma_features_corrected$min, gamma_features_corrected$max, gamma_features_corrected$var,gamma_features_corrected$sd,
delta_features_corrected$mean, delta_features_corrected$min, delta_features_corrected$max, delta_features_corrected$var,delta_features_corrected$sd,
theta_features_corrected$mean, theta_features_corrected$min, theta_features_corrected$max, theta_features_corrected$var,theta_features_corrected$sd,  
attention_corrected$mean, attention_corrected$min, attention_corrected$max, attention_corrected$var,attention_corrected$sd, 
meditation_corrected$mean, meditation_corrected$min, meditation_corrected$max, meditation_corrected$var,meditation_corrected$sd,
mean(diff_frequencies$diff_alpha_beta), mean(diff_frequencies$diff_alpha_gamma), mean(diff_frequencies$diff_alpha_delta), mean(diff_frequencies$diff_alpha_theta), 
mean(diff_frequencies$diff_beta_alpha), mean(diff_frequencies$diff_beta_gamma), mean(diff_frequencies$diff_beta_delta), mean(diff_frequencies$diff_beta_theta),
mean(diff_frequencies$diff_gamma_alpha), mean(diff_frequencies$diff_gamma_beta), mean(diff_frequencies$diff_gamma_delta), mean(diff_frequencies$diff_gamma_theta),
mean(diff_frequencies$diff_delta_alpha), mean(diff_frequencies$diff_delta_beta), mean(diff_frequencies$diff_delta_gamma), mean(diff_frequencies$diff_delta_theta),
mean(diff_frequencies$diff_theta_alpha), mean(diff_frequencies$diff_theta_beta), mean(diff_frequencies$diff_theta_gamma), mean(diff_frequencies$diff_theta_delta),
mean(diff_frequencies$diff_theta_alpha_plus_beta), mean(diff_frequencies$diff_beta_alpha_plus_theta), arousal, valence)
features_video <- as.matrix(t(feature_video))

#append the features on a file
EEG_dataset <- "D:/features_extraction/dataset/EEG_dataset.arff"

write.table(features_video, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)


}
