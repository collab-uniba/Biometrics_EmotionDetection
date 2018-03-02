# Author: Daniela Girardi, Garofalo Antonio
# This script provides a list of method to calculate signal features

# 
# 
# 
# ------------------------------------- FEATURES EXTRACTION -------------------------------------
# 
# 
# 

# It calculates the statistical base features
# (mean, min, max, variance, standard deviation)
calculate_features = function (attribute) {

	mean <- mean(attribute, trim = 0) 
	min <- min(attribute, trim = 0)
	max <- max(attribute, trim = 0)
	var <- var(attribute)
	sd <- sd(attribute)
	# mean_abs <- mean( abs(attribute), trim = 0) 
	# max_abs <- max(abs(attribute), trim = 0) 
	# median <- median(attribute)

	features_extracted <- list( "mean" = mean, "min" = min, "max" = max, "var" = var, "sd" = sd)

	return(features_extracted)
}

# It calculates the featrures based on first derivative
# (proportion of negative samples in derivative vs all samples)
# (average of derivative for negative values only)
# (only negative values)
# (derivative for only negative values)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/predict.smooth.spline.html
calculate_derivative_features  = function(time_v, data_filtered_video){

	time_video <- as.numeric(time_v)
	video_filtered_deriv <- predict(smooth.spline(time_video, data_filtered_video), time_video, 1)
	mean_deriv <- mean(video_filtered_deriv$y)
	number_all_samples <- length(video_filtered_deriv$y)
	samples_neg_deriv <- video_filtered_deriv[video_filtered_deriv$y < 0]
	number_samples_neg_deriv <-  length(samples_neg_deriv)

	# proportion of negative samples in derivative vs all samples
	prop_neg_samples <- number_samples_neg_deriv/number_all_samples 

	# average of derivative for negative values only
	data_video_df <- data.frame(time_video, data_filtered_video)

	# get only negative values
	data_only_negative <- data_video_df[ !(data_video_df$data_filtered_video > 0),]
	
	# compute derivative for only negative values
	video_filtered_deriv_neg <- predict(smooth.spline(data_only_negative$time_video, data_only_negative$data_filtered_video), data_only_negative$time_video, 1)
	mean_deriv_only_neg <- mean(video_filtered_deriv_neg$y)


	features_extracted_deriv <- list("mean_deriv" = mean_deriv, "mean_deriv_only_neg" = mean_deriv_only_neg, "prop_neg_samples" = prop_neg_samples )
	return(features_extracted_deriv)

}

# It applies Wavelet Transform to the video signal and it calculates the features based on amplitude peaks values
# (mean ampl, min ampl, max ampl, number_phasic_peaks / min, sum_phasic_peaks / min) 
calculate_peaks_features = function (Signal_video.cwt){

	W <- Signal_video.cwt
	z <- wavCWTTree(W)
	p <- wavCWTPeaks(z,snr.min=0.1)
	# print(p[,peaks])
	coefficients <- p$y # frequency 

	min_peaks_ampl <- min(coefficients)
	max_peaks_ampl <- max(coefficients)
	mean_peaks_ampl <- mean(coefficients)

	num_phasic_peaks_divided_min <- length(coefficients)/min_peaks_ampl
	sum_phasic_peaks_divided_min <- sum(coefficients)/min_peaks_ampl

	features_extracted_peaks <- list("mean_peaks_ampl" = mean_peaks_ampl, "min_peaks_ampl" = min_peaks_ampl, "max_peaks_ampl" = max_peaks_ampl, 
	"num_phasic_peaks_divided_min" = num_phasic_peaks_divided_min, "sum_phasic_peaks_divided_min" = sum_phasic_peaks_divided_min )

	return(features_extracted_peaks)

}

# It calculates the ratio between an EEG wave and others EEG wave 
# (For example alpha/beta, alpha/gamma, ecc.)
# (theta/(alpha+beta), beta/(alpha+theta))
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

# It calculates the ratio frenquencies subtracting the baseline values for each EEG wave type
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

# It returns a list of additional EEG features
# (latency, latency_interval_from_start, amplitude, absolute_amplitude, absolute_latency_amplitude_ratio,
# peak_to_peak, peak_to_peak_time_window, peak_to_peak_slope,
# zero_crossings, zero_crossings_peak_to_peak, zero_crossings_density,
# positive_area, negative_area, absolute_negative_area, total_area, absolute_total_area, total_absolute_area,
# average_absolute_signal_slope, slope_sign_alternation)
calculate_new_EEG_features = function(attribute, timestamp){ # NOME DA CAMBIARE
	col = 1
	# timestamp <- dataFile[c(useful_seconds$start_video1:useful_seconds$end_video1) , c(col)]

	# Latency (LAT)
	latTimeValue <- timestamp[which.max(attribute)]
	latIndex <- which.max(attribute)
	latTimestamp <- strptime(latTimeValue[c(col)], format = "%H:%M:%S")
	LAT <- as.numeric(latTimestamp)

	# Latency Interval from Start (LATFS)
	startVideo_timestamp <- strptime(timestamp[1], format = "%H:%M:%S")
	startVideo_millisec <- as.numeric(startVideo_timestamp)
	LATFS <- LAT - startVideo_millisec

	# Amplitude (AMP)
	AMP <- max(attribute)

	# Latency/Amplitude Ratio (LAR)
	LAR <- LAT / AMP

	# Absolute Amplitude (AAMP)
	AAMP <- abs(AMP)

	# Absolute Latency/Amplitude Ratio (ALAR)
	ALAR <- abs(LAT / AMP)

	col = 1
	minTimeValue <- timestamp[which.min(attribute)]
	minIndex <- which.min(attribute)
	minTimestamp <- strptime(minTimeValue[c(col)], format = "%H:%M:%S")
	t_min <- as.numeric(minTimestamp)
	S_min <- min(attribute)

	# Peak-to-Peak (PP)
	PP <- AMP - S_min

	# Peak-to-Peak Time Window (PPT) # DA FIXARE
	# Tuning can't compute this feature because it almost always results zero,
	# so it can't be used to calculate composite features (PPS, ZCD) 
	t1 <- t_min
	t2 <- LAT
	# PPT <- t2 - t1

	# Peak-to-Peak Slope # DA FIXARE il ppt risulta 0 in quanto la differenza tra t2 e t1 in millisecondi non può essere registrata: IL SENSORE che trasmette dati,
	# Tuning can't compute this feature because it almost always results Inf
	# The problem is the sensor which sends data: they have accuracy per second and not accuracy per millisecond, so, few milliseconds differences are sent with SAME timestamp
	# PPS <- PP / PPT

	# Zero Crossings (ZC)
	ZC <- sum(attribute[1:(length(attribute)-1)] * attribute[2:length(attribute)] <= 0) - sum(attribute == 0)

	# Zero Crossings Peak-to-Peak (ZCPP)
	# It uses row index of t_min and LAT instead timestamp of PP
	# ZC <- sum(attribute[(t1+1):t2] * attribute[t1:(t2-1)] <= 0) - sum(attribute == 0)
	if(minIndex > latIndex){
		ZCPP <- sum(attribute[latIndex:(minIndex-1)] * attribute[(latIndex + 1):minIndex] <= 0) - sum(attribute == 0)
	} else {
		ZCPP <- 0
	}
	

	# Zero Crossings Density (ZCD)
	# Tuning can't compute this feature because it almost always results Inf
	# ZCD <- ZC / PPT

	# Positive Area (PAR)
	PAR <- 0.5* sum(attribute + abs(attribute))

	# Negative Area (NAR)
	NAR <- 0.5* sum(attribute - abs(attribute))

	# Absolute Negative Area (ANAR)
	ANAR <- abs(NAR)

	# Total Area (TAR)
	TAR <- PAR + NAR

	# Absolute Total Area (ATAR)
	ATAR <- abs(TAR)

	# Total Absolute Area (TAAR)
	TAAR <- PAR + abs(NAR)

	# Average Absolute Signal Slope (AASS)
	signal_length <- length(attribute)
	AASS <- sum(abs(attribute[2:signal_length] - attribute[1:(signal_length-1)])) / signal_length

	# Slope Sign Alternation (SSA)
	signal_length <- length(attribute)
	SSA <- 0.5 * sum( abs(sign(attribute[1:(signal_length - 2)] - attribute[2:(signal_length - 1)]) 
		+ sign(attribute[3:signal_length] - attribute[2:(signal_length-1)])))

	# It contains all features (also PPT, PPS, ZCD)
	# features_extracted <- list( "latency" = LAT, "latency_interval_from_start" = LATFS, "amplitude" = AMP, "absolute_amplitude" = AAMP, "absolute_latency_amplitude_ratio" = ALAR,
	# 	"peak_to_peak" = PP, "peak_to_peak_time_window" = PPT, "peak_to_peak_slope" = PPS, 
	# 	"zero_crossings" = ZC, "zero_crossings_peak_to_peak" = ZCPP, "zero_crossings_density" = ZCD,
	# 	"positive_area" = PAR, "negative_area" = NAR, "absolute_negative_area" = ANAR, "total_area" = TAR, "absolute_total_area" = ATAR, "total_absolute_area" = TAAR,
	# 	"average_absolute_signal_slope" = AASS, "slope_sign_alternation" = SSA)

	features_extracted <- list( "latency" = LAT, "latency_interval_from_start" = LATFS, "amplitude" = AMP, "absolute_amplitude" = AAMP, "absolute_latency_amplitude_ratio" = ALAR,
		"peak_to_peak" = PP, "zero_crossings" = ZC, "zero_crossings_peak_to_peak" = ZCPP,
		"positive_area" = PAR, "negative_area" = NAR, "absolute_negative_area" = ANAR, "total_area" = TAR, "absolute_total_area" = ATAR, "total_absolute_area" = TAAR,
		"average_absolute_signal_slope" = AASS, "slope_sign_alternation" = SSA)

	return(features_extracted)
}


# It calculates the features for HR
# (mean, standard_deviation, mean_corrected, standard_deviation_corrected)
calculate_HR_features = function(data_video_HR, baseline_HR){

	if('-1' %in% data_video_HR ){
		mean_HR = '?'
		sd_HR = '?'
	} else{
		mean_HR = mean(data_video_HR)
		sd_HR = sd(data_video_HR)
	}

	# Subract the mean HR of baseline to video

	if( '-1' %in% data_video_HR | '-1' %in% baseline_HR){
		mean_HR_corrected = '?'
		sd_HR_corrected = '?' 
	} else{
		mean_baseline <-mean( baseline_HR, trim = 0) 
		video_HR_corrected <- subtract_baseline(data_video_HR, mean_baseline)
		mean_HR_corrected = mean(video_HR_corrected)
		sd_HR_corrected = sd(video_HR_corrected)
	}

	HR_features <- list("mean_HR" = mean_HR, "sd_HR" = sd_HR, "mean_HR_corrected" = mean_HR_corrected, "sd_HR_corrected" = sd_HR_corrected )
	 
	 
	 return(HR_features)

}

# It calculates the EMG features after applying RMSE and MovingAverage algorithm
# (mean_rmse, mean_movavg, mean_integration)
calculate_EMG_features = function(attribute){
	# http://support.minitab.com/en-us/minitab/17/topic-library/modeling-statistics/time-series/moving-average/what-value-should-i-use-for-moving-average-length/

	#  Quadratic Mean
	rmse <- envelope(attribute, 1, wsize = 30, method =  "RMS")
	mean_rmse <- mean(rmse$val, na.rm=TRUE)

	movav <- movingaverage(attribute, wsize = 30, units = "samples")
	mean_movavg <- mean(movav$val, na.rm= TRUE)

	# EMG Signal Integration
	integ <- integration(attribute)
	mean_integ <- mean(integ$val, trim = 0)

	EMG_features <- list("mean_rmse" = mean_rmse, "mean_movavg" = mean_movavg, "mean_integ" = mean_integ )

	return(EMG_features)

}

# It returns a list of additional EMG features
# (area_under_curve, simple_square_integral,
# mean_absolute_value, modified_mean_absolute_value_1, modified_mean_absolute_value_2,
# root_mean_square, waveform_length, 
# average_absolute_signal_slope, zero_crossings, slope_sign_alternation)
calculate_new_EMG_features = function(attribute){
	# Area Under Curve (AUC) (Integral)
	AUC <- sum(abs(attribute))

	# Simple Square Integral (SSI)
	SSI <- sum(abs(attribute)^2)

	# Mean Absolute Value (MAV)
	MAV <- mean(abs(attribute), trim = 0)

	# Modified Mean Absolute Value 1 (MMAV1)
	index <- 1
	signal_length <- length(attribute)
	timeCoeff <- 0
	mav_value <- 0 
	while(index <= signal_length){
		if (index >= (0.25 * signal_length) && index <= (0.75 * signal_length)) {
			timeCoeff <- 1
		} else {
			timeCoeff <- 0.5
		}

		mav_value <- mav_value +  (timeCoeff * abs(attribute[index]))
		index <- index + 1
		timeCoeff <- 0
	}

	MMAV1 <- mav_value / signal_length

	# Modified Mean Absolute Value 2 (MMAV2)
	index <- 1
	signal_length <- length(attribute)
	timeCoeff <- 0
	mav_value <- 0 
	while(index <= signal_length){
		if (index < (0.25 * signal_length)){
			timeCoeff <- ((4 * index) / signal_length)
		} else if (index >= (0.25 * signal_length) && index <= (0.75 * signal_length)) {
			timeCoeff <- 1
		} else {
			timeCoeff <- ((4 * (index - signal_length)) / signal_length)
		}

		mav_value <- mav_value +  (timeCoeff * abs(attribute[index]))
		index <- index + 1
		timeCoeff <- 0
	}

	MMAV2 <- mav_value / signal_length

	# Root Mean Square (RMS)
	RMS <- sqrt(mean(attribute^2))

	# Waveform Length (WL)
	signal_length <- length(attribute)
	WL <- sum(abs(attribute[2:signal_length] - attribute[1:(signal_length-1)]))

	# Average Absolute Signal Slope (AASS)
	signal_length <- length(attribute)
	AASS <- sum(abs(attribute[2:signal_length] - attribute[1:(signal_length-1)])) / signal_length

	# Zero Crossings (ZC)
	signal_length <- length(attribute)
	ZC <- sum(attribute[1:(length(attribute)-1)] * attribute[2:length(attribute)] <= 0) - sum(attribute == 0)

	# Slope Sign Change (SSA)
	signal_length <- length(attribute)
	SSA <- 0.5 * sum( abs(sign(attribute[1:(signal_length - 2)] - attribute[2:(signal_length - 1)]) 
		+ sign(attribute[3:signal_length] - attribute[2:(signal_length-1)])))

	features_extracted <- list( "area_under_curve" = AUC, "simple_square_integral" = SSI, 
			"mean_absolute_value" = MAV, "modified_mean_absolute_value_1" = MMAV1, "modified_mean_absolute_value_2" = MMAV2,
			"root_mean_square" = RMS, "waveform_length" = WL, "average_absolute_signal_slope" = AASS, "zero_crossings" = ZC, "slope_sign_alternation" = SSA)

	return(features_extracted)
}

# It returns a list of additional GSR features
# (latency, latency_interval_from_start, half_recovery_time,
# positive_area, negative_area, absolute_negative_area, 
# total_area, absolute_total_area, total_absolute_area)
calculate_new_GSR_features = function(attribute, timestamp){

	col = 1

	# Latency (LAT)
	latTimeValue <- timestamp[which.max(attribute)]
	latTimestamp <- strptime(latTimeValue[c(col)], format = "%Y-%m-%d %H:%M:%S")
	LAT <- as.numeric(latTimestamp)

	# Latency Interval from Start (LATFS)
	startVideo_timestamp <- strptime(timestamp[1], format = "%Y-%m-%d %H:%M:%S")
	startVideo_millisec <- as.numeric(startVideo_timestamp)
	LATFS <- LAT - startVideo_millisec

	# Half Recovery Time (HRT)
	col = 1
	tempValues <- attribute[which.max(attribute): length(attribute)]
	halfPeakValue <- timestamp[which(tempValues <= max(tempValues) / 2)]
	halfPeakTimestamp <- strptime(halfPeakValue[c(col)], format = "%Y-%m-%d %H:%M:%S")
	HRT <- as.numeric(halfPeakTimestamp) - LAT

	# Positive Area (PAR)
	PAR <- 0.5* sum(attribute + abs(attribute))

	# Negative Area (NAR)
	NAR <- 0.5* sum(attribute - abs(attribute))

	# Absolute Negative Area (ANAR)
	ANAR <- abs(NAR)

	# Total Area (TAR)
	TAR <- PAR + NAR

	# Absolute Total Area (ATAR)
	ATAR <- abs(TAR)

	# Total Absolute Area (TAAR)
	TAAR <- PAR + abs(NAR)

	features_extracted <- list( "latency" = LAT, "latency_interval_from_start" = LATFS, "half_recovery_time" = HRT,
		"positive_area" = PAR, "negative_area" = NAR, "absolute_negative_area" = ANAR, 
		"total_area" = TAR, "absolute_total_area" = ATAR, "total_absolute_area" = TAAR )

	return(features_extracted)
}
