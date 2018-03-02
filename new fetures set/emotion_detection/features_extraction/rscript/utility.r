# Author: Daniela Girardi, Garofalo Antonio
# This script provides a list of utility method to format signal features, 
# build the dataset for each signal and combining several type of signal features in order to build combined dataset

# 
# 
# 
# ------------------------------------- DATASET BUILDER -------------------------------------
# 
# 
# 

datasetPath <- "C:/emotion_detection/features_extraction/dataset/"

# A matrix row used as header for EEG dataset, in which are saved the features names
EEG_header <- matrix(c("id",
	"alpha_mean_corrected", "alpha_min_corrected", "alpha_max_corrected", "alpha_var_corrected", "alpha_sd_corrected", 
	"beta_mean_corrected", "beta_min_corrected", "beta_max_corrected", "beta_var_corrected", "beta_sd_corrected",
	"gamma_mean_corrected", "gamma_min_corrected", "gamma_max_corrected", "gamma_var_corrected", "gamma_sd_corrected",
	"delta_mean_corrected", "delta_min_corrected", "delta_max_corrected", "delta_var_corrected", "delta_sd_corrected",
	"theta_mean_corrected", "theta_min_corrected", "theta_max_corrected", "theta_var_corrected", "theta_sd_corrected",

	"attention_mean_corrected", "attention_min_corrected", "attention_max_corrected", "attention_var_corrected", "attention_sd_corrected", 
	"meditation_mean_corrected", "meditation_min_corrected", "meditation_max_corrected", "meditation_var_corrected", "meditation_sd_corrected", 
	
	"diff_alpha_beta", "diff_alpha_gamma", "diff_alpha_delta", "diff_alpha_theta", 
	"diff_beta_alpha", "diff_beta_gamma", "diff_beta_delta", "diff_beta_theta", 
	"diff_gamma_alpha", "diff_gamma_beta", "diff_gamma_delta", "diff_gamma_theta",
	"diff_delta_alpha",	"diff_delta_beta", "diff_delta_gamma", "diff_delta_theta", 
	"diff_theta_alpha", "diff_theta_beta", "diff_theta_gamma", "diff_theta_delta ", 
	"diff_theta_alpha_plus_beta", "diff_beta_alpha_plus_theta",
	
	"alpha_latency", "alpha_latency_interval_from_start", "alpha_amplitude", "alpha_absolute_amplitude", "alpha_absolute_latency_amplitude_ratio",
	"alpha_peak_to_peak", "alpha_zero_crossings", "alpha_zero_crossings_peak_to_peak",
	"alpha_positive_area", "alpha_negative_area", "alpha_absolute_negative_area", "alpha_total_area", "alpha_absolute_total_area", "alpha_total_absolute_area",
	"alpha_average_absolute_signal_slope", "alpha_slope_sign_alternation", 

	"beta_latency", "beta_latency_interval_from_start", "beta_amplitude", "beta_absolute_amplitude", "beta_absolute_latency_amplitude_ratio",
	"beta_peak_to_peak", "beta_zero_crossings", "beta_zero_crossings_peak_to_peak", 
	"beta_positive_area", "beta_negative_area", "beta_absolute_negative_area", "beta_total_area", "beta_absolute_total_area", "beta_total_absolute_area",
	"beta_average_absolute_signal_slope", "beta_slope_sign_alternation", 

	"gamma_latency", "gamma_latency_interval_from_start", "gamma_amplitude", "gamma_absolute_amplitude", "gamma_absolute_latency_amplitude_ratio",
	"gamma_peak_to_peak", "gamma_zero_crossings", "gamma_zero_crossings_peak_to_peak", 
	"gamma_positive_area", "gamma_negative_area", "gamma_absolute_negative_area", "gamma_total_area", "gamma_absolute_total_area", "gamma_total_absolute_area",
	"gamma_average_absolute_signal_slope", "gamma_slope_sign_alternation", 

	"delta_latency", "delta_latency_interval_from_start", "delta_amplitude", "delta_absolute_amplitude", "delta_absolute_latency_amplitude_ratio",
	"delta_peak_to_peak", "delta_zero_crossings", "delta_zero_crossings_peak_to_peak", 
	"delta_positive_area", "delta_negative_area", "delta_absolute_negative_area", "delta_total_area", "delta_absolute_total_area", "delta_total_absolute_area",
	"delta_average_absolute_signal_slope", "delta_slope_sign_alternation", 

	"theta_latency", "theta_latency_interval_from_start", "theta_amplitude", "theta_absolute_amplitude", "theta_absolute_latency_amplitude_ratio",
	"theta_peak_to_peak", "theta_zero_crossings", "theta_zero_crossings_peak_to_peak",
	"theta_positive_area", "theta_negative_area", "theta_absolute_negative_area", "theta_total_area", "theta_absolute_total_area", "theta_total_absolute_area",
	"theta_average_absolute_signal_slope", "theta_slope_sign_alternation", 

	"arousal", "valence"
	), 1, 140)

# A matrix row used as header for EMG dataset, in which are saved the features names
EMG_header <- matrix(c("id",
	"ch1_corrected_mean", "ch1_corrected_min", "ch1_corrected_max", "ch1_corrected_var", "ch1_corrected_sd",
	"ch2_corrected_mean", "ch2_corrected_min", "ch2_corrected_max", "ch2_corrected_var", "ch2_corrected_sd", 
	
	"ch1_mean_rmse", "ch1_mean_movavg", "ch1_mean_integ",
	"ch2_mean_rmse", "ch2_mean_movavg", "ch2_mean_integ", 
	
	"ch1_corrected_area_under_curve", "ch1_corrected_simple_square_integral", 
	"ch1_corrected_mean_absolute_value", "ch1_corrected_modified_mean_absolute_value_1", "ch1_corrected_modified_mean_absolute_value_2", 
	"ch1_corrected_root_mean_square", "ch1_corrected_waveform_length", 
	"ch1_corrected_average_absolute_signal_slope", "ch1_corrected_zero_crossings", "ch1_corrected_slope_sign_alternation",
	
	"ch2_corrected_area_under_curve", "ch2_corrected_simple_square_integral", 
	"ch2_corrected_mean_absolute_value", "ch2_corrected_modified_mean_absolute_value_1", "ch2_corrected_modified_mean_absolute_value_2", 
	"ch2_corrected_root_mean_square", "ch2_corrected_waveform_length", 
	"ch2_corrected_average_absolute_signal_slope", "ch2_corrected_zero_crossings", "ch2_corrected_slope_sign_alternation",
	
	"arousal", "valence"
	), 1, 39)

# A matrix row used as header for GSR dataset, in which are saved the features names
GSR_header <- matrix(c("id",
	"phasic_corrected_mean", "phasic_corrected_min", "phasic_corrected_max", "phasic_corrected_var", "phasic_corrected_sd",
		
	"phasic_derivative_corrected_mean_deriv", "phasic_derivative_corrected_mean_deriv_only_neg", "phasic_derivative_corrected_prop_neg_samples", 
		
	"corrected_mean_peaks_ampl", "corrected_min_peaks_ampl", "corrected_max_peaks_ampl", "corrected_num_phasic_peaks_divided_min", 
	"corrected_sum_phasic_peaks_divided_min", 

	"phasic_corrected_latency", "phasic_corrected_latency_interval_from_start", "phasic_corrected_half_recovery_time",
	"phasic_corrected_positive_area", "phasic_corrected_negative_area", "phasic_corrected_absolute_negative_area", 
	"phasic_corrected_total_area", "phasic_corrected_absolute_total_area", "phasic_corrected_total_absolute_area",

	"arousal", "valence"
	), 1, 25)

# 
# ----------------- ROWS BUILDER -------------------
# 

# It returns a matrix row which can be written on CSV file. It contains all EEG Features (see features_extraction.r for details on the structure of the parameters used in this function)
build_row_for_EEG_dataset = function (id, 
	alpha_features_corrected, beta_features_corrected, gamma_features_corrected, delta_features_corrected, theta_features_corrected, attention_corrected, meditation_corrected,
		diff_frquencies,
		new_apha_features_corrected, new_beta_features_corrected, new_gamma_features_corrected, new_delta_features_corrected, new_theta_features_corrected,
		arousal, valence
	){

	feature_video <- c(id, 
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
		mean(diff_frequencies$diff_theta_alpha_plus_beta), mean(diff_frequencies$diff_beta_alpha_plus_theta), 

		new_apha_features_corrected$latency, new_apha_features_corrected$latency_interval_from_start, new_apha_features_corrected$amplitude, new_apha_features_corrected$absolute_amplitude, new_apha_features_corrected$absolute_latency_amplitude_ratio,
		new_apha_features_corrected$peak_to_peak, new_apha_features_corrected$zero_crossings, new_apha_features_corrected$zero_crossings_peak_to_peak,
		new_apha_features_corrected$positive_area, new_apha_features_corrected$negative_area, new_apha_features_corrected$absolute_negative_area, new_apha_features_corrected$total_area, new_apha_features_corrected$absolute_total_area, new_apha_features_corrected$total_absolute_area,
		new_apha_features_corrected$average_absolute_signal_slope, new_apha_features_corrected$slope_sign_alternation,

		new_beta_features_corrected$latency, new_beta_features_corrected$latency_interval_from_start, new_beta_features_corrected$amplitude, new_beta_features_corrected$absolute_amplitude, new_beta_features_corrected$absolute_latency_amplitude_ratio,
		new_beta_features_corrected$peak_to_peak, new_beta_features_corrected$zero_crossings, new_beta_features_corrected$zero_crossings_peak_to_peak, 
		new_beta_features_corrected$positive_area, new_beta_features_corrected$negative_area, new_beta_features_corrected$absolute_negative_area, new_beta_features_corrected$total_area, new_beta_features_corrected$absolute_total_area, new_beta_features_corrected$total_absolute_area,
		new_beta_features_corrected$average_absolute_signal_slope, new_beta_features_corrected$slope_sign_alternation,
		
		new_gamma_features_corrected$latency, new_gamma_features_corrected$latency_interval_from_start, new_gamma_features_corrected$amplitude, new_gamma_features_corrected$absolute_amplitude, new_gamma_features_corrected$absolute_latency_amplitude_ratio,
		new_gamma_features_corrected$peak_to_peak, new_gamma_features_corrected$zero_crossings, new_gamma_features_corrected$zero_crossings_peak_to_peak, 
		new_gamma_features_corrected$positive_area, new_gamma_features_corrected$negative_area, new_gamma_features_corrected$absolute_negative_area, new_gamma_features_corrected$total_area, new_gamma_features_corrected$absolute_total_area, new_gamma_features_corrected$total_absolute_area,
		new_gamma_features_corrected$average_absolute_signal_slope, new_gamma_features_corrected$slope_sign_alternation,
		
		new_delta_features_corrected$latency, new_delta_features_corrected$latency_interval_from_start, new_delta_features_corrected$amplitude, new_delta_features_corrected$absolute_amplitude, new_delta_features_corrected$absolute_latency_amplitude_ratio,
		new_delta_features_corrected$peak_to_peak, new_delta_features_corrected$zero_crossings, new_delta_features_corrected$zero_crossings_peak_to_peak,
		new_delta_features_corrected$positive_area, new_delta_features_corrected$negative_area, new_delta_features_corrected$absolute_negative_area, new_delta_features_corrected$total_area, new_delta_features_corrected$absolute_total_area, new_delta_features_corrected$total_absolute_area,
		new_delta_features_corrected$average_absolute_signal_slope, new_delta_features_corrected$slope_sign_alternation,
		
		new_theta_features_corrected$latency, new_theta_features_corrected$latency_interval_from_start, new_theta_features_corrected$amplitude, new_theta_features_corrected$absolute_amplitude, new_theta_features_corrected$absolute_latency_amplitude_ratio,
		new_theta_features_corrected$peak_to_peak, new_theta_features_corrected$zero_crossings, new_theta_features_corrected$zero_crossings_peak_to_peak, 
		new_theta_features_corrected$positive_area, new_theta_features_corrected$negative_area, new_theta_features_corrected$absolute_negative_area, new_theta_features_corrected$total_area, new_theta_features_corrected$absolute_total_area, new_theta_features_corrected$total_absolute_area,
		new_theta_features_corrected$average_absolute_signal_slope, new_theta_features_corrected$slope_sign_alternation,

		arousal, valence)
	
	features_video <- as.matrix(t(feature_video))

	return (features_video)
	
}

# It returns a matrix row which can be written on CSV file. It contains all EMG Features (see features_extraction.r for details on the structure of the parameters used in this function)
build_row_for_EMG_dataset = function( id, emg_features_video_ch1_corrected, emg_features_video_ch2_corrected, 
	specific_features_ch1, specific_features_ch2, 
	new_emg_features_video_ch1_corrected, new_emg_features_video_ch2_corrected,
	arousal, valence ){

	feature_video <- c(id, 
		emg_features_video_ch1_corrected$mean, emg_features_video_ch1_corrected$min, emg_features_video_ch1_corrected$max, emg_features_video_ch1_corrected$var ,emg_features_video_ch1_corrected$sd,
		emg_features_video_ch2_corrected$mean, emg_features_video_ch2_corrected$min,emg_features_video_ch2_corrected$max,emg_features_video_ch2_corrected$var, emg_features_video_ch2_corrected$sd, 
		
		specific_features_ch1$mean_rmse, specific_features_ch1$mean_movavg, specific_features_ch1$mean_integ,
		specific_features_ch2$mean_rmse, specific_features_ch2$mean_movavg, specific_features_ch2$mean_integ, 

		new_emg_features_video_ch1_corrected$area_under_curve, new_emg_features_video_ch1_corrected$simple_square_integral, 
		new_emg_features_video_ch1_corrected$mean_absolute_value, new_emg_features_video_ch1_corrected$modified_mean_absolute_value_1, new_emg_features_video_ch1_corrected$modified_mean_absolute_value_2, 
		new_emg_features_video_ch1_corrected$root_mean_square, new_emg_features_video_ch1_corrected$waveform_length, 
		new_emg_features_video_ch1_corrected$average_absolute_signal_slope, new_emg_features_video_ch1_corrected$zero_crossings, new_emg_features_video_ch1_corrected$slope_sign_alternation,
		
		new_emg_features_video_ch2_corrected$area_under_curve, new_emg_features_video_ch2_corrected$simple_square_integral, 
		new_emg_features_video_ch2_corrected$mean_absolute_value, new_emg_features_video_ch2_corrected$modified_mean_absolute_value_1, new_emg_features_video_ch2_corrected$modified_mean_absolute_value_2, 
		new_emg_features_video_ch2_corrected$root_mean_square, new_emg_features_video_ch2_corrected$waveform_length, 
		new_emg_features_video_ch2_corrected$average_absolute_signal_slope, new_emg_features_video_ch2_corrected$zero_crossings, new_emg_features_video_ch2_corrected$slope_sign_alternation,
		
		arousal, valence)
	
	features_video <- as.matrix(t(feature_video))

	return (features_video)

} 

# It returns a matrix row which can be written on CSV file. It contains all GSR Features (see features_extraction.r for details on the structure of the parameters used in this function)
build_row_for_GSR_dataset = function( id, phasic_features_corrected, 
	phasic_derivative_features_corrected, phasic_peaks_features_corrected, 
	new_phasic_features_corrected, 
	arousal, valence ){


	feature_video <- c(id,
		phasic_features_corrected$mean, phasic_features_corrected$min, phasic_features_corrected$max, phasic_features_corrected$var, phasic_features_corrected$sd,
		
		phasic_derivative_features_corrected$mean_deriv, phasic_derivative_features_corrected$mean_deriv_only_neg, phasic_derivative_features_corrected$prop_neg_samples, 
		
		phasic_peaks_features_corrected$mean_peaks_ampl, phasic_peaks_features_corrected$min_peaks_ampl, phasic_peaks_features_corrected$max_peaks_ampl, phasic_peaks_features_corrected$num_phasic_peaks_divided_min, 
		phasic_peaks_features_corrected$sum_phasic_peaks_divided_min, 

		new_phasic_features_corrected$latency, new_phasic_features_corrected$latency_interval_from_start, new_phasic_features_corrected$half_recovery_time,
		new_phasic_features_corrected$positive_area, new_phasic_features_corrected$negative_area, new_phasic_features_corrected$absolute_negative_area, 
		new_phasic_features_corrected$total_area, new_phasic_features_corrected$absolute_total_area, new_phasic_features_corrected$total_absolute_area,

		arousal, valence)
	
	features_video <- as.matrix(t(feature_video))

	return (features_video)

} 

# It append to a pre-existing matrix a new matrix row
# WARNING: the matrix params MUST HAVE the same number columns
append_new_video_features = function(old_video_features_list, new_video_features){
	nCol <- ncol(old_video_features_list)
	new_matrix_row <- matrix(new_video_features , 1, nCol)
	new_video_features_list <- rbind(old_video_features_list, new_matrix_row)
	
	return(new_video_features_list)
} 

# 
# ----------------- SINGLE SIGNAL DATASET BUILDER -------------------
# 

# It writes on a csv a matrix (features_list) containing all features(columns) for each video(rows)
add_to_EEG_dataset = function(features_list){
	
	EEG_dataset <- paste(datasetPath, "IEEG_dataset.csv", sep= "")
	if(!file.exists(EEG_dataset)){
		write.table(EEG_header, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}
	write.table(features_list, file = EEG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
}

# It writes on a csv a matrix (features_list) containing all features(columns) for each video(rows)
add_to_EMG_dataset = function(features_list){

	EMG_dataset <- paste(datasetPath, "EMG_dataset.csv", sep= "")
	if(!file.exists(EMG_dataset)){
		write.table(EMG_header, file = EMG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}
	write.table(features_list, file = EMG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
}

# It writes on a csv a matrix (features_list) containing all features(columns) for each video(rows)
add_to_GSR_dataset = function(features_list){

	GSR_dataset <- paste(datasetPath, "GSR_dataset.csv", sep= "")
	if(!file.exists(GSR_dataset)){
		write.table(GSR_header, file = GSR_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}
	write.table(features_list, file = GSR_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)

}

# 
# ----------------- COMBINED SIGNAL DATASET BUILDER -------------------
# 

# The section below provides to build a set of dataset combining several types of signal features

add_to_EEG_EMG_dataset = function(eeg_features, emg_features){
	combined_EEG_EMG_dataset <- paste(datasetPath, "IEEG_EMG_dataset.csv", sep= "")
	
	if(!file.exists(combined_EEG_EMG_dataset)){
		EEG_header_col <- ncol(EEG_header) -2
		EEG_header_cut <- matrix(EEG_header[, 1:EEG_header_col], 1, EEG_header_col)
		EEG_EMG_header <- merge(EEG_header_cut, EMG_header, by.x = 'V1', by.y = 'V1')
		write.table(EEG_EMG_header, file = combined_EEG_EMG_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}

	combined_EEG_EMG <- merge(eeg_features, emg_features, by.x = 'V1', by.y = 'V1')
	write.table(combined_EEG_EMG, file = combined_EEG_EMG_dataset,  sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
}

add_to_EEG_GSR_dataset = function(eeg_features, gsr_features){
	combined_EEG_GSR_dataset <- paste(datasetPath, "IEEG_GSR_dataset.csv", sep= "")

	if(!file.exists(combined_EEG_GSR_dataset)){
		EEG_header_col <- ncol(EEG_header) -2
		EEG_header_cut <- matrix(EEG_header[, 1:EEG_header_col], 1, EEG_header_col)
		EEG_GSR_header <- merge(EEG_header_cut, GSR_header, by.x = 'V1', by.y = 'V1')
		write.table(EEG_GSR_header, file = combined_EEG_GSR_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}

	combined_EEG_GSR <- merge(eeg_features, gsr_features, by.x = 'V1', by.y = 'V1')
	write.table(combined_EEG_GSR, file = combined_EEG_GSR_dataset,  sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
}

add_to_EMG_GSR_dataset = function(emg_features, gsr_features){
	combined_EMG_GSR_dataset <- paste(datasetPath, "EMG_GSR_dataset.csv", sep= "")

	if(!file.exists(combined_EMG_GSR_dataset)){
		EMG_header_col <- ncol(EMG_header) -2
		EMG_header_cut <- matrix(EMG_header[, 1:EMG_header_col], 1, EMG_header_col)
		EMG_GSR_header <- merge(EMG_header_cut, GSR_header, by.x = 'V1', by.y = 'V1')
		write.table(EMG_GSR_header, file = combined_EMG_GSR_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}

	combined_EMG_GSR <- merge(emg_features, gsr_features, by.x = 'V1', by.y = 'V1')
	write.table(combined_EMG_GSR, file = combined_EMG_GSR_dataset,  sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
}

add_to_EEG_EMG_GSR_dataset = function(eeg_features, emg_features, gsr_features){
	combined_EEG_EMG_GSR_dataset <- paste(datasetPath, "IEEG_EMG_GSR_dataset.csv", sep= "")

	if(!file.exists(combined_EEG_EMG_GSR_dataset)){
		EEG_header_col <- ncol(EEG_header) -2
		EEG_header_cut <- matrix(EEG_header[, 1:EEG_header_col], 1, EEG_header_col)
		EMG_header_col <- ncol(EMG_header) -2
		EMG_header_cut <- matrix(EMG_header[, 1:EMG_header_col], 1, EMG_header_col)
		EEG_EMG_header <- merge(EEG_header_cut, EMG_header_cut, by.x = 'V1', by.y = 'V1')
		EEG_EMG_GSR_header <-  merge(EEG_EMG_header, GSR_header, by.x = 'V1', by.y = 'V1')
		write.table(EEG_EMG_GSR_header, file = combined_EEG_EMG_GSR_dataset, sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
	}

	combined_EEG_EMG <- merge(eeg_features, emg_features, by.x = 'V1', by.y = 'V1')
	combined_EEG_EMG_GSR <- merge(combined_EEG_EMG, gsr_features, by.x = 'V1', by.y = 'V1')
	write.table(combined_EEG_EMG_GSR, file = combined_EEG_EMG_GSR_dataset,  sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE,  append=TRUE)
}
