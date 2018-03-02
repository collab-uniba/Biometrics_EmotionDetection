.libPaths( c( .libPaths(), "./lib") )
library(caret)

mc_nemar <- function(predictions_old, predictions_new, outdir, outfile) {

	if(!dir.exists(outdir))
	    dir.create(outdir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
	  output_file = paste(outdir, outfile, sep = "")

	if ((nlevels(predictions_old) == 2) && (nlevels(predictions_new) == 2)){
		metrics <- confusionMatrix(data = predictions_old, reference = predictions_new)
		print(metrics)
		out <- capture.output(metrics)
		cat("\nMetrics:\n", out, file=output_file, sep="\n", append=TRUE)
	} else {
		cat("\nMetrics:\n", "Error: there must be at least 2 factors levels in the data ", "\n", file = output_file, append = TRUE)
	}
	

}