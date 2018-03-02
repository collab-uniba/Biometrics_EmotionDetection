library(caret)

scalar_metrics <- function(predictions, truth, outdir, outfile) {

#if(!dir.exists(outdir))
#    dir.create(outdir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
#  output_file = paste(outdir, outfile, sep = "/")
  
  
  cm = as.matrix(table(Actual = truth, Predicted = predictions)) # create the confusion matrix
  out <- capture.output(cm)
  cat("\nConfusion Matrix:\n", out, file=outfile, sep="\n", append= TRUE)
  
 n = sum(cm) # number of instances
 nc = nrow(cm) # number of classes
 diag = diag(cm) # number of correctly classified instances per class 
 rowsums = apply(cm, 1, sum) # number of instances per class
 colsums = apply(cm, 2, sum) # number of predictions per class
 p = rowsums / n # distribution of instances over the actual classes
 q = colsums / n # distribution of instances over the predicted classes
 
 accuracy = sum(diag) / n 
 precision = diag / colsums 
 recall = diag / rowsums 
 f1 = 2 * precision * recall / (precision + recall) 
 
metrics <- data.frame(precision, recall, f1) 

 oneVsAll = lapply(1 : nc,
                      function(i){
                        v = c(cm[i,i],
                              rowsums[i] - cm[i,i],
                              colsums[i] - cm[i,i],
                              n-rowsums[i] - colsums[i] + cm[i,i]);
                        return(matrix(v, nrow = 2, byrow = T))})
						
						
#Summing up the values of these 3 matrices results in one confusion matrix and 
#allows us to compute weighted metrics such as average accuracy and micro-averaged metrics						
s <- matrix(0, nrow = 2, ncol = 2)
 for(i in 1 : nc){s = s + oneVsAll[[i]]}

#Because the sum of the one-vs-all matrices is a symmetric matrix, the micro-averaged precision, recall, and F-1 wil be the same.
#apply: Apply Functions Over Array Margins(1: row; 2: column; 1:2 both)
micro_prf = (diag(s) / apply(s,1, sum))[1]; 

out <- capture.output(metrics)
cat("\nMetrics micro:\n", out, file=outfile, sep="\n", append=TRUE)

out <- capture.output(micro_prf)
cat("\n Overall (Micro-average):\n", out, file=outfile, sep="\n", append=TRUE)


macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)

metrics_macro <- data.frame(macroPrecision, macroRecall, macroF1)
out <- capture.output(metrics_macro)
cat("\nMetrics macro:\n", out, file=outfile, sep="\n", append=TRUE)
}