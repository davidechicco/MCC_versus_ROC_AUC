setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)
options(repos = list(CRAN="http://cran.rstudio.com/"))

list.of.packages <- c("easypackages", "ggplot2", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./utils.r")
source("./confusion_matrix_rates.r")


# rates_multi_thresholds
rates_multi_thresholds <- function(actual_labels, predicted_values)
{
    tpr_sum <- 0
    tnr_sum <- 0
    ppv_sum <- 0
    npv_sum <- 0

    for(i in 1:length(predicted_values)) {
        
        current_threshold <- predicted_values[i]
    
        predicted_values_binary <- as.numeric(predicted_values)
        predicted_values_binary[predicted_values_binary>=current_threshold]=1
        predicted_values_binary[predicted_values_binary<current_threshold]=0
        
        actual <- actual_labels
        predicted <- predicted_values_binary
  
        TP <- sum(actual == 1 & predicted == 1)
        TN <- sum(actual == 0 & predicted == 0)
        FP <- sum(actual == 0 & predicted == 1)
        FN <- sum(actual == 1 & predicted == 0)
        
#         cat("\nTOTAL:\n\n")
#         cat(" FN = ", (FN), " / ", (FN+TP), "\t (truth == 1) & (prediction < threshold)\n");
#         cat(" TP = ", (TP), " / ", (FN+TP),"\t (truth == 1) & (prediction >= threshold)\n\n");
#             
# 
#         cat(" FP = ", (FP), " / ", (FP+TN), "\t (truth == 0) & (prediction >= threshold)\n");
#         cat(" TN = ", (TN), " / ", (FP+TN), "\t (truth == 0) & (prediction < threshold)\n\n");

        current_tpr <-  TP / (TP + FN)
        current_tnr <-  TN / (TN + FP)
        current_ppv <- TP / (TP + FP)
        current_npv <- TN / (TN + FN)
        
        #if(is.na(current_npv)) sys.exit()
        
        if(is.na(current_tpr)==FALSE) tpr_sum <- tpr_sum + current_tpr
        if(is.na(current_tnr)==FALSE) tnr_sum <- tnr_sum + current_tnr
        if(is.na(current_ppv)==FALSE) ppv_sum <- ppv_sum + current_ppv
        if(is.na(current_npv)==FALSE) npv_sum <- npv_sum + current_npv
    }
    
        tpr_average <- tpr_sum/length(predicted_values)
        tnr_average <- tnr_sum/length(predicted_values)
        ppv_average  <- ppv_sum/length(predicted_values)
        npv_average  <- npv_sum/length(predicted_values)

        cat("multi-threshold average TPR = ", dec_three(tpr_average), "\n", sep="")
        cat("multi-threshold average TNR = ", dec_three(tnr_average), "\n", sep="")
        cat("multi-threshold average PPV = ", dec_three(ppv_average), "\n", sep="")
        cat("multi-threshold average NPV = ", dec_three(npv_average), "\n", sep="")
        
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
# ground truth
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 

ONE <- 1
ZERO <- 0

number_of_ones <-   90
number_of_zeros <- 5
total_number_of_samples <- number_of_zeros + number_of_ones

ground_truth <- c(rep(ONE, number_of_ones), rep(ZERO, number_of_zeros))

positive_perc <- number_of_ones * 100 / total_number_of_samples
negative_perc <- number_of_zeros * 100 / total_number_of_samples

cat("Imbalance of the dataset:\n")
cat("  positive percentage ", dec_two(positive_perc), "%\n", sep="")
cat("  negative percentage ", dec_two(negative_perc), "%\n\n", sep="")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
# predictor
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 

predictor <- seq(from = 1/total_number_of_samples, to = 1, by = 1/total_number_of_samples)

for(k in 1:total_number_of_samples) {

    cat("\n\n\n(k=", k, ")\n", sep="")

#     cat("predictor:\n")
#     print(predictor)

    rates_multi_thresholds(ground_truth, predictor)

    cf_output <- confusion_matrix_rates(ground_truth, predictor, " ")
    predictor <- c(predictor[-1], predictor[1])
}

