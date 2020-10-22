setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)
options(repos = list(CRAN="http://cran.rstudio.com/"))

list.of.packages <- c("easypackages", "ggplot2", "lubridate", "pastecs")
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
    
    tpr_vector <- c()
    tnr_vector <- c()
    ppv_vector <- c()
    npv_vector <- c()
    
    tprNAs <- 0
    tnrNAs <- 0
    ppvNAs <- 0
    npvNAs <- 0

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
        
#         cat("current_tpr = ", current_tpr, "\t", sep="")
#         cat("current_tnr = ", current_tnr, "\t", sep="")
#         cat("current_ppv = ", current_ppv, "\t", sep="")
#         cat("current_npv = ", current_npv, "\n", sep="")
#         
        tpr_vector <- c(tpr_vector, current_tpr)
        tnr_vector <- c(tnr_vector, current_tnr)
        ppv_vector <- c(ppv_vector, current_ppv)
        npv_vector <- c(npv_vector, current_npv)
        
        
        #if(is.na(current_npv)) sys.exit()
        
        if(is.na(current_tpr)==FALSE) tpr_sum <- tpr_sum + current_tpr
        if(is.na(current_tnr)==FALSE) tnr_sum <- tnr_sum + current_tnr
        if(is.na(current_ppv)==FALSE) ppv_sum <- ppv_sum + current_ppv
        if(is.na(current_npv)==FALSE) npv_sum <- npv_sum + current_npv

        if(is.na(current_tpr)) tprNAs <- tprNAs + 1
        if(is.na(current_tnr)) tnrNAs <- tnrNAs + 1
        if(is.na(current_ppv)) ppvNAs <- ppvNAs + 1
        if(is.na(current_npv)) npvNAs <- npvNAs + 1

         
    }
    
        tpr_average <- tpr_sum/length(predicted_values)
        tnr_average <- tnr_sum/length(predicted_values)
        ppv_average  <- ppv_sum/length(predicted_values)
        npv_average  <- npv_sum/length(predicted_values)

#         cat("multi-threshold average TPR = ", dec_three(tpr_average), "\n", sep="")
#         cat("multi-threshold average TNR = ", dec_three(tnr_average), "\n", sep="")
#         cat("multi-threshold average PPV = ", dec_three(ppv_average), "\n", sep="")
#         cat("multi-threshold average NPV = ", dec_three(npv_average), "\n", sep="")
        
#        cat("length(tpr_vector) = ",  length(tpr_vector), "\n", sep="")
#        cat("length(tnr_vector) = ",  length(tnr_vector), "\n", sep="")
#        cat("length(ppv_vector) = ",  length(ppv_vector), "\n", sep="")
#        cat("length(npv_vector) = ",  length(npv_vector), "\n", sep="")
#        
#        cat("tprNAs = ", tprNAs, "\n", sep="")
#        cat("tnrNAs = ", tnrNAs, "\n", sep="")
#        cat("ppvNAs = ", ppvNAs, "\n", sep="")
#        cat("npvNAs = ", npvNAs, "\n", sep="")
       
        rates_vectors <- data.frame(TP_rates=tpr_vector, TN_rates=tnr_vector, PPV_rates=ppv_vector, NPV_rates=npv_vector)
        return(rates_vectors)
        
}

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 
# ground truth
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - 

ONE <- 1
ZERO <- 0

number_of_ones <-   50
number_of_zeros <- 50

cat("  number_of_ones ", number_of_ones, "\n", sep="")
cat("  number_of_zeros ", number_of_zeros, "\n", sep="")

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

theseRatesVectors <- NULL

predictor <- seq(from = 1/total_number_of_samples, to = 1, by = 1/total_number_of_samples)

listDiffROCAUCnormMCC <- c()
listDiffPRAUCnormMCC <- c()

for(k in 1:total_number_of_samples) {

    cat("\n\n=== === === ===\n(k=", k, ")\n", sep="")

    cat("\n predictor: ")
    cat(dec_three(predictor[1]), ", ", dec_three(predictor[2]), ", ", dec_three(predictor[3]), ", ...,  ", dec_three(predictor[total_number_of_samples-2]), ", ", dec_three(predictor[total_number_of_samples-1]), ", ", dec_three(predictor[total_number_of_samples]), "\n\n", sep="")

   theseRatesVectors <-  rates_multi_thresholds(ground_truth, predictor)

   statDesResults <- stat.desc(theseRatesVectors)
   someStatResults <- (statDesResults)[c("mean", "std.dev", "median", "min", "max"),]
   print(dec_three(someStatResults))
   cat("\n")    
   
    cf_output <- confusion_matrix_rates(ground_truth, predictor, " ")
    
    cat("\nuse case  metric         TPR   TNR        PPV    NPV \n", sep="")
    
    cat("UC", k, " MCC_tau = ", dec_three(cf_output$"MCC"), "\t",   dec_three(cf_output$"TP_rate"), "\t",  dec_three(cf_output$"TN_rate"), "\t",  dec_three(cf_output$"PPV"), "\t",  dec_three(cf_output$"NPV"), " [tau = 0.5]\n", sep="")
    
    cat("UC", k, " ROC_AUC = ", dec_three(cf_output$"ROC_AUC"), "\t",   dec_three(someStatResults["max",]$"TP_rates"), "\t",  dec_three(someStatResults["max",]$"TN_rates"), "\t",  dec_three(someStatResults["max",]$"PPV_rates"), "\t",  dec_three(someStatResults["max",]$"NPV_rates"), " [max]\n", sep="")
        
    cat("UC", k, " PR_AUC  = ", dec_three(cf_output$"PR_AUC"), "\t",   dec_three(someStatResults["max",]$"TP_rates"), "\t",  dec_three(someStatResults["max",]$"TN_rates"), "\t",  dec_three(someStatResults["max",]$"PPV_rates"), "\t",  dec_three(someStatResults["max",]$"NPV_rates"), " [max]\n", sep="")

    cat("\n")
    
    listDiffROCAUCnormMCC[k] <-  cf_output$"diffROCAUCnormMCC"
    listDiffPRAUCnormMCC[k] <- cf_output$"diffPRAUCnormMCC"
    
    predictor <- c(predictor[-1], predictor[1])
}

# cat("listDiffROCAUCnormMCC:\n")
# print(listDiffROCAUCnormMCC)
# 
# cat("listDiffPRAUCnormMCC:\n")
# print(listDiffPRAUCnormMCC)

delta_normMCCs_ROC_AUCs <- data.frame(use_case = seq(1,total_number_of_samples), listDiffROCAUCnormMCC)
delta_normMCCs_PR_AUCs <- data.frame(use_case = seq(1,total_number_of_samples), listDiffPRAUCnormMCC)

topRanks <- 5
cat("The ", topRanks, " use cases with highest (normMCC - ROC AUC) difference:\n")
print(delta_normMCCs_ROC_AUCs[order(-listDiffROCAUCnormMCC), ][1:topRanks,])

cat("The ", topRanks, " use cases with highest (normMCC - PR AUC) difference:\n")
print(delta_normMCCs_PR_AUCs[order(-listDiffPRAUCnormMCC), ][1:topRanks,])


