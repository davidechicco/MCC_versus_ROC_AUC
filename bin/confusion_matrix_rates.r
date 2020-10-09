options(stringsAsFactors = FALSE)
# library("clusterSim")

list.of.packages <- c("easypackages", "PRROC", "e1071", "Metrics", "MLmetrics") #, "rcompanion", "irr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(new.packages)
# script_dir <- dirname(sys.frame(1)$ofile)
# cat("script_dir: ", script_dir, "\n", sep="")
source("utils.r")

threshold <- 0.5

# Brier score
Brier_score_function <- function(actual_labels, predicted_values) 
{
    this_Brier_score <- sum((predicted_values - actual_labels)^2) / length(predicted_values)
    # cat("Brier score (original)= ", dec_three(this_Brier_score), " (best value: 0; worst value: 1)\n")

    return(this_Brier_score)
}

# regression rates
regression_rates <- function(actual_labels, predicted_values, keyword)
{

    library("Metrics")
    thisRMSE <- rmse(actual_labels, predicted_values)
    thisMAE <- mae(actual_labels, predicted_values)
    thisMSE <- mse(actual_labels, predicted_values)
    thisSMAPE <- smape(actual_labels, predicted_values)
    
    thisR2score <- MLmetrics::R2_Score(predicted_values, actual_labels) # (predicted_values, actual_labels) # notice the swap
    # R2_Score(y_pred, y_true)

    cat("  @@@ regression :: \t R^2  \t MAE \t MSE  \t SMAPE \t RMSE \n")
    cat("  @@@ regression :: \t ", dec_three(thisR2score), " \t ", dec_three(thisMAE), " \t ", dec_three(thisMSE),  " \t ", dec_three(thisSMAPE),  " \t ", dec_three(thisRMSE)," \n", sep="")
    
    cat("\t(R^2: best value = +1, worst value= -infinity)\n\n\n")
    
    NUM_METRICS <- 5
    outputDataframe <- matrix(ncol=NUM_METRICS, nrow=1)
    outputDataframe[,1] <- thisR2score
    outputDataframe[,2] <- thisMAE
    outputDataframe[,3] <- thisMSE
    outputDataframe[,4] <- thisSMAPE
    outputDataframe[,5] <- thisRMSE
    colnames(outputDataframe) <- c( "R^2", "MAE", "MSE", "SMAPE", "RMSE")

    return(outputDataframe)
}


# Confusion matrix rates
confusion_matrix_rates <- function (actual_labels, predicted_values, keyword)
{

    fg_test <- predicted_values[actual_labels==1]
    bg_test <- predicted_values[actual_labels==0]

    library("PRROC")
    pr_curve_test <- pr.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
    # plot(pr_curve_test)
    # print(pr_curve_test)
    prc_auc <- pr_curve_test$auc.integral
    # cat("\nPR AUC (integral) \t", prc_auc, "\n", sep="")    
    # cat("PRC AUC (Davis & Goadrich) ", pr_curve_test$auc.davis.goadrichl, "\n", sep="")

    roc_curve_test <- PRROC::roc.curve(scores.class0 = fg_test, scores.class1 = bg_test, curve = F)
    # plot(pr_curve_test)
    # print(roc_curve_test)
    roc_auc <- roc_curve_test$auc
    # cat("ROC AUC \t\t", roc_auc, "\n\n", sep="")

    theBrierScore <- Brier_score_function(actual_labels, predicted_values)


    predicted_values_binary <- as.numeric(predicted_values)
    predicted_values_binary[predicted_values_binary>=threshold]=1
    predicted_values_binary[predicted_values_binary<threshold]=0

    actual <- actual_labels
    predicted <- predicted_values_binary
  
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  
#   cat("\nTOTAL:\n\n")
#   cat(" FN = ", (FN), " / ", (FN+TP), "\t (truth == 1) & (prediction < threshold)\n");
#   cat(" TP = ", (TP), " / ", (FN+TP),"\t (truth == 1) & (prediction >= threshold)\n\n");
# 	
# 
#   cat(" FP = ", (FP), " / ", (FP+TN), "\t (truth == 0) & (prediction >= threshold)\n");
#   cat(" TN = ", (TN), " / ", (FP+TN), "\t (truth == 0) & (prediction < threshold)\n\n");

  sum1 <- TP+FP; sum2 <-TP+FN ; sum3 <-TN+FP ; sum4 <- TN+FN;
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  thisMcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  
  f1_score <- 2*TP / (2*TP + FP + FN)
  accuracy <- (TN+TP) / (TN + TP + FP + FN)
  recall <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  balanced_accuracy <- 0.5*(recall+specificity)
  PPV <-  TP /(TP+FP)
  NPV <- TN /(TN+FN)
  
 
  kappa_upper = 2*(TP*TN - FP*FN)
  kappa_lower = (TP + FP )*(FP + TN) + (TP + FN )*(FN + TN)
  theCohenKappa <- kappa_upper / kappa_lower
  
#   cat("Cohen's Kappa = ",  dec_three(theCohenKappa), "\t (worst value: -1; best value: +1)\n", sep="")
#   cat("Brier score (original) = ", dec_three(theBrierScore), "\t (best value: 0; worst value: 1)\n")
#   
   binary_Brier_score <- (FP+FN) / (TN + TP + FP + FN)
#   cat("Brier score (binary) = ", dec_three(binary_Brier_score), "\t (best value: 0; worst value: 1)\n")
#   cat("MCC = ", dec_three(thisMcc), "\t (best value: -1; worst value: +1)\n")
  
  normMCC <- (thisMcc + 1)/2
  complBrieScore <- 1 - theBrierScore
  
#   cat(" K \t BS \t binBS \t MCC \t normMCC  1-BS\n")
#   cat(dec_three(theCohenKappa), " \t ",  sep="")
#   cat(dec_three(theBrierScore), " \t ",  sep="")
#   cat(dec_three(binary_Brier_score), " \t ",  sep="")
#   cat(dec_three(thisMcc), " \t ",  sep="")
#   cat(dec_three(normMCC), " \t ",  sep="")
#   cat(dec_three(complBrieScore), " \n ",  sep="")
  
  diffROCAUCnormMCC <- abs(normMCC - roc_auc)
  
  cat(" MCC \t normMCC  ROC AUC  PR AUC delta(ROC, normMCC)\n")
  cat(dec_three(thisMcc), " \t ",  sep="")
  cat(dec_three(normMCC), " \t ",  sep="")
  cat(dec_three(roc_auc), " \t  ",  sep="")
  cat(dec_three(prc_auc), " \t ",  sep="")
  cat(dec_three(diffROCAUCnormMCC), " \n ",  sep="")
  
  cat("F1_score accuracy  TPR  TNR    PPV    NPV\n")
  cat(dec_three(f1_score), " \t  ",  sep="")
  cat(dec_three(accuracy), "   ",  sep="")
  cat(dec_three(recall), "  ",  sep="")
  cat(dec_three(specificity),  "  ",  sep="")
  cat(dec_three(PPV), "   ",  sep="")
  cat(dec_three(NPV),  " \n",  sep="")
 
  #  resultsList <- list("MCC" = thisMcc, "F1 score" = f1_score, "accuracy" = accuracy, "TP rate" = recall, "TN rate" = specificity, "PR AUC" = prc_auc, "ROC AUC" = roc_auc)

    NUM_METRICS <- 13
    outputDataframe <- matrix(ncol=NUM_METRICS, nrow=1)
    outputDataframe[,1] <- thisMcc
    outputDataframe[,2] <- f1_score
    outputDataframe[,3] <- accuracy
    outputDataframe[,4] <- recall
    outputDataframe[,5] <- specificity
    outputDataframe[,6] <- PPV
    outputDataframe[,7] <- NPV
    outputDataframe[,8] <- prc_auc
    outputDataframe[,9] <- roc_auc
    outputDataframe[,10] <- theCohenKappa
    outputDataframe[,11] <- theBrierScore
    outputDataframe[,12] <- normMCC
    outputDataframe[,13] <- complBrieScore
    colnames(outputDataframe) <- c("MCC", "F1_score", "accuracy", "TP_rate", "TN_rate", "PPV", "NPV", "PR_AUC", "ROC_AUC", "K", "BS", "normMCC", "complBS")

    return(outputDataframe)
}

# Matthews correlation coefficient
mcc <- function (actual, predicted)
{
  # Compute the Matthews correlation coefficient (MCC) score
  # Jeff Hebert 9/1/2016
  # Geoffrey Anderson 10/14/2016 
  # Added zero denominator handling.
  # Avoided overflow error on large-ish products in denominator.
  #
  # actual = vector of true outcomes, 1 = Positive, 0 = Negative
  # predicted = vector of predicted outcomes, 1 = Positive, 0 = Negative
  # function returns MCC
  
  TP <- sum(actual == 1 & predicted == 1)
  TN <- sum(actual == 0 & predicted == 0)
  FP <- sum(actual == 0 & predicted == 1)
  FN <- sum(actual == 1 & predicted == 0)
  #TP;TN;FP;FN # for debugging
  sum1 <- TP+FP; sum2 <-TP+FN ; sum3 <-TN+FP ; sum4 <- TN+FN;
  denom <- as.double(sum1)*sum2*sum3*sum4 # as.double to avoid overflow error on large products
  if (any(sum1==0, sum2==0, sum3==0, sum4==0)) {
    denom <- 1
  }
  mcc <- ((TP*TN)-(FP*FN)) / sqrt(denom)
  
  cat("\nMCC = ", (mcc), "\n\n", sep="")
  
  return(mcc)
}
