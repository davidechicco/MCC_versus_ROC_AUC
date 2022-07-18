options(stringsAsFactors = FALSE)
# library("clusterSim")

set.seed(11)

list.of.packages <- c("easypackages", "dplyr", "PRROC", "e1071", "Metrics", "MLmetrics", "rcompanion", "irr" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("utils.r")
source("confusion_matrix_rates.r")

# NUM_VALUES <- 10
# gold_standard_binary_values <- sample(0:1, NUM_VALUES, replace=TRUE)

# this_label <- "negatively imbalanced"
# this_label <- "balanced"
this_label <- "positively imbalanced"


gold_standard_binary_values <- c()
letter_title <- ""


# positively_imbalanced
if(this_label == "negatively imbalanced") { 
    gold_standard_binary_values <- c(0,0,0,1,1,1,1,1,1,1) 
    letter_title <- "(a)"
    }

    # balanced
if(this_label == "balanced") { 
    gold_standard_binary_values <- c(0,0,0,0,0,1,1,1,1,1) 
    letter_title <- "(b)"
    }
    
# negatively imbalanced
if(this_label == "positively imbalanced") { 
    gold_standard_binary_values <- c(0,0,0,0,0,0,0,1,1,1) 
    letter_title <- "(c)"
    }



# cat("MCC = ", these_rates$MCC, "\t") 
# cat("ROC AUC = ", these_rates$ROC_AUC, "\n") 

norm_mcc_vector <- c()
roc_auc_vector <- c()

  LOOPS <- 10000

  for(i in c(1:LOOPS)){
  
    i_perc <- i * 100 / LOOPS
   #  if(floor(i_perc) %in% c(1:10) )    cat(dec_two(i_perc), "% ", sep="")

    predicted_real_values <-  runif(gold_standard_binary_values %>% length(), min=0, max=1)
    these_rates <- as.data.frame(confusion_matrix_rates(gold_standard_binary_values, predicted_real_values, "TEST", FALSE))
    norm_mcc_vector[i] <- (these_rates$MCC + 1) /2
    roc_auc_vector[i] <- these_rates$ROC_AUC
    
    # add noise
    noise_segment <- 0
    norm_mcc_vector[i] <- norm_mcc_vector[i] + runif(1, min=-noise_segment, max=noise_segment)
    roc_auc_vector[i] <- roc_auc_vector[i]+ runif(1, min=-noise_segment, max=noise_segment)

  }
  cat("\n")
  
 random_number <- sample(0:1000, 1, replace=TRUE)
  this_dataframe <- data.frame(norm_mcc_vector, roc_auc_vector)
  colnames(this_dataframe) <- c("normMCC", "ROCAUC")
  this_plot <- ggplot(this_dataframe, aes(x=normMCC, y=ROCAUC)) + geom_point() +  geom_smooth(method=lm) +  geom_jitter() + ggtitle(paste0(letter_title, " ", this_label, " ground truth")) + theme(plot.title = element_text(hjust = 0.5))
  this_plot

  thisPdfFile <- paste0("../results/MCC_versus_ROC_AUC_", this_label,"_random", random_number, ".pdf")
  thisPdfFile <- gsub(" ", "_", thisPdfFile)	
  ggsave(this_plot, file=thisPdfFile)
  cat("Saved ", thisPdfFile, "\n", sep="")

  thisPngFile <- paste0("../results/MCC_versus_ROC_AUC_", this_label,"_random", random_number, ".png")
  thisPngFile <- gsub(" ", "_", thisPngFile)	
  ggsave(this_plot, file=thisPngFile)
  cat("Saved ", thisPngFile, "\n", sep="")
