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
# this_label <- "positively imbalanced"

these_labels <- c("negatively imbalanced", "balanced", "positively imbalanced")

MCC_versus_ROC_AUC_plot <- FALSE
TPR_versus_ROC_AUC_plot <- FALSE
TNR_versus_ROC_AUC_plot <- FALSE
PPV_versus_ROC_AUC_plot <- TRUE
NPV_versus_ROC_AUC_plot <- FALSE

gold_standard_binary_values <- c()
letter_title <- ""

for(this_label in these_labels){

    # positively_imbalanced
    if(this_label == "negatively imbalanced") { 
        gold_standard_binary_values <- c(0,0,0,1,1,1,1,1,1,1) 
        letter_title <- "(i)"
        }

        # balanced
    if(this_label == "balanced") { 
        gold_standard_binary_values <- c(0,0,0,0,0,1,1,1,1,1) 
        letter_title <- "(j)"
        }
        
    # negatively imbalanced
    if(this_label == "positively imbalanced") { 
        gold_standard_binary_values <- c(0,0,0,0,0,0,0,1,1,1) 
        letter_title <- "(k)"
        }



    # cat("MCC = ", these_rates$MCC, "\t") 
    # cat("ROC AUC = ", these_rates$ROC_AUC, "\n") 

    norm_mcc_vector <- c()
    roc_auc_vector <- c()
    specificity_vector <- c()
    sensitivity_vector <- c()
    precision_vector<- c()
    NPV_vector <- c()
    
    threshold <- 0.5
    
    LOOPS <- 10000

    for(i in c(1:LOOPS)){
    
        i_perc <- i * 100 / LOOPS
    #  if(floor(i_perc) %in% c(1:10) )    cat(dec_two(i_perc), "% ", sep="")

        predicted_real_values <-  runif(gold_standard_binary_values %>% length(), min=0, max=1)
        these_rates <- as.data.frame(confusion_matrix_rates(gold_standard_binary_values, predicted_real_values, "TEST", FALSE, threshold))
        norm_mcc_vector[i] <- (these_rates$MCC + 1) /2
        roc_auc_vector[i] <- these_rates$ROC_AUC
        
        specificity_vector[i] <- these_rates$TP_rate
        sensitivity_vector[i] <- these_rates$TN_rate        
        precision_vector[i] <- these_rates$PPV
        NPV_vector[i] <- these_rates$NPV
        
        # add noise
        noise_segment <- 0
        norm_mcc_vector[i] <- norm_mcc_vector[i] + runif(1, min=-noise_segment, max=noise_segment)
        roc_auc_vector[i] <- roc_auc_vector[i]+ runif(1, min=-noise_segment, max=noise_segment)
        specificity_vector[i] <- specificity_vector[i] + runif(1, min=-noise_segment, max=noise_segment)
        sensitivity_vector[i] <- sensitivity_vector[i]+ runif(1, min=-noise_segment, max=noise_segment)
        precision_vector[i] <- precision_vector[i] + runif(1, min=-noise_segment, max=noise_segment)
        NPV_vector[i] <- NPV_vector[i]+ runif(1, min=-noise_segment, max=noise_segment)

    }
    cat("\n")
    
    thisTextSize <- 20
    
    random_number <- sample(0:1000, 1, replace=TRUE)
    random_numberB <- sample(0:1000, 1, replace=TRUE)
    this_dataframe <- data.frame(norm_mcc_vector, roc_auc_vector,  sensitivity_vector, specificity_vector, precision_vector, NPV_vector)
    colnames(this_dataframe) <- c("normMCC", "ROCAUC", "TPR", "TNR", "PPV", "NPV")
    
    fileName <- ""  
    
    if(MCC_versus_ROC_AUC_plot == TRUE) {
        fileName <- "MCC_versus_ROC_AUC_plot"
        this_plot <- ggplot(this_dataframe, aes(x=normMCC, y=ROCAUC)) + geom_point() +  geom_smooth(method=lm) +  geom_jitter() + ggtitle(paste0(letter_title, " ", this_label, " ground truth")) + theme(plot.title = element_text(hjust = 0.5), text=element_text(size=thisTextSize)) + xlim(0,1) + ylim(0,1)
        this_plot
    }

    if(TPR_versus_ROC_AUC_plot == TRUE) {
        fileName <- "TPR_versus_ROC_AUC_plot"
        this_plot <- ggplot(this_dataframe, aes(x=TPR, y=ROCAUC)) + geom_point() +  geom_smooth(method=lm) +  geom_jitter() + ggtitle(paste0(letter_title, " ", this_label, " ground truth")) + theme(plot.title = element_text(hjust = 0.5), text=element_text(size=thisTextSize)) + xlim(0,1) + ylim(0,1)
        this_plot
    }
    
    if(TNR_versus_ROC_AUC_plot == TRUE) {
        fileName <- "TNR_versus_ROC_AUC_plot"
        this_plot <- ggplot(this_dataframe, aes(x=TNR, y=ROCAUC)) + geom_point() +  geom_smooth(method=lm) +  geom_jitter() + ggtitle(paste0(letter_title, " ", this_label, " ground truth")) + theme(plot.title = element_text(hjust = 0.5), text=element_text(size=thisTextSize)) + xlim(0,1) + ylim(0,1)
        this_plot
    }
    
    if(PPV_versus_ROC_AUC_plot == TRUE) {
        fileName <- "PPV_versus_ROC_AUC_plot"
        this_plot <- ggplot(this_dataframe, aes(x=PPV, y=ROCAUC)) + geom_point() +  geom_smooth(method=lm) +  geom_jitter() + ggtitle(paste0(letter_title, " ", this_label, " ground truth")) + theme(plot.title = element_text(hjust = 0.5), text=element_text(size=thisTextSize)) + xlim(0,1) + ylim(0,1)
        this_plot
    }    
    
    if(NPV_versus_ROC_AUC_plot == TRUE) {
        fileName <- "NPV_versus_ROC_AUC_plot"
        this_plot <- ggplot(this_dataframe, aes(x=NPV, y=ROCAUC)) + geom_point() +  geom_smooth(method=lm) +  geom_jitter() + ggtitle(paste0(letter_title, " ", this_label, " ground truth")) + theme(plot.title = element_text(hjust = 0.5), text=element_text(size=thisTextSize)) + xlim(0,1) + ylim(0,1)
        this_plot
    }    
    
    #   thisPdfFile <- paste0("../results/MCC_versus_ROC_AUC_", this_label,"_random", random_number,  random_numberB, ".pdf")
    #   thisPdfFile <- gsub(" ", "_", thisPdfFile)	
    #   ggsave(this_plot, file=thisPdfFile)
    #   cat("Saved ", thisPdfFile, "\n", sep="")

    thisPngFile <- paste0("../results/", fileName, "_", this_label,"_random", random_number, random_numberB, ".png")
    thisPngFile <- gsub(" ", "_", thisPngFile)	
    ggsave(this_plot, file=thisPngFile)
    cat("Saved ", thisPngFile, "\n", sep="")
}
