

options(stringsAsFactors = FALSE)
# library("clusterSim")

set.seed(11)

list.of.packages <- c("easypackages", "dplyr", "PRROC", "plotROC", "pROC" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)



# https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html

set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)


test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)


thisPlot <- ggplot(test, aes(d = D, m = M1)) + geom_roc(n.cuts = 50, labels = FALSE)
thisPlot

threshold <- 0.5
cat("cut-off threshold = ", threshold, "\n", sep="")
source("confusion_matrix_rates.r")
confusion_matrix_rates(test$D, test$M1, "test", TRUE, threshold)

calc_auc(thisPlot)$AUC

random_number <- sample(0:1000, 1, replace=TRUE)

plotFile <- paste0("ROC_curve_example_random",random_number)

SAVE_PLOT_FILE <- FALSE

if(SAVE_PLOT_FILE == TRUE) {
    thisPdfFile <- paste0("../results/", plotFile, ".pdf")
    ggsave(this_plot, file=thisPdfFile)
    cat("Saved ", thisPdfFile, "\n", sep="")

    thisPngFile <- paste0("../results/", plotFile, ".png")
    ggsave(this_plot, file=thisPngFile)
    cat("Saved ", thisPngFile, "\n", sep="")
}
