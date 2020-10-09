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

    cf_output <- confusion_matrix_rates(ground_truth, predictor, " ")
    predictor <- c(predictor[-1], predictor[1])
}

