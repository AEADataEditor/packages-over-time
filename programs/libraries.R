#' ---
#' local libraries

libraries <- c("rcrossref","purrr","dplyr","stringr","tidyr","jsonlite","httr","tidylog","readr","ggplot2","scales","ggpattern")

results <- sapply(as.list(libraries), pkgTest)
cbind(libraries,results)
