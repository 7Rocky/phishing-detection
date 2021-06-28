library(base)
library(modules)
library(rstudioapi)
library(stats)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

do_pca <- function(data) {
  matrix <- base::as.matrix(data)

  pca <- stats::prcomp(matrix, scale. = TRUE)

  percent <- pca$sdev ^ 2 / base::sum(pca$sdev ^ 2) * 100

  graphics::barplot(percent,
                    main = 'Scree Plot',
                    xlab = 'Principal Component',
                    ylab = 'Percent Variation')
}

data <- modules::use('data')$get_data$get_all_data()

data$url <- NULL
data$status <- NULL

to_remove <- base::c()

for (i in 1:base::ncol(data)) {
  if (base::length(base::unique(data[, i])) == 1) {
    to_remove <- base::append(to_remove, i)
  }
}

data[, to_remove] <- NULL

do_pca(data)

clean_data <- modules::use('data')$get_data$get_data()

clean_data$url <- NULL
clean_data$status <- NULL

do_pca(clean_data)

base::print(base::Sys.time() - start)
base::rm(i, to_remove, start)
