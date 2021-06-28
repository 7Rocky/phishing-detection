library(arules)
library(arulesViz)
library(base)
library(methods)
library(modules)
library(rstudioapi)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

data_or <- modules::use('data')$get_data$get_data()

to_remove <- base::c()

for (i in 1:base::ncol(data_or)) {
  if (base::length(base::unique(data_or[, i])) > 2) {
    to_remove <- base::append(to_remove, i)
  }
}

data <- data_or[, -to_remove]

for (i in 1:base::ncol(data)) {
  data[, i] <- base::as.factor(data[, i])
}
base::rm(i, to_remove)

data = methods::as(data, 'transactions')

rules <-
  arules::apriori(data,
                  parameter = base::list(
                    supp = 0.01,
                    conf = 0.5,
                    maxtime = 0
                  ))

rules <-
  rules[-base::which(Matrix::colSums(arules::is.subset(rules, rules)) > 1)]

subrules <- rules[arules::quality(rules)$coverage < 1]

base::plot(subrules, method = 'graph')

base::print(base::Sys.time() - start)
base::rm(start)
