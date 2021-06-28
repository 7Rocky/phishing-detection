library(base)
library(caret)
library(dplyr)
library(e1071)
library(ggpubr)
library(modules)
library(randomForest)
library(rstudioapi)
library(stats)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

data <- modules::use('data')$get_data$get_data()

formula <- stats::formula('status ~ .')

data$url <- NULL
data$status <- base::as.factor(data$status)

base::set.seed(0xACDC)
rf_model <-
  randomForest::randomForest(formula,
                             data,
                             mtry = base::length(base::colnames(data)) - 1)
base::print(rf_model)

tuning_rf_mtry <- function(set) {
  max_predictors <- base::ncol(set) - 1
  n_predictors <- base::rep(NA, max_predictors)
  oob_err_rate <- base::rep(NA, max_predictors)

  for (i in 1:max_predictors) {
    base::set.seed(0xACDC)
    rf <- randomForest::randomForest(formula, set, mtry = i)
    n_predictors[i] <- i
    oob_err_rate[i] <- utils::tail(rf$err.rate[, 1], n = 1)
  }

  results <- dplyr::tibble(n_predictors, oob_err_rate)

  return (results)
}

hyp_mtry <- tuning_rf_mtry(data)

best_mtry_data <-
  hyp_mtry %>% dplyr::arrange(oob_err_rate) %>% utils::head(1)

best_mtry <- best_mtry_data$n_predictors

base::plot(
  ggplot2::ggplot(data = hyp_mtry,
                  ggplot2::aes(x = n_predictors, y = oob_err_rate)) +
    ggplot2::scale_x_continuous(breaks = hyp_mtry$n_predictors) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = best_mtry_data, color = 'red') +
    ggplot2::labs(title = 'Evolution of the OOB error vs Number of predictors',
                  x = 'Number of predictors (m)') +
    ggplot2::theme_bw()
)
base::rm(hyp_mtry, best_mtry_data)

base::set.seed(0xACDC)
rf_model <-
  randomForest::randomForest(formula, data, mtry = best_mtry)

tuning_rf_nodesize <- function(set) {
  size <- 1:20
  oob_err_rate <- base::rep(NA, base::length(size))

  for (i in size) {
    base::set.seed(0xACDC)
    rf <-
      randomForest::randomForest(formula, set,
                                 mtry = best_mtry,
                                 nodesize = i)
    oob_err_rate[i] <- utils::tail(rf$err.rate[, 1], n = 1)
  }

  results <- dplyr::tibble(size, oob_err_rate)

  return (results)
}

hyp_nodesize <- tuning_rf_nodesize(data)

best_nodesize_data <-
  hyp_nodesize %>% dplyr::arrange(oob_err_rate) %>% utils::head(1)

best_nodesize <- best_nodesize_data$size

base::plot(
  ggplot2::ggplot(data = hyp_nodesize,
                  ggplot2::aes(x = size, y = oob_err_rate)) +
    ggplot2::scale_x_continuous(breaks = hyp_nodesize$size) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = best_nodesize_data, color = 'red') +
    ggplot2::labs(title = 'Evolution of the OOB error vs Node size',
                  x = 'Number of observations in terminal nodes') +
    ggplot2::theme_bw()
)
base::rm(hyp_nodesize, best_nodesize_data)

base::set.seed(0xACDC)
rf_model <- randomForest::randomForest(formula,
                                       data,
                                       mtry = best_mtry,
                                       nodesize = best_nodesize)
oob_error_rate <-
  base::data.frame(
    oob_error_rate = rf_model$err.rate[, 1],
    trees = base::seq_along(rf_model$err.rate[, 1])
  )

base::plot(
  ggplot2::ggplot(data = oob_error_rate,
                  ggplot2::aes(x = trees, y = oob_error_rate)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = 'Evolution of the OOB error vs Number of trees',
                  x = 'Number of trees') +
    ggplot2::theme_bw()
)

best_ntree <- base::which.min(oob_error_rate$oob_error_rate)

base::set.seed(0xACDC)
rf_model <- randomForest::randomForest(
  formula,
  data,
  mtry = best_mtry,
  ntree = best_ntree,
  nodesize = best_nodesize,
  importance = TRUE
)

base::print(rf_model)

importance_pred <-
  base::as.data.frame(randomForest::importance(rf_model, scale = TRUE))
importance_pred <- tibble::rownames_to_column(importance_pred,
                                              var = 'variable')

p1 <- ggplot2::ggplot(
  data = importance_pred,
  ggplot2::aes(
    x = stats::reorder(variable, MeanDecreaseAccuracy),
    y = MeanDecreaseAccuracy,
    fill = MeanDecreaseAccuracy
  )
) +
  ggplot2::labs(x = 'variable', title = 'Accuracy Reduction') +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom')

p2 <- ggplot2::ggplot(data = importance_pred,
                      ggplot2::aes(
                        x = stats::reorder(variable, MeanDecreaseGini),
                        y = MeanDecreaseGini,
                        fill = MeanDecreaseGini
                      )) +
  ggplot2::labs(x = 'variable', title = 'Purity Reduction (Gini)') +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom')

base::plot(ggpubr::ggarrange(p1, p2))

base::print(base::Sys.time() - start)
base::rm(p1, p2, oob_error_rate, importance_pred, start)

predict_using_randomforest <-
  function(file = 'scripts/output.csv') {
    output <- utils::read.csv(
      file,
      header = TRUE,
      sep = ',',
      stringsAsFactors = FALSE,
      strip.white = TRUE
    )

    urls <- output$url
    output$url <- NULL

    results <-
      stats::predict(rf_model, newdata = output, type = 'class')

    base::print(base::data.frame(url = urls, status = results))
  }
