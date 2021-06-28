library(base)
library(caret)
library(e1071)
library(graphics)
library(modules)
library(neuralnet)
library(rsample)
library(rstudioapi)
library(stats)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

data <- modules::use('data')$get_data$get_data()

target_column <- 47
target_column_name <- base::colnames(data)[target_column]
train_test_prop <- 0.75

formula <- stats::as.formula(base::paste(target_column_name, ' ~ .'))

data$url <- NULL
data[, target_column_name] <-
  base::ifelse(data[, target_column_name] == 'legitimate', 1, 2)

data_split <- rsample::initial_split(data, prop = train_test_prop)
data_train <- rsample::training(data_split)
data_test <- rsample::testing(data_split)

target_data <- data[, target_column_name]
target_train <- data_train[, target_column_name]
target_test <- data_test[, target_column_name]

maxs <- base::apply(data, 2, max)
mins <- base::apply(data, 2, min)

scaled <-
  base::as.data.frame(base::scale(data, center = mins, scale = maxs - mins))

data_train_scaled <- scaled[data_split$in_id, ]
data_test_scaled <- scaled[-data_split$in_id, ]

nn <- neuralnet::neuralnet(
  formula,
  data = data_train_scaled,
  hidden = base::c(6, 3),
  linear.output = TRUE
)

base::plot(nn, rep = 'best')

best_rep <- base::which.min(nn$result.matrix[1, ])

plot_results <- function(target, ds_pr.nn, title) {
  graphics::plot(
    target,
    type = 'p',
    col = 'red',
    xlab = 'Sample',
    ylab = 'Status',
    main = base::paste(title, ': Real (red) - Predicted (blue)')
  )
  graphics::lines(ds_pr.nn, type = 'p', col = 'blue')
}

do_mlp <- function(fdata, target, ds_pr.nn) {
  error <- target - ds_pr.nn

  R2 <-
    1 - base::sum(error ^ 2) /
    base::sum((target - base::mean(target_data)) ^ 2)
  R2_adjust <-
    (1 - (1 - R2) * (base::nrow(fdata) - 1) /
       (base::nrow(fdata) - base::ncol(fdata) - 1))

  target <- base::ifelse(target == 1, 'legitimate', 'phishing')

  predicted <-
    base::ifelse(base::round(ds_pr.nn) == 1, 'legitimate', 'phishing')

  cm <-
    caret::confusionMatrix(base::as.factor(predicted), base::as.factor(target))

  base::print(base::paste('R2:', R2))
  base::print(base::paste('R2 adjust:', R2_adjust))
  base::print(cm$table)
  base::print(cm$overall['Accuracy'])
}

pr.nn_training <-
  stats::predict(nn, data_train_scaled, rep = best_rep, all.units = FALSE)

ds_pr.nn_train <-
  pr.nn_training *
  (maxs[target_column_name] - mins[target_column_name]) +
  mins[target_column_name]

pr.nn_test <-
  stats::predict(nn, data_test_scaled, rep = best_rep, all.units = FALSE)

ds_pr.nn_test <-
  pr.nn_test *
  (maxs[target_column_name] - mins[target_column_name]) +
  mins[target_column_name]

base::print('Train:')
do_mlp(data_train, target_train, ds_pr.nn_train)

base::print('Test:')
plot_results(target_test, ds_pr.nn_test, 'Test')
do_mlp(data_test, target_test, ds_pr.nn_test)

base::print(base::Sys.time() - start)
base::rm(start)

predict_using_mlp <- function(file = 'scripts/output.csv') {
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
    base::ifelse(base::round(
      stats::predict(nn, output, rep = best_rep, all.units = FALSE) *
        (maxs[target_column_name] - mins[target_column_name]) +
        mins[target_column_name]
    ) == 1,
    'legitimate',
    'phishing')

  base::print(base::data.frame(url = urls, status = results))
}
