library(base)
library(C50)
library(caret)
library(e1071)
library(modules)
library(rpart)
library(rpart.plot)
library(rstudioapi)
library(stats)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

train_test_prop <- 0.75

data <- modules::use('data')$get_data$get_data()
data$url <- NULL

data$status <- base::as.factor(data$status)

formula <- stats::as.formula('status ~ .')

index <-
  base::sample(base::nrow(data),
               base::round(train_test_prop * base::nrow(data)))

train <- data[index, ]
test <- data[-index, ]

train_label <- base::as.factor(train$status)
test_label <- base::as.factor(test$status)

tree_result <-
  rpart::rpart(formula, train, method = 'class')

plot_tree <- function(tree) {
  rpart.plot::rpart.plot(
    tree,
    type = 1,
    tweak = 1.1,
    box.palette = base::c('#00AFBB', '#FC4E07')
  )
}

eval_tree <- function(tree, set, label) {
  predicted <- stats::predict(tree, newdata = set, type = 'class')
  cm_train <- caret::confusionMatrix(predicted, label)
  base::print(cm_train$table)
  base::print(cm_train$overall['Accuracy'])
}

plot_tree(tree_result)

base::print('Train (rpart):')
eval_tree(tree_result, train, train_label)

base::print('Test (rpart):')
eval_tree(tree_result, test, test_label)

rpart::plotcp(tree_result, col = 'red', upper = 'splits')

tree_pruned <- rpart::prune(tree_result, cp = 0.014)

plot_tree(tree_pruned)

base::print('Train (rpart, pruned):')
eval_tree(tree_pruned, train, train_label)

base::print('Test (rpart, pruned):')
eval_tree(tree_pruned, test, test_label)

tree_result_C50 <- C50::C5.0(
  formula,
  data = train,
  control = C50::C5.0Control(CF = 0.25)
)

base::print('Train (C5.0):')
eval_tree(tree_result_C50, train, train_label)

base::print('Test (C5.0):')
eval_tree(tree_result_C50, test, test_label)

base::print(base::Sys.time() - start)
base::rm(start, index)

predict_using_rpart <- function(file = 'scripts/output.csv') {
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
    stats::predict(tree_pruned, newdata = output, type = 'class')

  base::print(base::data.frame(url = urls, status = results))
}

predict_using_C50 <- function(file = 'scripts/output.csv') {
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
    stats::predict(tree_result_C50, newdata = output, type = 'class')

  base::print(base::data.frame(url = urls, status = results))
}
