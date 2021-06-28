library(base)
library(caret)
library(e1071)
library(modules)
library(rstudioapi)
library(stats)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

train_test_prop <- 0.75

data <- modules::use('data')$get_data$get_data()

target_column <- 47
target_column_name <- base::colnames(data)[target_column]
formula <- stats::as.formula(base::paste(target_column_name, ' ~ .'))

data$url <- NULL

index <-
  base::sample(1:base::nrow(data), size = base::nrow(data) * train_test_prop)

train <- data[index, ]
test <- data[-index, ]

knn_model <- caret::train(
  formula,
  data = train,
  method = 'knn',
  trControl = caret::trainControl('cv', number = 10),
  preProcess = base::c('center', 'scale'),
  tuneLength = 20
)

base::plot(knn_model)
base::print(knn_model$bestTune)

predicted <- knn_model %>% stats::predict(test)

cm <-
  caret::confusionMatrix(predicted, base::as.factor(test$status))
base::print(cm$table)
base::print(cm$overall['Accuracy'])

base::print(base::Sys.time() - start)
base::rm(start, index)

predict_using_knn <- function(file = 'scripts/output.csv') {
  output <- utils::read.csv(
    file,
    header = TRUE,
    sep = ',',
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )

  urls <- output$url
  output$url <- NULL

  results <- knn_model %>% stats::predict(output)

  base::print(base::data.frame(url = urls, status = results))
}
