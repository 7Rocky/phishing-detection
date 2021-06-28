library(base)
library(caret)
library(dummies)
library(e1071)
library(factoextra)
library(modules)
library(RSNNS)
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

data_dm <- dummies::dummy.data.frame(data = data[, 2:46], sep = '_')

data_norm <-
  base::as.data.frame(RSNNS::normalizeData(data_dm, type = '0_1'))

base::names(data_norm) <- base::names(data)[2:46]

index <-
  base::sample(base::nrow(data_norm),
               base::round(train_test_prop * base::nrow(data_norm)))

train <- data_norm[index, ]
test <- data_norm[-index, ]

train_label <- data[index, target_column]
test_label <- data[-index, target_column]

base::plot(factoextra::fviz_nbclust(train, kmeans, 'wss', k.max = 15))
base::plot(factoextra::fviz_nbclust(train, kmeans, 'silhouette', k.max = 15))

cluster <- stats::kmeans(train, centers = 2, nstart = 20)

base::print(cluster$size)

distEuclidean <- function(x, y) {
  z <- base::matrix(0, nrow = base::nrow(x), ncol = base::nrow(y))

  for (k in 1:base::nrow(y)) {
    z[, k] <- base::sqrt(base::colSums((base::t(x) - y[k, ]) ^ 2))
  }

  return(z)
}

clusters <- function(x, centers) {
  Distances <- distEuclidean(x, centers)
  Clusters <- base::apply(Distances, 1, which.min)

  return(Clusters)
}

do_kmeans <- function(set, label) {
  prediction <- clusters(set, cluster[['centers']])

  label <-
    base::as.factor(base::ifelse(label == 'legitimate', 2, 1))

  cm <- caret::confusionMatrix(label, base::as.factor(prediction))
  base::print(cm$table)
  base::print(cm$overall['Accuracy'])
}

base::print('Train:')
do_kmeans(train, train_label)

base::print('Test:')
do_kmeans(test, test_label)

base::print(base::Sys.time() - start)
base::rm(start, index)

predict_using_kmeans <- function(file = 'scripts/output.csv') {
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
    base::ifelse(clusters(output, cluster[['centers']]) == 2,
                 'legitimate',
                 'phishing')

  base::print(base::data.frame(url = urls, status = results))
}
