library(base)
library(caret)
library(e1071)
library(grDevices)
library(graphics)
library(kohonen)
library(modules)
library(rstudioapi)
library(stats)
library(tidyverse)
library(utils)


base::setwd(base::dirname(rstudioapi::getActiveDocumentContext()$path))
start <- base::Sys.time()

base::set.seed(0xACDC)

data <- modules::use('data')$get_data$get_data()

xdim <- 3
ydim <- 3

radius <- 2

groups <- 2

target_column <- 47
target_column_name <- base::colnames(data)[target_column]
train_test_prop <- 0.75

data$url <- NULL

data[, target_column_name] <-
  base::as.factor(data[, target_column_name])

labels <- base::unique(data[, target_column_name])

data_norm <- base::scale(data[, 1:45])

index <-
  base::sample(base::nrow(data_norm),
               base::round(train_test_prop * base::nrow(data_norm)))
train <- data_norm[index, ]
test <- data_norm[-index, ]
train_label <- data[index, target_column_name]
test_label <- data[-index, target_column_name]

som_grid <-
  kohonen::somgrid(xdim = xdim, ydim = ydim, topo = 'hexagonal')

som_not_supervised <- function(data_n) {
  data_som <- kohonen::som(
    data_n,
    grid = som_grid,
    alpha = base::c(0.05, 0.01),
    radius = radius
  )

  coolBlueHotRed <- function(n, alpha = 1) {
    grDevices::rainbow(n, end = 4 / 6, alpha = alpha)[n:1]
  }

  graphics::par(mfrow = base::c(4, 6))

  for (j in 1:24) {
    base::plot(
      data_som,
      type = 'property',
      property = data_som$codes[[1]][, j],
      palette.name = coolBlueHotRed,
      main = base::colnames(data_n)[j],
      cex = 0.5
    )
  }

  graphics::par(mfrow = base::c(4, 6))

  for (j in 25:(base::ncol(data_n))) {
    base::plot(
      data_som,
      type = 'property',
      property = data_som$codes[[1]][, j],
      palette.name = coolBlueHotRed,
      main = base::colnames(data_n)[j],
      cex = 0.5
    )
  }

  return (data_som)
}

data_som <- som_not_supervised(data_norm)

data_norm <- data_norm[, -base::c(19, 21, 22, 23)]

data_som <- som_not_supervised(data_norm)

graphics::par(mfrow = base::c(1, 1))

data_hc <-
  stats::cutree(stats::hclust(stats::dist(data_som$codes[[1]])), groups)

base::plot(
  data_som,
  type = 'codes',
  bgcol = grDevices::rainbow(groups + 1)[data_hc + 1],
  main = 'Clustering the patterns discovered'
)
kohonen::add.cluster.boundaries(data_som, data_hc)

kohmap <- kohonen::xyf(
  train,
  train_label,
  grid = som_grid,
  alpha = base::c(0.05, 0.01),
  radius = radius
)

graphics::par(mfrow = base::c(1, 1))

base::plot(
  kohmap,
  type = 'mapping',
  labels = base::as.numeric(train_label),
  col = base::as.numeric(train_label) + 1,
  pch = 1,
  main = 'Map of classes'
)

kohmap_predict_train <-
  stats::predict(kohmap, newdata = train, whatmap = 1)

cm_train <-
  caret::confusionMatrix(train_label, kohmap_predict_train$predictions[[2]])
base::print('Train:')
base::print(cm_train$table)
base::print(cm_train$overall['Accuracy'])

kohmap_predict_test <-
  stats::predict(kohmap, newdata = test, whatmap = 1)

cm_test <-
  caret::confusionMatrix(test_label, kohmap_predict_test$predictions[[2]])
base::print('Test:')
base::print(cm_test$table)
base::print(cm_test$overall['Accuracy'])

base::print(base::Sys.time() - start)
base::rm(start, index)

predict_using_som <- function(file = 'scripts/output.csv') {
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
    stats::predict(kohmap,
                   newdata = base::as.matrix(output),
                   whatmap = 1)$predictions[[2]]

  results <-
    base::apply(results, 1, function(r) {
      return (base::colnames(results)[base::which.max(r)])
    })

  base::print(base::data.frame(url = urls, status = results))
}
