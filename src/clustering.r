library(base)
library(caret)
library(cluster)
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

data <- modules::use('data')$get_data$get_data()

target_column <- 47

data_dm <-
  dummies::dummy.data.frame(data = data[, 2:46], sep = '_')

data_norm <-
  base::as.data.frame(RSNNS::normalizeData(data_dm, type = '0_1'))

base::names(data_norm) <- base::names(data)[2:46]

data_norm <- base::t(data_norm)

distance <- stats::dist(data_norm, method = 'euclidean')

base::plot(factoextra::fviz_dist(
  distance,
  gradient = base::list(low = '#00AFBB',
                        mid = 'white', high = '#FC4E07')
))

methods <- base::c('average', 'single', 'complete', 'ward')
base::names(methods) <- methods

hc_methods <-
  purrr::map_dbl(methods, function(x) {
    cluster::agnes(data_norm, method = x)$ac
  })
base::print(hc_methods)

best_ac_method <- base::names(base::which.max(hc_methods))

hc_agl <- cluster::agnes(data_norm, method = best_ac_method)
cluster::pltree(
  hc_agl,
  cex = 0.6,
  hang = -1,
  main = base::paste('Dendrogram of agnes - ', best_ac_method)
)
stats::rect.hclust(hc_agl, k = 2, border = base::c('#00AFBB', '#FC4E07'))

data_norm <- base::t(data_norm)

methods <- base::c('average', 'single', 'complete', 'ward')
base::names(methods) <- methods

hc_methods <-
  purrr::map_dbl(methods, function(x) {
    cluster::agnes(data_norm, method = x)$ac
  })
base::print(hc_methods)

best_ac_method <- base::names(base::which.max(hc_methods))

hc_agl <- cluster::agnes(data_norm, method = best_ac_method)
hc_div <- cluster::diana(data_norm)

base::print(hc_div$dc)

eval_hc <- function(hc) {
  cut <- dendextend::cutree(stats::as.hclust(hc), k = 2)

  assoc <-
    base::as.data.frame(data_norm) %>% dplyr::mutate(cluster = cut)
  cm <-
    caret::confusionMatrix(base::as.factor(labels),
                           base::as.factor(assoc$cluster))
  base::print(cm$table)
  base::print(cm$overall['Accuracy'])

  base::plot(stats::as.hclust(hc), cex = 0.6)
  stats::rect.hclust(stats::as.hclust(hc), k = 3, border = 2:5)
}

labels <-
  base::ifelse(data[, target_column] == 'legitimate', 1, 2)
eval_hc(hc_agl)

labels <-
  base::ifelse(data[, target_column] == 'legitimate', 2, 1)
eval_hc(hc_div)

base::print(base::Sys.time() - start)
base::rm(start)
