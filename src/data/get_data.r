modules::export('get_all_data')
modules::export('get_data')

get_all_data <- function() {
  # https://www.kaggle.com/manishkc06/web-page-phishing-detection
  # https://data.mendeley.com/datasets/c2gw7fy2j4/1
  all_data <- utils::read.csv(
    'data/phishing_data.csv',
    header = TRUE,
    sep = ',',
    stringsAsFactors = FALSE
  )

  return (clean_data(all_data))
}

get_data <- function() {
  data <- clean_data(get_all_data())
  percent <- 0.9

  for (t in 1:10) {
    data <- remove_columns(remove_rows(data, percent))
  }

  return (data)
}

clean_data <- function(data) {
  data <- stats::na.omit(data)

  data[data == 'zero' | data == 'Zero'] <- '0'
  data[data == 'one' | data == 'One'] <- '1'

  classes <- base::sapply(data[, -base::c(1, 89)], class)

  for (i in base::which(classes == 'character')) {
    data[, i + 1] <- base::as.numeric(data[, i + 1])
  }

  return (data)
}

remove_rows <- function(data, percent) {
  for (i in 1:base::length(data)) {
    relative_frequencies <- base::table(data[, i]) / base::nrow(data)

    if (base::length(base::which(relative_frequencies > percent)) > 0) {
      df <- base::as.data.frame(relative_frequencies)
      df$Freq <- base::as.numeric(df$Freq)
      most_frequent_value <-
        df$Var1[base::which(df$Freq > percent)[1]]

      if (base::length(base::which(data[, i] != most_frequent_value)) > 0) {
        data <- data[-base::which(data[, i] != most_frequent_value),]
      }
    }
  }

  return (data)
}

remove_columns <- function(old_data) {
  data <- old_data

  for (i in 1:(base::length(old_data) - 1)) {
    relative_frequencies <-
      base::table(old_data[, i]) / base::nrow(old_data)

    if (base::length(base::which(relative_frequencies == 1)) > 0) {
      data[base::colnames(old_data)[i]] <- NULL
    }
  }

  return (data)
}
