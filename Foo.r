split_data <- function(dataset, ratio = 0.75) {
    data_size <- nrow(dataset)
    sample <- sample.int(n = data_size, size = floor(ratio * data_size))
    list(train = dataset[sample, ], test = dataset[-sample, ])
}

numerate_classes <- function(dataset) {
    for (i in seq_len(ncol(dataset))) {
        if (is.character(dataset[, i])) {
            dataset[, i] <- as.numeric(as.factor(dataset[, i]))
        }
    }
    dataset
}

fill_missing <- function(dataset, na_character = "?") {
    for (i in seq_len(ncol(dataset))) {
        if (is.numeric(dataset[, i]) || is.logical(dataset[, i]))
          dataset[, i][is.na(dataset[, i])] <- mean(dataset[, i], na.rm = TRUE)
    }
    dataset
}

cleve <- read.table("Datasets/cleve.data", na.strings = c("?"))
iris <- read.csv("Datasets/iris.data");

cleve <- fill_missing(cleve)

plot(iris[, 1],
     iris[, 2],
     xlab = "sepal length",
     ylab = "sepal width")

hist(iris[, 3], xlab = "petal width", col = "green", border ="red")

plot(cleve[, 1],
    cleve[, 4],
    xlab = "Age",
    ylab = "Cholesterol")

hist(cleve[, 1], xlab = "Age")