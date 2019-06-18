#Instllation der notwendigen Packages
install.packages("labeling")
install.packages("ggplot2")
install.packages("keras")

#Erstellung eines neuen Ordners inklusive Download des Datenpaketes; anschließendes Endpacken.
dir.create(file.path("~/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "~/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "~/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "~/jena_climate"
)


#Daten öffnen und in die Variable data einlesen, über glimpse ausgeben.
library(tibble)
library(readr)

data_dir <- "~/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)

glimpse(data)

#Ausgabe der Daten als Plot mittels ggplot2
library(ggplot2)
#Ausgabe x Achse aller Datensätze zusammen (420551)
ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()
#Ausgabe x Achse aller Datensätze für degC
ggplot(data[1:1440,], aes(x = 1:1440, y = `T (degC)`)) + geom_line()

#10 % der Daten werden in die Variable data geladen
data <- data.matrix(data[,-1])

#
train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }           
    list(samples, targets)
  }
}


lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

#Trainingsdaten werden über die generator funktion verarbeitet
train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

#Validierungsdaten werden über die generator funktion verarbeitet
val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)

#Testdaten werden über die generator funktion verarbeitet
test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# Wieviele Schritte werden im Validierungschritt durchgeführt
val_steps <- (300000 - 200001 - lookback) / batch_size

# Wieviele Schritte werden im Testschritt durchgeführt
test_steps <- (nrow(data) - 300001 - lookback) / batch_size

mean(abs(preds - targets))

library(keras)
evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()

celsius_mae <- 0.29 * std[[2]]

#Ab hier entstehen Fehler, ich habe schon gegooglet und die Umgebungsvariablen angepasst. 
#gibt es hier Erfahrungen was verändert werden muss?
library(keras)

model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)