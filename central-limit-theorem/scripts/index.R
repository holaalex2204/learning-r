# INPUT SECTION ##########
## General Input Parameters
nSizes <- c(1,3, 5, 7,10, 100)
k <- 500
## Generate population =============================
### Create population with a normal distribution --------------
mockMean <- 1000
mockSD <- 200
populationSize <- 10000
data <- rnorm(10000, mean=mockMean, sd=mockSD)
data <- c(1,1,1,4,3,5,8,8,8)
dataClasses <- seq(min(data), max(data),  (max(data)-min(data))/30)

### You can add an excel input reading if you want use datasets ---------
# import(datasets)
# data <- iris$Petal.Length 

## Display freq of population =========
hist(data, freq = FALSE, main = "Data Distribution", breaks = dataClasses)


# CALC FREQ OF SAMPLING ######
dataMean <- mean(data)
dataSD <- sd(data)
## Iterate with different n size =======
for (n in nSizes) {
  # init loop variables
  i = 0
  tempFrequencies <- vector(mode= "double", length = k)
  # create sampling and calculate mean k times
  repeat {
    tempFrequencies[i] <- mean(sample(data, size = n, replace = TRUE))
    i <- i + 1
    if (i > k ) {
      break;
    }
  }
  # graph distribution of freq
  hist(
    tempFrequencies,
    breaks = dataClasses,
    main = c("Histogram of  with n =", n),
    xlab = "Values"
  )
}