library(UsingR)
library (ggplot2)



q1 <- function (){
  
  x <- c(0.18, -1.54, 0.42, 0.95)
  w <- c(2, 1, 3, 1)
  
 
  lsq<- function (mu) {sum(w*(x-mu)^2)}
  
  mu <- c(3, 0.1471, 1.077, 0.0025)
  cbind(mu, sapply (mu, lsq))
}

q2 <- function () {
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
  
  e <- data.frame (x, y)
  
  lm(y~x-1, data = e)
}


q2 <- function () {
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
  
  e <- data.frame (x, y)
  
  lm(y~x-1, data = e)
}

data(mtcars)
q3 <- function () {
  lm (mpg ~ wt, data = mtcars)
}

q4 <- function () {
  cor <- 0.5
  Sx <- 0.5
  Sy <- 1
  
  # slope = cor(x,y) * sd(y)/Sd(x)
  slope <- cor * Sy/Sx
  slope
 }


q5 <- function () {
  cor <- 0.4
  x <- 1.5
  
  #in normalized data sets, slope = correlation
  x*cor
}

stddev <- function (x){
  var <- 1/(length(x) - 1) * sum ((x - mean(x))^2)
  sqrt(var)
}

q6 <- function () {
  x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
  x_origin <- x - mean (x)
  
  scaled <- x_origin / stddev (x_origin)
  scaled
}


q7 <- function() {
  
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
  
  d<-data.frame(x,y)
  
  lm (y ~ x , d)
}



q9 <- function () {
  x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
  min <- c(0.573, 0.8, 0.36, 0.44)
  
  err <- sapply (min, function (m){sum (x - m)^2})
  data.frame (min, err)
}

########################


v2 <- function(){
  data(galton)
  par(mfrow=c(1,2))
  hist(galton$child)
  hist(galton$child)
}

v3 <- function () {
  
  library(manipulate)
  myHist <- function(mu){
    mse <- mean((galton$child - mu)^2)
    g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
    g <- g + geom_vline(xintercept = mu, size = 3)
    g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
    g
  }
  manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
}



