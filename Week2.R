
q1 <- function (){
  
  x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
  y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
  ct <- coefTable(x,y)
  ct
}

q2 <- function () {
  x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
  y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
  
  fit <- lm (y ~ x)
  sd(fit$residuals)
}

#############

library(UsingR); data(diamond)

coefTable <- function (x,y){
  n <- length(y)
  beta1 <- cor(y, x) * sd(y) / sd(x)
  beta0 <- mean(y) - beta1 * mean(x)
  e <- y - beta0 - beta1 * x
  sigma <- sqrt(sum(e^2) / (n-2)) 
  ssx <- sum((x - mean(x))^2)
  seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
  seBeta1 <- sigma / sqrt(ssx)
  tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
  pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
  pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
  coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
  colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
  rownames(coefTable) <- c("(Intercept)", "x")  
  coefTable
}

x1 <- function () {
  y <- diamond$price; x <- diamond$carat; n <- length(y)
  beta1 <- cor(y, x) * sd(y) / sd(x)
  beta0 <- mean(y) - beta1 * mean(x)
  e <- y - beta0 - beta1 * x
  sigma <- sqrt(sum(e^2) / (n-2)) 
  ssx <- sum((x - mean(x))^2)
  seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
  seBeta1 <- sigma / sqrt(ssx)
  tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
  pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
  pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
  coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
  colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
  rownames(coefTable) <- c("(Intercept)", "x")
}