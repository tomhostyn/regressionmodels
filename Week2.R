library(UsingR); 
data(diamond)



q1 <- function (){
  
  x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
  y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
  fit <- lm (y ~ x)
  summary(fit)$coefficient[1,4] #pick Intercept, Pr(>|t|) # WRONG
  summary(fit)$coefficient[2,4] #pick slope, Pr(>|t|)
}

q2 <- function () {
  x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
  y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
  
  fit <- lm (y ~ x)
  summary (fit)
  summary(fit)$sigma
}

data(mtcars)

q3_wrong <- function (){
  fit <- lm (mpg ~ wt, data=mtcars)
  #prob_interval <- c(-1, 1) * qt(0.975, df = fit$df)  #use .975 for 95% confidence.  1- 5%/2 ?
  confint(fit, 'wt', level=0.95)
  # -6.486 is wrong.
}

q3 <- function (){
  fit <- lm (mpg ~ wt, data=mtcars)
#  predict (fit, d, interval="predict")
  predict (fit, d, interval="confidence")[2]
}

q4 <- function () {
  #the estimate expected change  in mpg per 1000 lbs increase in wt
}

q5 <- function () {
  fit <- lm (mpg ~ wt, data=mtcars)
  confint(fit, 'wt', level=0.95)
  d <- data.frame(wt = 3)
  predict (fit, d, interval="predict")
}

q6 <- function () {
  fit <- lm (mpg ~ wt, data=mtcars)
  interval <- confint(fit, 'wt', level=0.95)
  2 * interval
}

q7 <- function () {
  p <- 1:1000
  cm <- 2*p + 100 + runif (p, -1, 1)
  m <- cm/100
  fit_cm <- lm (p ~ cm)
  fit_m <- lm (p ~ m)  
  
  c(cm= coef(fit_cm)[2], m=coef(fit_m)[2])
}

q7 <- function () {
  #
}

q8 <- function () {
  p <- 1:1000
  q <- 2*p + 100 + runif (p, -1, 1)
  c <- 500
  pprime <- p + c
  fit_q <- lm (q ~ p)
  fit_r <- lm (q ~ pprime)  
  
  b0 <- coef(fit_q)[1]
  b1 <- coef(fit_q)[2]
  
  df <- cbind(c("b1+c", "b0-c*b1", "b0+c*b1", "c*b1"),
        c(b1+c, b0-c*b1, b0+c*b1, c*b1), 
        coef(fit_r)[1])
  df
}


q9 <- function (){
  fit <- lm (mpg ~ wt, data=mtcars)
  originfit <- lm (mpg ~ wt -1 , data=mtcars)
  intfit <- lm (mpg ~ 1 , data=mtcars)
  
  pred <- predict (fit, mtcars["wt"])
  sqerr <- sum((mtcars$mpg - pred)^2)
  
  origin_pred <- predict (originfit, mtcars["wt"])
  origin_sqerr <- sum((mtcars$mpg - origin_pred)^2)

  int_pred <- predict (intfit, mtcars["wt"])
  int_sqerr <- sum((mtcars$mpg - int_pred)^2)
  
  c(sqerr/origin_sqerr, sqerr/int_sqerr)
}


q10_wrong <- function (){
  #if an intercept is included , the residuals won't likely sum to zero
}
q10 <- function (){  
  p <- 1:1000
  q <- 2*p
  sum(lm(mpg ~ wt, data=mtcars)$residuals)
  # must always be zero!
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


x2 <- function () {
  y <- diamond$price; x <- diamond$carat;
  fit <- lm (y ~ x )
  summary (fit)
}