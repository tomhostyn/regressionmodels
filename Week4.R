library(MASS)
data(shuttle)
q1<-function () {
 
    fit <- glm (use~wind, family="binomial",data=shuttle)
    print(summary(fit))
    exp(coef(fit)[2])
    #b1 is log odds ratio
    # therefore exp(b1) is odds ratio.  slide 8 03_02_binary_outcomes
}

q2<-function () {
  
  fit <- glm (use~wind+magn, family="binomial",data=shuttle)
  print(summary(fit))
  exp(coef(fit))
}

q3<-function () {
  
  shuttle$use <- as.numeric(shuttle$use=="auto")
  fit <- glm (use ~ wind, family="binomial",data=shuttle)
  fit2 <- glm (1 - use ~ wind, family="binomial",data=shuttle)
  
  print(coef(fit))
  print(coef(fit2))
  
}

q4 <- function () {
  fit <- glm (count ~ spray-1, family="poisson",data=InsectSprays)
  summary(fit)
  exp(fit$coef[1])/exp(fit$coef[2])
}


q5<- function () {
  F <- function (x, t){3*t+x+5}
  t <- 1:10
  
  count <- c(F(t,1),F(t,0))
  dataset<- data.frame (count=count,x=c(rep(1,10), rep(0,10)), t=log(c(t,t)))
  dataset$t2 <- log(10)+t
  
  
  print (coef(glm(count ~ x + offset(t), family = poisson, data = dataset)))
  print (coef(glm(count ~ x + offset(t2), family = poisson, data = dataset)))
  
}


q6 <- function () {
  x <- -5:5
  y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
  knot <- matrix (c(-5:0, rep(0,5),rep(0,5),0:5), nrow=11 )
  lm(y~knot)
}