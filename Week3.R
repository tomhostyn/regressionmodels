
library(UsingR)
data(mtcars)
require(stats)
require(graphics)

p <- function (){ 
  pairs(mtcars, panel=panel.smooth)
}

q1 <- function () {
  mtcars_f <- mtcars
  mtcars_f$cyl <- as.factor(mtcars_f$cyl) 
  model <- lm (mpg ~ cyl + wt -1, data = mtcars_f)
  
  print(model)
  
#  pairs(mpg ~ cyl + wt, data = mtcars_f)
  
  print (coef(model)["cyl8"] - coef(model)["cyl4"])
  q2_vis()
}


q2 <- function () {
  mtcars_f <- mtcars
  mtcars_f$cyl <- as.factor(mtcars_f$cyl) 
  model <- lm (mpg ~ cyl + wt -1, data = mtcars_f)
  model2 <- lm (mpg ~ cyl -1, data = mtcars_f)
  
  print(model)
  print(model2)
#   print ("with weight")
#   print (coef(model)["cyl8"] - coef(model)["cyl4"])
#   print ("without weight")
#   print (coef(model2)["cyl8"] - coef(model2)["cyl4"])
  
  
  q2_vis()
#   pairs(mpg ~ cyl + wt -1, data = mtcars_f)
#   pairs(mpg ~ cyl -1, data = mtcars_f)
}

q2_vis <- function (){
  mtcars_f <- mtcars
  mtcars_f$cyl <- as.factor(mtcars_f$cyl) 
  model <- lm (mpg ~ cyl + wt, data = mtcars_f)
  model2 <- lm (mpg ~ cyl, data = mtcars_f)
  

  plot (mtcars$wt, mtcars$mpg)
  points (mtcars$wt[mtcars$cyl == 4], mtcars$mpg[mtcars$cyl == 4], col="red", pch="4")
  points (mtcars$wt[mtcars$cyl == 6], mtcars$mpg[mtcars$cyl == 6], col="blue", pch="6")
  points (mtcars$wt[mtcars$cyl == 8], mtcars$mpg[mtcars$cyl == 8], col="purple", pch="8")
  abline(lm(mpg~wt, data=mtcars))
}

q2_vis2 <- function (){
  mtcars_f <- mtcars
  mtcars_f$cyl <- as.factor(mtcars_f$cyl) 
  #REMOVE WEIGHT!
  model <- lm (mpg ~  wt, data = mtcars)
  model2 <- lm (cyl ~ wt , data = mtcars)
  plot (resid(lm (mpg ~ wt, data = mtcars)), resid(lm (cyl~wt, data = mtcars)))
  abline (lm(resid(lm (mpg ~ wt, data = mtcars)) ~ resid(lm (cyl~wt, data = mtcars))))
  
#   plot (mtcars$wt, mtcars$mpg)
#   points (mtcars$wt[mtcars$cyl == 4], mtcars$mpg[mtcars$cyl == 4], col="red", pch="4")
#   points (mtcars$wt[mtcars$cyl == 6], mtcars$mpg[mtcars$cyl == 6], col="blue", pch="6")
#   points (mtcars$wt[mtcars$cyl == 8], mtcars$mpg[mtcars$cyl == 8], col="purple", pch="8")
#   abline(lm(mpg~wt, data=mtcars))
}


q3 <- function (){

  mtcars_f <- mtcars
  mtcars_f$cyl <- as.factor(mtcars_f$cyl) 
  model <- lm (mpg ~ cyl + wt, data = mtcars_f)
  model2 <- lm (mpg ~ cyl*wt -1, data = mtcars_f)
 
  print(summary(model))
  print(summary(model2))
  a <- anova (model, model2)
  P <- a[["Pr(>F)"]][2]
  print (a)
  if (P > 0.05){
    print ("P is larger than 0.05. the interaction term is not necessary")
  }
}


q4 <- function () {
  ?mtcars
  print ( "NOTE :  wt   Weight (lb/1000)")
  m <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  print ("expected change in mpg per 0.5 ton (=1000 lb) increase in weight = -3.206")
  m1 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
  print ("-6.411 = expected change in mpg per 2x0.5 ton (=1000 lb) increase in weight")
  
  print(m)
  print (m1)
}


q5 <- function (){
  x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
  y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
  hat <- hatvalues (lm (y~x))
  max(hat)
}


q6 <- function () {
  x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
  y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
  fit <- lm (y~x)
  hat <- hatvalues (fit)
  
  dfbetas(fit)[which.max(hat),2]
}