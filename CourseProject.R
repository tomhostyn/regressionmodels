library(UsingR); 
data(mtcars)

library(car)  # for matrix scatter plots

# http://little-book-of-r-for-multivariate-analysis.readthedocs.org/en/latest/src/multivariateanalysis.html




initialPlots <- function () {
  
  plot (mtcars$mpg)
  plot (mtcars$am)
  
  plot (mtcars$mpg[mtcars$am == 0], col="red")
  plot (mtcars$mpg[mtcars$am == 1], col="blue")
  
  scatterplotMatrix(mtcars)

  scatterplotMatrix(mtcars[c("mpg", "wt", "cyl", "disp", "hp", "am")])  
}

earlyAnalysis <- function () {
  fitall <- lm(mpg ~ . , data = mtcars)
  summary (fitall)
}


correlation.test <- function () {
  names <- names(mtcars)
  
  cor <- c()
  for (i in 1:(length(names))){
    t <- cor.test(mtcars$mpg, mtcars[[i]])
    cor <- c(cor, t$estimate)
  }
  mpgcor.df <- data.frame(names=names, cor = cor)
  
  mpgcor.df[order(abs(mpgcor.df$cor), decreasing=TRUE),]
}


plotlms <- function() {
  mpg_wt_fit <- lm (mpg ~ wt, data = mtcars)
  plot (mpg_wt_fit)
}


doanalysis <- function () { 
  mtcars$mpg
  mtcars$am
  mtcars$cyl 
  
  fit <- lm ()
}