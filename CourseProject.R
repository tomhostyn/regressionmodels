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
  scatterplotMatrix(scale(mtcars[c("mpg", "wt", "cyl", "disp", "hp", "am")]))  
}

earlyAnalysis <- function () {
  fitall <- lm(mpg ~ . , data = mtcars)
  summary (fitall)
}

principalAnalysis <- function() {
  standardised <- as.data.frame(scale(mtcars))
  mtcars.pca <- prcomp(standardised)
  screeplot(mtcars.pca, type="lines")
}

correlation.test <- function () {
  names <- names(mtcars)
  
  cor <- c()
  pval <- c()
  for (i in 1:(length(names))){
    t <- cor.test(mtcars$mpg, mtcars[[i]])
    cor <- c(cor, t$estimate)
    pval <- c(pval, t$p.value)
  }
  mpgcor.df <- data.frame(names=names, cor = cor, pval = pval)
  
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



# Regress the given variable on the given predictor,
# suppressing the intercept, and return the residual.
regressOneOnOne <- function(predictor, other, dataframe){
  # Point A. Create a formula such as Girth ~ Height -1
  formula <- paste0(other, " ~ ", predictor, " - 1")
  # Use the formula in a regression and return the residual.
  resid(lm(formula, dataframe))
}

# Eliminate the specified predictor from the dataframe by
# regressing all other variables on that predictor
# and returning a data frame containing the residuals
# of those regressions.
eliminate <- function(predictor, dataframe){
  # Find the names of all columns except the predictor.
  others <- setdiff(names(dataframe), predictor)
  # Calculate the residuals of each when regressed against the given predictor
  temp <- sapply(others, function(other)regressOneOnOne(predictor, other, dataframe))
  # sapply returns a matrix of residuals; convert to a data frame and return.
  as.data.frame(temp)
}
