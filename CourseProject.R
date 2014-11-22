library(UsingR); 
data(mtcars)
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("A", "M")


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

correlation.test.wt <- function () {
  names <- names(mtcars)
  
  cor <- c()
  pval <- c()
  for (i in 1:(length(names))){
    t <- cor.test(mtcars$wt, mtcars[[i]])
    cor <- c(cor, t$estimate)
    pval <- c(pval, t$p.value)
  }
  mpgcor.df <- data.frame(names=names, r2 = cor*cor , cor = cor, pval = pval)
  
  mpgcor.df <- mpgcor.df[order(abs(mpgcor.df$cor), decreasing=TRUE),]
  mpgcor.df
}


correlation.test <- function (var) {
  names <- names(mtcars)
  
  cor <- c()
  pval <- c()
  for (i in 1:(length(names))){
    t <- cor.test(as.numeric(mtcars[[var]]), as.numeric(mtcars[[i]]))
    cor <- c(cor, t$estimate)
    pval <- c(pval, t$p.value)
  }
  sig  <- sapply (pval, function (p){
    s <- ' '
    if (p < 0.1){s <- "*"}
    if (p < 0.05){s <- "**"}
    if (p < 0.01){s <- "***"}
    s})
  mpgcor.df <- data.frame(names=names, r2 = cor*cor , cor = cor, pval = pval, sig=sig)
  
  mpgcor.df <- mpgcor.df[order(abs(mpgcor.df$cor), decreasing=TRUE),]
  mpgcor.df
}

correlation.test.plot <- function (var){
  mpgcor.df <- correlation.test(var)
  cols <- sapply (mpgcor.df$names, function (c){if (c == "am"){"red"}else{"darkblue"}})
  
  barplot(mpgcor.df$r2, names.arg=mpgcor.df$names, col=cols,
          main=paste("correlation of factors with",var))
}

correlation.test.plot.mpg <- function(){
  correlation.test.plot("mpg")
}

plotlms <- function() {
  mpg_wt_fit <- lm (mpg ~ wt, data = mtcars)
  plot (mpg_wt_fit)
}




###############  VISUALIZING THE DATA SET ##################

V1 <- function (){
  mtcars_f <- mtcars
  mtcars_f$am <- as.factor(mtcars_f$am) 
  model <- lm (mpg ~ am + wt, data = mtcars_f)
  model2 <- lm (mpg ~ am, data = mtcars_f)
  
  
  plot (mtcars$wt, mtcars$mpg, main="Mpg vs weight vs Transmission type", sub="automatic = red, manual = blue")
  points (mtcars$wt[mtcars$am == "A"], mtcars$mpg[mtcars$am == "A"], pch = 19, cex = 1.3, col="red")
  points (mtcars$wt[mtcars$am == "M"], mtcars$mpg[mtcars$am == "M"], pch = 19, cex = 1.3, col="blue")
  abline(lm(mpg~wt, data=mtcars))
}

models <- function () {
  mtcars_f <- mtcars
  mtcars$am <- as.factor(mtcars$am)
  m1 <- lm(mpg~ wt -1, data=mtcars)
  m2 <- lm(mpg~ wt + am -1, data=mtcars)
  m3 <- lm(mpg~ wt + am + cyl -1, data=mtcars)
  m4 <- lm(mpg~ wt + am + cyl + hp -1, data=mtcars)
  
  anova(m1, m2, m3, m4)
#  m5 <- lm(mpg~ wt + am + cyl + hp + disp -1, data=mtcars_f)
#  m6 <- lm(mpg~ wt + am + cyl + hp + vs -1, data=mtcars_f)
#  m7 <- lm(mpg~ wt + am + cyl + hp + drat -1, data=mtcars_f)
  
  
#  print(anova(m1, m2, m3, m4, m5))
 # print(anova(m1, m2, m3, m4, m6))
  #print(anova(m1, m2, m3, m4, m7))
  
  #plot (m4)
#  plot(lm(mpg~ wt + am +drat -1, data=mtcars_f))
}

V2 <- function (){
  mtcars_f <- mtcars
  mtcars_f$am <- as.factor(mtcars_f$am) 
  #REMOVE WEIGHT!
  model <- lm (mpg ~  wt, data = mtcars)
  model2 <- lm (cyl ~ wt , data = mtcars)
  x <- resid(lm (mpg ~ wt, data = mtcars))
  y <- resid(lm (cyl~wt, data = mtcars))
  plot (x,y)
  points (x[mtcars$am == 0], y[mtcars$am == 0], pch = 19, cex = 1.3, col="red")
  points (x[mtcars$am == 1], y[mtcars$am == 1], pch = 19, cex = 1.3, col="blue")
  res_model <- lm(y ~ x)
  print(summary (res_model))
  abline (res_model)
}

overlap <- function (){
  min <- min(mtcars$wt[mtcars$am == "A"])
  max <- max(mtcars$wt[mtcars$am == "M"])
  
  mtcars_mix <- mtcars [mtcars$wt <= max & mtcars$wt >=min,]
  mtcars_mix

  plot (mtcars_mix$wt, mtcars_mix$mpg, main="Mpg vs weight vs Transmission type", sub="automatic = red, manual = blue")
  points (mtcars_mix$wt[mtcars_mix$am == "A"], mtcars_mix$mpg[mtcars_mix$am == "A"], pch = 19, cex = 1.3, col="red")
  points (mtcars_mix$wt[mtcars_mix$am == "M"], mtcars_mix$mpg[mtcars_mix$am == "M"], pch = 19, cex = 1.3, col="blue")
  abline(lm(mpg~wt, data=mtcars_mix))
  
}