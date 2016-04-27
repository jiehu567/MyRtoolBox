# Plot Confidence Interval for any linear model

plotCI <- function(model_name,      
                   plotLine = TRUE, 
                   plotCI = TRUE, 
                   plotHI = FALSE, 
                   col1 = "red", col2 = "green") {
  
  X <- model_name$model[,2]
  Y <- model_name$fitted.values
  
  if (plotLine) {
    plot(X,Y)
    abline(model_name)
  }
  
  newdata <- data.frame(X = seq(from = min(X), to = max(X),by = 0.1))
  prd_CI <- predict(model_name, newdata = newdata, interval = "confidence", type = "response") 
  prd_HI <- predict(model_name, newdata = newdata, interval = "predict", type = "response")
  
  df_up_CI <- data.frame(newdata,prd_CI[,2])
  names(df_up_CI) <- c("X", "Pred")
  df_low_CI <- data.frame(newdata,prd_CI[,3])
  names(df_low_CI) <- c("X", "Pred")
  
  df_up_HI <- data.frame(newdata,prd_HI[,2])
  names(df_up_HI) <- c("X", "Pred")
  df_low_HI <- data.frame(newdata,prd_HI[,3])
  names(df_low_HI) <- c("X", "Pred")
  
  if(plotCI){
    lines(df_up_CI$X, df_up_CI$Pred,col= col1,lty=2)
    lines(df_low_CI$X,df_low_CI$Pred,col= col1,lty=2)
  }
 
  if(plotHI){
    lines(df_up_HI$X, df_up_HI$Pred,col= col2,lty=2)
    lines(df_low_HI$X,df_low_HI$Pred,col= col2,lty=2)
  }
}

# test code:
X <- c(0:9)
Y <- c(98,135,162,178,221,232,283,300,374,395)
model <- lm(Y~X)
plotCI(model,plotHI = T, col1 = "grey", col2 = "green")
