

#' Plot the lag scatter plots and the regression lines for time series
#'
#' @param ts_data data of the time series as a vector
#' @param nlag number of lagging; 2 by default
#' @param Rsqur logical value; whether the R^2 values for regression are added on the plot; TRUE by default
#'
#' @return
#' @export
#'
#' @examples
#' data("AHIdat_ap")
#' CDname<- names(AHIdat_ap)
#' dat_ap <- AHIdat_ap[1]
#' names(dat_ap) <- NULL
#' dat_ap <- data.frame(dat_ap[1])
#' ts_data <- dat_ap$kO3.24hm.lag0
#' LagPlot(ts_data,nlag=2,Rsqur = TRUE) 

LagPlot <- function(ts_data,nlag=2,Rsqur = TRUE){
  library(ggplot2)
  library(gtable)
  library(gridExtra)
  
  

  # create the lagged time series 
  data_lag <- matrix(ts_data, nrow=length(ts_data))
  for (i in 1:nlag){
    data_lag <- cbind(data_lag, c(rep(NA,i),ts_data[1:(length(ts_data)-i)]))
  }
  data_lag <- data.frame(data_lag)
   
  # create the plot
  PLAG <- list()
  for (i in 1:nlag){
    mod <- lm(data_lag[,i+1]~data_lag[,1])
    plag <- ggplot(data = data_lag) + 
      geom_point(mapping = aes(x =data_lag[,1] , y = data_lag[,i+1],),color='orange')+
      geom_abline(intercept = coef(mod)[1], slope = coef(mod)[2], color = 'blue', linewidth =2)+
      xlab(paste("data.lag",0,sep=''))+
      ylab(paste("data.lag",i,sep=''))
    if (Rsqur == TRUE){
      plag <- plag + annotate("text", x = Inf, y = Inf, label = paste("R² =", round(summary(mod)$adj.r.squared, 3)), hjust = 1.1, vjust = 2)
    }
    PLAG[[i]] <- plag
  }
  grid.arrange(grobs=PLAG, nrow=1)
}
