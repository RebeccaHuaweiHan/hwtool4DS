#' This function plot transparent lines from the a data.frame, the level of transparency was an parameter of the function
#' @title sparkplot
#' @param dat the data.frame user will pass into the function
#' @param level.trans level of transparency for the color of lines
#' @param ... pass arguments to the embedded functions
#' @return NULL
#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics lines
#' @importFrom stats runif
#'
#' @examples num_data =20  # number of data for each line
#'           dat<-data.frame(col1=runif(num_data),col2=runif(num_data),
#'                           col3=runif(num_data),col4=runif(num_data),
#'                           col5=runif(num_data),col6=runif(num_data))
#'           sparkplot(dat,level.trans = 0.3,
#'                     xlab="xaxis", ylab="dataframe column values",
#'                     main ="Transparent Lines / Spark Plot")

sparkplot <- function(dat,level.trans=0.5,...){

  # "sanity check": check if dat is a data.frame
  if(!is.data.frame(dat)){
    stop("Sorry, dat is not a data.frame, please call sparkplot again and make
    sure that dat is a data.frame")
  }
  if((!is.numeric(level.trans))||(level.trans<0)||(level.trans>1)){
    stop("The level.trans parameter should be a value between 0 and 1! Please
         call sparkplot again.")
  }



  # Number of lines in the figure
  num_lines<- ncol(dat)
  # Number of dots for each line
  x_lim<- nrow(dat)

  # plot the first column of data without showing
  plot(1:x_lim, dat[,1],xlim = c(1, nrow(dat)), ylim = c(min(dat), max(dat)),
       type = "n",...)

  # plot the rest columns(expect the last column) of data in transparent lines
  # colors of those lines are random
  for (i in 2:(num_lines-1)){
    lines(1:x_lim,
          dat[, i],
          col = rgb(runif(1),runif(1),runif(1),alpha = level.trans),
          ylim =c(0,20),
          lwd = 2)
    }

  # plot the last column with a fixed color of
  # "turquoise" (64/255,224/255,208/255)
  lines(1:x_lim, dat[, num_lines],col = rgb(0,0,0,alpha = level.trans)
        ,ylim =c(0,20), lwd = 2)
  NULL
}


