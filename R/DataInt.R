#
#

#' A function that abstract the k columns (2 step interpolated data) in AHIdat_ap
#' AHIdat_ap is a rda file from the package of AHItool (unpublished) created by Wesley Burr
#' @param dat data.frame from AHIdat_ap list
#' @param key the first char in the names of columns that will be abstracted
#' @param WarmStart the starting month for warm season
#' @param WarmEnd  the ending month for warm season
#'
#' @return datInt A data.frame that has dat's columns with a name starts with "k" and ends with "0"
#' @export
#'
#' @examples
#' dat <- data.frame(Month=rep(1:12,50),k.pollutant1.log1=runif(600),date=runif(600),k.pol.log0=runif(600))
#' dat.Int <- DataInt(dat)

DataInt <- function(dat,key="k",WarmStart=4,WarmEnd=9){
  # find columns with a name starts with "k" and ends with "0"
  ap_name <- names(dat)
  index <- c()
  for (i in 1: length(ap_name)){
    cha <- ap_name[i]
    if (substr(ap_name[i],1,1)==key){
      if (substr(cha,nchar(cha),nchar(cha))=="0"){
        index <- c(index, i)
      }
    }
  }
  datInt <- dat[index]
  # add another column season showing whether is warm season or cold season
  season <- rep(x="Cold",nrow(dat))
  season[(dat$Month>(WarmStart-1))&(dat$Month<(WarmEnd+1))] <- "Warm"
  datInt <- data.frame(datInt,Season=season)
}
