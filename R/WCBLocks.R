#' Segregate pollutants and temperature time series by Warm/Cold seasons in 'AHIdat_ap.rda' and
#' 'AHIdat_default.rda'. The return will be ready for a classification problem.
#'
#' @param listnum The number in the list of data file 'AHIdat_ap.rda' and 'AHIdat_default.rda', each number represent a CD name. ranging from 1 to 24. Default value is 1, meaning the CD of Halifax.
#' @param PolCol The columns' names of pollutant measurements of interests, default value is c('k03.8hm','k03.24hm','kN02.24hm').
#' @param TempCol The column's name of temperature, default value is "Temp".
#'
#' @return X The input (of the classification problem) is an array with 3 dimension: i, j, k. i is the index for the i-th inputs/observation, j is the day index in each season, k is the  index for channels. The range of i is 1632 (34(years)*24(CDs)*2(seasons)). The range of j is 182 (days in one season). The range of k is 4 channels (k03.8hm,k03.24hm,kN02.24hm, and temp) 
#' @return Y The output (of the classification problem) is an array with 1 dimension, i.e. the index for each response. i is the same as i in the input. The i represent the i-th datum in the target data, and its range is 1632 (34(years)*24(CDs)*2(seasons)). Each response has3 values. The first value is the type of season W/C, the second value is the year, the third value is the name of CD. 
#' @export
#'
#' @examples
WCBlocks <- function(listnum=1,PolCol=c('k03.8hm','k03.24hm','kN02.24hm'),TempCol="Temp"){
  # load data
  data("AHIdat_ap")
  data("AHIdat_default")
  CDname<- names(AHIdat_ap)
  
  # extract data from the CD of 'listnum'
  dat_ap <- AHIdat_ap[listnum]
  dat_t <- AHIdat_default[listnum]
  names(dat_ap) <- NULL
  names(dat_t) <- NULL
  dat_ap <- data.frame(dat_ap[1])
  dat_t <- data.frame(dat_t[1])
  # array for X
  X <- array(dim=c(1632,182,4))
  # array for Y
  Y <- array(dim=1632)
  for (i in 1:length(CDname) ){
    # get the data.frame of pollutants and temperature for each CD
    dat_ap <- AHIdat_ap[i]
    dat_t <- AHIdat_default[i]
    names(dat_ap) <- NULL
    names(dat_t) <- NULL
    dat_ap <- data.frame(dat_ap[1])
    dat_t <- data.frame(dat_t[1])
    #extract data for each season at each CD
    Begin <- 1 # record the starting row index
    End <- 1 # record the ending row index
    Season <- is.element(dat_ap[1,]$Month,c(4:9)) # If in warm season
    while(End<=nrow(dat_ap)){
      End <- End+1
      if (is.element(dat_ap[End,]$Month,c(4:9))!=Season){
        Season <- !(Season)
        if ((End-Begin)>181){
          year <- dat_ap[Begin,]$Year
          X[(i-1)*68+(year-1980)*2+1+as.numeric(!Season),,1] <- dat_ap[Begin:(End-1),]$kO3.8hmx.lag0[1:182]
          X[(i-1)*68+(year-1980)*2+1+as.numeric(!Season),,2] <- dat_ap[Begin:(End-1),]$kO3.24hm.lag0[1:182]
          X[(i-1)*68+(year-1980)*2+1+as.numeric(!Season),,3] <- dat_ap[Begin:(End-1),]$kNO2.24hm.lag0[1:182]
          X[(i-1)*68+(year-1980)*2+1+as.numeric(!Season),,4] <- dat_t[Begin:(End-1),]$Temp.24hm.lag0[1:182]
          Y[(i-1)*68+(year-1980)*2+1+as.numeric(!Season)] <- !(Season)
        }
        Begin <- End
        
      }
    }
  }
  
  X
  Y
}

