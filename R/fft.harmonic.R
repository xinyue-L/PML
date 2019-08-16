#' FFT auxillary function
#'
#' Take FFT input and reorganize it showing top significant frequencies.
#' 
#' @param Xk the trelliscope data frame containing two variables: subject ID and activity
#' 
#' @keywords internal
#' 
#' @return Organized FFT results from top significant frequencies.


fft.harmonic <- function(Xk) {
  pd  <- cbind(0:(length(Xk)-1), Mod(Xk))
  pd[2:length(Xk),2] <- 2*pd[2:length(Xk),2] 
  pd <- pd[pd[,1]<length(Xk)/2,]
  pd <- pd[-1,]
  pd <- cbind(pd,length(Xk)/pd[,1])
  pd <- cbind(pd,pd[,2]/sum(pd[,2]))
  pd <- pd[order(-pd[,4]),]
  colnames(pd) <- c("seq","value","freq","prop")
  return(pd)
}
