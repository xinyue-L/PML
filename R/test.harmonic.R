#' Harmonic analysis test for Fast Fourier Transform
#'
#' This function conducts harmonic test sequentially based on observations or Fast Fourier Transform (FFT)
#' results.
#'
#' @param ob Either the original observation or FFT results. See parameter \code{fft}.
#' @param p The p-value to be considered statistically significant.
#' @param fft If TRUE, \code{ob} is FFT results, with the first column frequencies and the second column signals in standardized proportions; if FALSE, \code{ob} is a vector of the original observation. The default is FALSE.
#' @param maxfreq To conduct test on at most \code{maxfreq} frequencies. The default is 10.
#'
#' @keywords harmonic test
#'
#' @return A list of two elements:
#' @return \item{sig}{The significant frequencies plus the first insignificant frequency.}
#' @return \item{fft}{The FFT results expressed in standardized proportions.}
#' @examples
#' data(pa3)
#'
#' #### test on individuals
#' ob <- do.call("c",pa3$activity[1:4])
#' re <- test.harmonic(ob,p=0.05/(length(ob)-1)/2)
#' re$sig;head(re$fft) ## no harmonic is significant
#' ob2 <- do.call("c",pa3$activity[11:13])
#' re2 <- test.harmonic(ob2,p=0.05/(length(ob2)-1)/2)
#' re2$sig;head(re2$fft) ## 3 significant harmonics
#'
#' #### test on the population average
#' re0 <- bandSelect(df=pa3,Nlength=1440*3,Nlambda=100,alpha=1,Ntop=3,
#' cross=FALSE,Ncross=NULL,plot=TRUE)
#' freq <- data.frame(Frequency=re0$freq,Proportion=colMeans(re0$xprop))
#' re3 <- test.harmonic(freq,p=0.05/nrow(freq),fft=TRUE)
#' print(re3$sig,digits=3,row.names=FALSE)
#'
#' @seealso \code{\link{pharmonic}}
#'
#' @references Fisher, R. A. (1929). Tests of significance in harmonic analysis. Proceedings of the Royal Society of London. Series A, 125(796), 54-59.
#'
#' @export

test.harmonic <- function(ob,p,fft=FALSE,maxfreq=10) {
  if(fft==TRUE) {
    fre <- ob[order(ob[,2],decreasing=TRUE),]
  } else {
    f <- fft(ob)
    fre <- fft.harmonic(f)
    fre <- fre[,c(3,4)]
  }
  names(fre) <- c("freq","prop(g)")
  n <- nrow(fre)
  re <- data.frame(freq=0,prop=0,p=0)
  for(i in 1:maxfreq) {
    r <- i
    p2 <- pharmonic(n,r,fre[i,2])
    if(p2<p) {
      if (i==1) {
        re[1,] <- c(fre[i,c(1,2)],p2)
      } else {
        re <- rbind(re,re[1,])
        re[i,] <- c(fre[i,c(1,2)],p2)
        if(i>maxfreq) {
          message(paste("P-value selected too large and too many frequencies significant ( > ,", maxfreq, ",max freq reached), consider Bonferonni correction?",sep=""))
          break
        }
      }
    } else {
      if (i==1) {
        re[1,] <- c(fre[i,c(1,2)],p2)
      } else {
        re <- rbind(re,re[1,])
        re[i,] <- c(fre[i,c(1,2)],p2)
      }
      break
    }
  }
  pp <- rep(p,nrow(re))
  re <- cbind(re,pp)
  colnames(re) <- c("frequency","prop (g)","p-value","p-threshold")
  return(list(sig=re,fft=fre))
}
