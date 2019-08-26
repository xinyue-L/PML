#' harmonic analysis test: g-value calculation
#'
#' This function calculates the g-value for the harmonic analysis test developed by R.A. Fisher (1929). 
#' Harmonic analysis refers to Fast Fourier Transform (FFT) results.
#' Specifically, g is the proportion (squared modulus of one frequency divided by the sum of all squared moduli).
#' In order for g to be statistically significant in the harmonic analysis test, it needs to be at least g-value
#' at significance level \eqn{\alpha}. Please note that for the rth largest frequency, if any of the previous (r-1) 
#' frequencies is not significant, then the rth largest frequency is also non-significant.
#' 
#' @param n the total number of frequencies in FFT results.
#' @param r the modulus of the tested frequency is ranked as the rth largest among all frequencies.
#' @param p the FFT result of the tested frequency expressed as the squared modulus divided by the sum of the squared moduli by all frequencies (proportion: m_r^2/(m_1^2+...+m_n^2)).
#' @param tol the tolerance level during calculation. The default is 10^-7.
#' @param init the crude estimate for g-value if known. It is not called to calculate usual g-values.
#' @keywords harmonic test
#' 
#' @return The g-value calculated by the harmonic test.

#' @examples
#' gharmonic(n=100,r=1,p=0.05)
#' 
#' @seealso \code{\link{pharmonic}}
#' 
#' @references Fisher, R. A. (1929). Tests of significance in harmonic analysis. Proceedings of the Royal Society of London. Series A, 125(796), 54-59.
#' 
#' @export

gharmonic <- function(n,r,p,tol=10^-7,init=NULL) {
  calc <- function(nn,rr,g) {
    re <- 0
    k <- ifelse(floor(1/g)==1/g,1/g-1,floor(1/g))
    if(k<rr) return(re)
    for (j in rr:k) {
      if(1-j*g>0) {
        temp <- lfactorial(nn)-lfactorial(rr-1)+(nn-1)*log(1-j*g)-log(j)-lfactorial(nn-j)-lfactorial(j-r)
        if(exp(temp)==0) break
        re <- re+(-1)^(j-rr)*exp(temp)
      } else {
        temp <- lfactorial(nn)-lfactorial(rr-1)+(nn-1)*log(j*g-1)-log(j)-lfactorial(nn-j)-lfactorial(j-r)
        if(exp(temp)==0) break
        re <- re+(-1)^(j-rr+nn-1)*exp(temp)
      }
    }
    return(re)
  }
  calc <- Vectorize(calc,vectorize.args ="g")
  #<70: 0.1-0.9
  #<983: 0.01-0.1
  if(is.null(init)) {
    first <- seq(0.1,0.9,by=0.1)
    firstre <- calc(nn=n,rr=r,g=first)
    if(!any(firstre>p & firstre<1,na.rm = TRUE)) {
      first <- seq(0.01,0.1,by=0.01)
      firstre <- calc(nn=n,rr=r,g=first)
      if(!any(firstre>p & firstre<1,na.rm = TRUE)) {
        first <- seq(0.001,0.01,by=0.001)
        firstre <- calc(nn=n,rr=r,g=first)
        if(!any(firstre>p & firstre<1,na.rm = TRUE)) {
          first <- seq(0.0001,0.001,by=0.0001)
          firstre <- calc(nn=n,rr=r,g=first)
          if(!any(firstre>p & firstre<1,na.rm = TRUE)) {
            first <- seq(0.00001,0.0001,by=0.00001)
            firstre <- calc(nn=n,rr=r,g=first)
            if(!any(firstre>p & firstre<1,na.rm = TRUE)) {
              stop("check calculation: specified range (0.00001-0.9) does not cover g")
            }
          }
        }
      }
    }
    index1 <- which(firstre-p>0 & firstre<1)
    if(length(index1)>=1) {
      index1 <- index1[length(index1)]
      lr <- first[c(index1,index1+1)]
    } else {
      index1 <- which(firstre-p<0 & firstre>0)
      index1 <- index1[1]
      lr <- first[c(index1-1,index1)]
    }
  } else {
    lr <- c(init-10^floor(log(init,10)),init+10^floor(log(init,10)))
  }
  
  if(lr[1]>lr[2]) lr <- c(lr[2],lr[1])
  
  lr <- c(lr[1],(lr[1]+lr[2])/2,lr[2])
  pnew <- p+0.05
  step <- 0
  while(abs(p-pnew)>tol & step < 1000) {
    step <- step+1
    temp <- calc(nn=n,rr=r,g=lr)
    pnew <- temp[2]
    if(abs(p-pnew)>tol) {
      if(temp[2]<p & temp[2]>0){
        lr <- c(lr[1],(lr[1]+lr[2])/2,lr[2])
      } else {
        lr <- c(lr[2],(lr[2]+lr[3])/2,lr[3])
      }
    }
  }
  return(lr[2])
}
