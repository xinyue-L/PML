#' Harmonic analysis test: p-value calculation
#'
#' This function calculates the p-value for the harmonic analysis test developed by R.A. Fisher (1929). Harmonic analysis specifically refers to Fast Fourier Transform (FFT) results.
#'
#' @param n the total number of frequencies in FFT results
#' @param r the modulus of the tested frequency is ranked as the rth largest among all frequencies
#' @param g the FFT result of the tested frequency expressed as the squared modulus divided by the sum of the squared moduli by all frequencies (proportion: m_r^2/(m_1^2+...+m_n^2)).
#'
#' @keywords harmonic test
#'
#' @return The p-value calculated by the harmonic test.

#' @examples
#' pharmonic(n=100,r=2,g=0.1)
#'
#' @seealso \code{\link{gharmonic}}
#'
#' @references Fisher, R. A. (1929). Tests of significance in harmonic analysis. Proceedings of the Royal Society of London. Series A, 125(796), 54-59.
#'
#' @export

pharmonic <- Vectorize(function(n,r,g) {
  re <- 0
  k <- ifelse(floor(1/g)==1/g,1/g-1,floor(1/g))
  term0 <- 0
  if(k<r) return(re)
  for (j in r:k) {
    if(1-j*g>0) {
      temp <- lfactorial(n)-lfactorial(r-1)+(n-1)*log(1-j*g)-log(j)-lfactorial(n-j)-lfactorial(j-r)
      if(exp(temp)==0) {
        term0 <- term0+1
        break
      }
      re <- re+(-1)^(j-r)*exp(temp)
    } else {
      temp <- lfactorial(n)-lfactorial(r-1)+(n-1)*log(j*g-1)-log(j)-lfactorial(n-j)-lfactorial(j-r)
      if(exp(temp)==0) {
        term0 <- term0+1
        break
      }
      re <- re+(-1)^(j-r+n-1)*exp(temp)
    }
  }
  return(re)
  },vec="g")
