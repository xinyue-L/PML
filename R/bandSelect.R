#' Penalized multi-band learning function
#'
#' In a group of individuals with physcial activity data, this function utilize Fast Fourier Transform (FFT)
#' and L-1/L-2 penalties to select significant harmonics/periodicities and describe the main activity pattern
#' (circadian rhythm) among the population.
#'
#' @param df the tbl_df data frame containing at least two variables: subject ID and activity. The function \code{form} can help prepare the data frame.
#' @param Nlength the length of observations necessary for each individual, note that it should be consistent among all
#' @param Nlambda \eqn{\lambda}'s take values from 0 to 2max(||X_k||^2), as 0 gives no penalty and the latter suppresses all \eqn{\theta}'s to 0. Therefore, we divide 2max(||X_k||^2) into Nlambda (default to be 100) \eqn{\lambda}'s to pick frequencies/harmonics/periodicities.
#' @param alpha the tuning parameter controlling the balance between L-1 and L-2 penalty. The default is 1, using complete Lasso/ L-1 penalty.
#' @param Ntop the number of frequencies/harmonics/periodicities picked for the population. The default is 5.
#' @param plot whether to plot: MSE against the number of nonzero \eqn{\theta}'s, and only the points at which the number of nonzero \eqn{\theta}'s changes (as \eqn{\lambda} changes) are be plotted. The default is TRUE.
#' @param cross whether to perform cross-validation. The default is FALSE.
#' @param Ncross the number of groups of data for cross-validation. If cross=TRUE, the data shall be divided into Ncross groups.
#' @keywords learning, penalty, harmonics, periodicity
#' 
#' @return if no cross-validation is conducted, return a list; if cross-validation, return a list of lists, with the last list consisting of all FFT results and cross-validation groups (showing the subject IDs leave-out /NOT used each time).
#' @return \item{topfreq}{vector of length \code{Ntop}: top frequencies selected.}
#' @return \item{mse}{vector of length \code{Nlambda}: mean squared error for each lambda (penalty). If no cross-validation, mse is calculated based on all available data; if cross-validation, mse is calculated based on the rest observations.}
#' @return \item{nonzero}{vector of length \code{Nlambda}: the number of nonzero \eqn{\theta}'s (frequencies) for each lambda (penalty).}
#' @return \item{deltazero}{vector of length \code{Nlambda}: the change in the number of nonzero \eqn{\theta}'s (frequencies) for each lambda (penalty).}
#' @return \item{lambda}{vector of length \code{Nlambda}: the value of lambda.}
#' @return \item{theta}{\code{Nfreq} by \code{Nlambda} matrix: estimated \eqn{\theta}'s (frequencies) at each lambda (penalty). Nfreq is the total number of frequencies given by FFT.}
#' @return \item{xscore}{\code{Nind} by \code{Nfreq} matrix: the original FFT scores for each individual. Nind is the number of individuals in the population, and Nfreq is the total number of frequencies given by FFT.}
#' @return \item{xprop}{\code{Nind} by \code{Nfreq} matrix: the original FFT results expressed as the proportion of variances explained by each frequency for each individual. Nind is the number of individuals in the population, and Nfreq is the total number of frequencies given by FFT.}
#' @return \item{freq}{vector of length \code{Nfreq}: list of frequencies in FFT results.}
#'
#' @examples
#' data(pa3)
#' re <- bandSelect(df=pa3,Nlength=1440*3,Nlambda=100,alpha=1,Ntop=5,cross=FALSE,Ncross=NULL,plot=TRUE)
#'
#' @seealso \code{\link{form}}
#' 
#' @references Li, X. , Kane, M. , Zhang, Y. , Sun, W. , Song, Y. , Dong, S. , Lin, Q. , Zhu, Q. , Jiang, F. & Zhao, H. (2019). Penalized Selection of Periodicities Characterizes the Consolidation of Sleep-Wake Circadian Rhythms During Early Childhood Development. Submitted.
#'
#' @export
#'

bandSelect <- function(df,Nlength,Nlambda=100,alpha=1,Ntop=5,cross=FALSE,Ncross=NULL,plot=TRUE) {
  ### preparation
  # Note: requre harmonic_func.R
  dflist <- lapply(split(df$activity,df$ID),unlist)
  re <- list()
  for(i in 1:length(dflist)) {
    if(length(dflist[[i]])<Nlength) next

    X.k <- fft(dflist[[i]][1:Nlength])
    X.k <- fft.harmonic(X.k)
    X.k <- X.k[order(X.k[,1]),]
    re[[length(re)+1]] <- X.k
  }
  xscore <- do.call("rbind",lapply(re,function(x) x[,2]))
  xprop <- do.call("rbind",lapply(re,function(x) x[,4]))
  print(paste("With a threshold of ",Nlength," observations per individual, "))
  print(paste(nrow(xscore)," individuals are used for penalized multi-band learning.",sep=""))

  calc <- function(x_sq,alph,lambda) max((2*x_sq-alph*lambda)/(2*x_sq+(1-alph)*lambda),0)
  calc <- Vectorize(calc,vectorize.args ="x_sq")

  ##cross-validation or not
  if(cross==TRUE | !is.null(Ncross)) {
    ####group
    Ng <- Ncross
    if(nrow(xprop)<Ng) {
      stop("The number of qualified observations is fewer than the number of groups!")
    }
    group <- function(xlength,ngroup) {
      n1 <- floor(xlength/ngroup)
      re <- list()
      numbers <- sample(1:xlength,xlength)
      if(ngroup==1) return(list(numbers))
      for(i in 1:(ngroup-1)) {
        re[[i]] <- numbers[(n1*(i-1)+1):(n1*(i))]
      }
      re[[ngroup]] <- numbers[(n1*(ngroup-1)+1):length(numbers)]
      return(re)
    }

    cats <- group(nrow(xprop),Ng)
    re_cross <- list()

    for (k in 1:Ng) {
      ##selected
      gp <- k
      xtemp <- xprop[-cats[[gp]],]
      xtheta <- data.frame(freq=re[[1]][,3],
                           xprop=apply(xtemp,2,function(x) sum(x^2)))
      lambdamax <- 2*max(xtheta$xprop)
      nlambda <- Nlambda
      reall <- matrix(nrow=nrow(xtheta),ncol=nlambda)
      for(i in 1:ncol(reall)) {
        reall[,i] <-  calc(xtheta$xprop,alph=alpha,lambda=lambdamax/nlambda*i)
      }
      nzeros <- apply(reall,2,function(x) sum(x==0))
      ##unselected for cross-validation
      mse <- numeric(nlambda)
      xother <- xprop[cats[[gp]],]

      for(i in 1:length(mse)) {
        xother_hat <- xother*rep(reall[,i],each=nrow(xother))
        mse[i] <- sum((xother-xother_hat)^2)
      }

      ##transitional points
      index <- which(nzeros[-length(nzeros)]-nzeros[-1] !=0)
      reindex <- c()
      temp_cont <- c()
      for(i in 1:Ntop) {
        temp <- xtheta$freq[which(reall[,index[length(index)+1-i]]!=0)]
        reindex <- c(reindex,temp[which(!temp %in% temp_cont)])
        temp_cont <- xtheta$freq[which(reall[,index[length(index)+1-i]]!=0)]
      }

      nonzero <- nrow(reall)-nzeros
      deltazero <- c(0,nonzero[-length(nonzero)]-nonzero[-1])
      #reindex #important frequencies
      re_cross[[k]] <- list(topfreq=reindex,mse=mse,nonzero=nonzero,deltazero=deltazero,
                            lambda=lambdamax/nlambda*1:nlambda,theta=reall)
    }
    names(re_cross) <- paste("group",1:length(re_cross),sep="")
    re_cross$fft <- list(xscore=xscore,xprop=xprop,group=cats,freq=X.k[,3]) ##all observation results
    return(re_cross)
  } else {
    ### start calculation
    xtheta <- data.frame(freq=re[[1]][,3],
                         xprop=apply(xprop,2,function(x) sum(x^2)))
    lambdamax <- 2*max(xtheta$xprop)
    nlambda <- Nlambda
    reall <- matrix(nrow=nrow(xtheta),ncol=nlambda)
    for(i in 1:ncol(reall)) {
      reall[,i] <-  calc(xtheta$xprop,alph=alpha,lambda=lambdamax/nlambda*i) ### L1/L2 penalty
    }
    nzeros <- apply(reall,2,function(x) sum(x==0))

    ### MSE
    mse <- numeric(nlambda)
    for(i in 1:length(mse)) {
      xprop_hat <- xprop*rep(reall[,i],each=nrow(xprop))
      mse[i] <- sum((xprop-xprop_hat)^2)
    }

    ##transitional points
    index <- which(nzeros[-length(nzeros)]-nzeros[-1] !=0)
    reindex <- c()
    temp_cont <- c()
    for(i in 1:Ntop) {
      temp <- xtheta$freq[which(reall[,index[length(index)+1-i]]!=0)]
      reindex <- c(reindex,temp[which(!temp %in% temp_cont)])
      temp_cont <- xtheta$freq[which(reall[,index[length(index)+1-i]]!=0)]
    }

    nonzero <- nrow(reall)-nzeros
    deltazero <- c(0,nonzero[-length(nonzero)]-nonzero[-1])

    if(plot==T) {
      plot(nonzero[deltazero!=0],mse[deltazero!=0],
           xlab=expression(paste("Number of Nonzero ", theta, "'s",sep="")),ylab="MSE")
    }

    #reindex #important frequencies
    re_cross <- list(topfreq=reindex,mse=mse,nonzero=nonzero,deltazero=deltazero,
                     lambda=lambdamax/nlambda*1:nlambda,theta=reall,
                     ### original results
                     xscore=xscore,xprop=xprop,freq=X.k[,3])

    return(re_cross)
  }
}
