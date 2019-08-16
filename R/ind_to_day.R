#' trelliscope auxillary function
#'
#' Take individual characteristics and map it to individual-day observations. For example, individual A/B is 40/45 
#' years old and has 4/2 day observations in dataset D. Therefore, mapping age to dataset D generates 
#' 40, 40, 40, 40, 45, 45 for day 1,2,3,4/1,2 observation of A/B respectively.
#' 
#' @param  x covariates data for individuals to be merged.
#' @param df day observation dataset.
#' 
#' @keywords internal
#' 
#' @return individual to day observations.

ind_to_day <- function(x,df) {
  if(is.list(x)) {
    temp <- unlist(lapply(split(df$ID,df$ID),length))
    temp2 <- do.call("cbind",x)
    re <- list()
    for(i in 1:length(temp)) {
      re[[i]] <- temp2[,rep(i,temp[i])]
    }
    re <- do.call("cbind",re)
    temp <- list()
    for (i in 1:ncol(re)) temp[[i]] <- re[,i]
    return(temp)
    
  } else if (is.vector(x)) {
    temp <- unlist(lapply(split(df$ID,df$ID),length))
    temp <- cbind(temp,x)
    temp <- unlist(apply(temp,1,function(x) rep(x[2],x[1])))
    names(temp) <- NULL
    temp <- as.numeric(temp)
    return(temp)
  }
}
