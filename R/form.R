#' Function to generate activity data frame for penalized multi-band learning
#'
#' This function generates the data frame necessary for further penalized multi-band learning.
#' 
#' @importFrom"dplyr"
#' '%>%'
#' @importFrom"dplyr"
#' mutate
#' 
#' @param lis the list of activity data, with each element corresponding to the observation by one individual and the name of each element coresponding to the individual id. Specifically, each element is a \code{nob} by \code{nday} matrix, where each column is an observation by day.
#' @param maxday the maximal number of days per individual in the observation, used to check the data format. The default is 14.
#' @param id a vector of id names corresponding to the \code{lis} activity data.
#' 
#' @keywords trelliscope
#' 
#' @return The activity data frame with 3 columns: ID, IDday, and activity.

#' @examples
#' data(lis3)
#' pa3 <- form(lis3)
#' 
#' @seealso \code{\link{bandSelect}}
#' 
#' @export

form <- function(lis,maxday=14,id=NULL) {
  checkformat <- do.call("c",lapply(lis,ncol))
  if(length(checkformat)!=length(lis)) {
    stop("Contain empty matrix in the data list.")
  }
  if(max(checkformat)>maxday) {
    stop(paste("Maxday ",maxday," reached: data list format may be wrong / consider change maxday.",sep=""))
  }
  
  #### ID info
  ID <- list()
  if(!is.null(id)) {
    if(length(id)!=length(lis)) {
      stop("The length of the ID vector is not the same as the length of the data list.")
    }
    for (i in 1:length(id)) ID[[i]] <- rep(names(id[i]),ncol(lis[[i]]))
  } else {
    if(is.null(names(lis))) {
      stop("Names of the data list are null: ID should be supplied.")
    }
    for (i in 1:length(lis)) ID[[i]] <- rep(names(lis[i]),ncol(lis[[i]]))
  }
  
  #### ID_Nday info
  ID_Nday <- NULL #required to avoid NOTE in CMD check
  act <- data.frame(ID=as.numeric(unlist(ID)),
                      ID_Nday=unlist(lapply(ID,function(x) seq(1,length(x)))))
  act <- dplyr::tbl_df(act)
  act <- dplyr::group_by(act,ID,ID_Nday)
  act <- tidyr::nest(act)
  
  #### activity
  liscol <- do.call("cbind",lis)
  liscol2 <- list()
  for(i in 1:ncol(liscol)) liscol2[[i]] <- liscol[,i]
  act <- dplyr::mutate(act,activity=liscol2);rm(liscol,liscol2)
  act$data <- NULL
  
  return(act)
}

