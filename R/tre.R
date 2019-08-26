#' Trelliscope Visualization for Accelerometer Data
#'
#' This function generates the data frame necessary for trelliscope visualization.
#'
#' @importFrom"dplyr"
#' '%>%'
#' @importFrom"dplyr"
#' mutate
#' @importFrom"trelliscopejs"
#' trelliscope
#' @importFrom"trelliscopejs"
#' pmap_plot
#' @import"rbokeh"
#'
#' @param lis the list of activity data, with each element corresponding to the observation by one individual and the name of each element coresponding to the individual id. Specifically, each element is a \code{nob} by \code{nday} matrix, where each column is an observation by day.
#' @param id a vector of id names corresponding to the \code{lis} activity data.
#' @param varlis optional data frame to be merged to activity data, and the covariates are of interest for plotting to see activity differences. The first variables needs to be "ID".
#' @param smband smoothing parameter for plotting smoothed activity data. the default is 1/12 (see function \code{lowess}).
#' @param maxday maxday the maximal number of days per individual in the observation, used to check the data format. The default is 14.
#' @param plot.ind whether to plot individual mean activity plots. If not, plot day activity plots. The default is TRUE.
#' @param plot.ori whether to plot the original activity curves (tend to have large variations). The default is TRUE.
#' @param plot.sm whether to plot lowess of the activity curves. The default is TRUE.
#' @param plot.tre whether to generate trelliscope plots. If so, no data will be returned; if not, a data frame will be returned containing all information including trelliscope panels. To generate trelliscope based on the data, one needs to set all activity list columns to NULL. The default is FALSE.
#' @param plot.tre.path If plot.tre is TRUE, then plot.tre.path specifies the path to generate trelliscope files. The default is current working directory.
#' @keywords trelliscope
#'
#' @return The data frame including activity, filtering stats, optional covariates, and trelliscope panels. (No data frame will be returned if plot.tre is TRUE.)

#' @examples
#' \dontrun{
#' data(lis3)
#' data(var3)
#'
#' #### individual mean activity plot: return a dataset with trelliscope panels
#' tre.ind <- tre(lis3,varlis=var3)
#' tre.ind$activity_ind <- tre.ind$activity_all <- NULL
#' trelliscopejs::trelliscope(tre.ind,name = "Individual Mean Activity Plot", 
#' nrow = 2, ncol = 2,path=tempdir())
#' 
#' #### day activity plot: directly generating trelliscope visualization
#' tre(lis3,plot.ind=FALSE,plot.ori=FALSE,plot.tre=TRUE,plot.tre.path=tempdir())
#' } 
#'
#' @seealso \code{\link{form}}
#'
#' @export

tre <- function(lis,id=NULL,varlis=NULL,smband=1/12,maxday=14,plot.ind=TRUE,plot.ori=TRUE,plot.sm=TRUE,plot.tre=FALSE,plot.tre.path=NULL) {
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
  ID_Nday <- activity <- activity_ind <- activity_max <- activity_all <- NULL #required to avoid NOTE in CMD check
  act <- data.frame(ID=as.numeric(unlist(ID)),
                    ID_Nday=unlist(lapply(ID,function(x) seq(1,length(x)))))
  act <- dplyr::tbl_df(act)
  act <- dplyr::group_by(act,ID,ID_Nday)
  act <- tidyr::nest(act)

  #### activity
  liscol <- do.call("cbind",lis)
  liscol2 <- list()
  for(i in 1:ncol(liscol)) liscol2[[i]] <- liscol[,i]
  act <- dplyr::mutate(act,activity=liscol2);rm(liscol)
  act$data <- NULL
  ## smoothed activity
  act <- dplyr::mutate(act,activity_sm=lapply(act$activity,function(x) round(lowess(x[c((length(x)*(1-smband)+1):length(x),1:length(x),1:(length(x)*smband))],f=smband)$y[(length(x)*smband+1):(length(x)*(1+smband))],1)))
  ## individual mean activity
  act <- dplyr::mutate(act,activity_ind=lapply(ind_to_day(lapply(lis,rowMeans),act),function(x) round(x,1)))
  ## global mean activity
  mean_global <- round(rowMeans(do.call("cbind",act$activity_ind)),1)
  temp <- list()
  for(i in 1:nrow(act)) temp[[i]] <- mean_global
  act <- dplyr::mutate(act,activity_all=temp)
  rm(temp,mean_global)

  #### filter stats
  ##count_mean
  act$count_mean <- unlist(lapply(liscol2,mean))
  ##max number of consecutive zeros
  act$zero_consecmax <- unlist(lapply(act$activity,function(x) {
    re <- rle(x)
    if(sum(re$values==0)==0) {
      return(0) ##no zeros at all
    } else {
      return(max(re$lengths[re$values==0]))
    }
  }))
  ##total number of zeros
  act$zero_Nmax <- unlist(lapply(act$activity,function(x) sum(x==0)))

  ####plot individuals (average over days) or days
  if(plot.ind==TRUE) {
    act <- act[!duplicated(act$ID),]
    act$activity <- act$activity_sm <- NULL

    ### merge with other datasets
    if(!is.null(varlis)) {
      deltadf <- data.frame(ID=unique(act$ID))
      deltadf <- merge(deltadf,varlis,by="ID",all.x=TRUE)
      act <- cbind(act,deltadf[,-1]);rm(deltadf)
    }

    ### generate plot: ind vs global
    ptm <- Sys.time()
    message("Generating trelliscope individual plots... It may take some time.")
    ind <- act[!duplicated(act$ID),]
    ind <- dplyr::mutate(act,panel = trelliscopejs::pmap_plot(list(ind$ID,ind$activity_ind,
                                                                   ind$activity_all,smband,plot.ori,plot.sm), ind_plot))
    message(paste("Total time: ",round(difftime(Sys.time(),ptm,units="mins")[[1]],2)," mins",sep=""))

    ## trelliscope plot
    if(plot.tre==TRUE) {
      ind$activity_ind <- ind$activity_all <- NULL
      if(is.null(plot.tre.path)) {
        trelliscopejs::trelliscope(ind,name = "Individual Mean Activity Plot", nrow = 2, ncol = 2,
                      path=getwd())
      } else {
        trelliscopejs::trelliscope(ind,name = "Individual Mean Activity Plot", nrow = 2, ncol = 2,
                      path=plot.tre.path)
      }
    } else {
      return(ind)
    }

  } else {
    ## merge with other datasets
    if(!is.null(varlis)) {
      deltadf <- data.frame(ID=unique(act$ID))
      deltadf <- merge(deltadf,varlis,by="ID",all.x=TRUE)
      deltadf2 <- apply(deltadf,2,function(y) ind_to_day(x=y,df=act))
      act <- cbind(act,deltadf2[,-1]);rm(deltadf,deltadf2)
    }
    ## auxillary information for plotting: y-axis activity max
    act <- dplyr::mutate(act,activity_max = ind_to_day(unlist(lapply(split(act$activity_sm,act$ID),function(x)
        max(unlist(lapply(x,max))))),act))

    ## generate plot: day observation
    ptm <- Sys.time()
    message("Generating trelliscope activity day plots... It may take some time.")
    act <- dplyr::mutate(act,panel = trelliscopejs::pmap_plot(list(id=ID,id_Nday=ID_Nday,act_ori=activity,act_ind=activity_ind,
                                    act_all=activity_all,act_max=activity_max,band=smband,ori=plot.ori,lw=plot.sm), act_plot))
    message(paste("Total time: ",round(difftime(Sys.time(),ptm,units="mins")[[1]],3)," mins",sep=""))
    #check memory: format(object.size(act),units="Mb",standard="legacy")

    ## trelliscope plot
    if(plot.tre==TRUE) {
      act$activity <- act$activity_sm <- act$activity_ind <- act$activity_all <- NULL
      if(is.null(plot.tre.path)) {
        trelliscopejs::trelliscope(act,name = "Daily Activity Plot", nrow = 2, ncol = 2,
                      path=getwd())
      } else {
        trelliscopejs::trelliscope(act,name = "Day Activity Plot", nrow = 2, ncol = 2,
                      path=plot.tre.path)
      }
    } else {
      return(act)
    }
  }
}
