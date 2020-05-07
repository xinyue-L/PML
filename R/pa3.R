#' Activity tbl_df data for three individuals
#'
#' A synthetic data in tbl_df format. It has 13 observations for 3 individuals, and the variables are "ID", 
#' "ID_Nday" (the ith day observation for an individual), and activity. The activity variable is an embedded
#' list with each element consisting of a vector of one-day observation. It is for illustration purposes of
#' the functions in the package \code{PML} only.
#'
#' @docType data
#'
#' @usage data(pa3)
#'
#' @format An object of classes \code{tbl_df}, \code{tbl}, \code{data.frame}.
#'
#' @keywords datasets
#'
#' @examples
#' data(pa3)
#' re <- bandSelect(df=pa3,Nlength=1440*3,Nlambda=100,alpha=1,Ntop=5,cross=FALSE,Ncross=NULL,plot=TRUE)
#' 
#' @seealso \code{\link{bandSelect}}