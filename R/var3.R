#' Covariate data for three individuals
#'
#' This is a synthetic data for 3 individuals, and the variables are "ID", age" and "gender". It is for
#' illustration purposes of the functions in the package \code{accpa} only.
#'
#' @docType data
#'
#' @usage data(pa3)
#'
#' @format An object of class \code{data.frame}.
#'
#' @keywords datasets
#'
#' @examples
#' data(lis3)
#' data(var3)
#' 
#' #### individual mean activity plot: return a dataset with trelliscope panels
#' tre.ind <- tre(lis3,varlis=var3)
#' tre.ind$activity_ind <- tre.ind$activity_all <- NULL
#' tre.ind %>%
#'   trelliscope(name = "Day Activity Plot", nrow = 2, ncol = 2,path=getwd())
#'   
#' @seealso \code{\link{tre}}