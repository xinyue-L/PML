#' Activity data for three individuals
#'
#' A synthetic data list of three elements, each consisting of an activity matrix for one individual, and
#' each column of an activity matrix is one-day activity observation. Therefore, an activity matrix is
#' \code{nob} by \code{nday}. It is a named list, the name of which is the individual ID. It is for illustration 
#' purposes of the functions in the package \code{PML} only.
#'
#' @docType data
#'
#' @usage data(lis3)
#'
#' @format An object of class \code{list}.
#'
#' @keywords datasets
#'
#' @examples
#' data(lis3)
#' pa3 <- form(lis3)
#' 
#' @seealso \code{\link{form}}, \code{\link{pa3}}