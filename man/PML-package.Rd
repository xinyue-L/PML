\name{PML-package}
\alias{PML-package}
\alias{PML}
\docType{package}
\title{
\packageTitle{PML}
}
\description{
\packageDescription{PML}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{PML}
\packageIndices{PML}
Penalized Multi-Band Learning algorithm can be effectively implemented for circadian rhythm analysis and daily activity pattern characterization using actigraphy (continuously measured objective physical activity data). Functions for interactive visualization of actigraph data are also included. 
}
\author{
\packageAuthor{PML}

Maintainer: \packageMaintainer{PML}
}
\references{
Li, X., Kane, M., Zhang, Y., Sun, W., Song, Y., Dong, S., Lin, Q., Zhu, Q., Jiang, F., Zhao, H. (2019) A Novel Penalized Multi-band Learning Approach Characterizes the Consolidation of Sleep-Wake Circadian Rhythms During Early Childhood Development.

Fisher, R. A. (1929). Tests of significance in harmonic analysis. Proceedings of the Royal Society of London. Series A, 125(796), 54-59.
}
\keyword{PML}

\examples{
library(PML)
##reformat data for further analysis
data(lis3)
pa3 <- form(lis3)

##apply Penalized Multi-band Learning
data(pa3)
re <- bandSelect(df=pa3,Nlength=1440*3,Nlambda=100,alpha=1,Ntop=3,cross=FALSE,Ncross=NULL,plot=TRUE)

##use trelliscope to visualize data:
##return a dataset with trelliscope panels for individual mean activity plots
data(var3)
tre.ind <- tre(lis3,varlis=var3)
tre.ind$activity_ind <- tre.ind$activity_all <- NULL

}
