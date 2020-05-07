## ----warning=FALSE-------------------------------------------------------
suppressMessages(library(PML))
data(lis3)
pa3 <- form(lis3)
pa3

## ----fig.width=7,fig.height=5,out.width = "100%"-------------------------
data(pa3)
re <- bandSelect(df=pa3,Nlength=1440*3,Nlambda=100,alpha=1,Ntop=3,
                 cross=FALSE,Ncross=NULL,plot=TRUE)

## ----results="hide"------------------------------------------------------
freq <- data.frame(Frequency=re$freq,Proportion=colMeans(re$xprop))
rownames(freq) <- NULL
head(freq[order(freq$Proportion,decreasing = TRUE),],5)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(freq[order(freq$Proportion,decreasing = TRUE),],5),row.names = FALSE)

## ------------------------------------------------------------------------
## no significant frequencies
ob <- do.call("c",pa3$activity[1:4])
re <- test.harmonic(ob,p=0.05/(length(ob)-1)/2)
re$sig;head(re$fft) 

## ------------------------------------------------------------------------
## 3 significant frequencies
ob2 <- do.call("c",pa3$activity[11:13])
re2 <- test.harmonic(ob2,p=0.05/(length(ob2)-1)/2) 
re2$sig;head(re2$fft) 

## ------------------------------------------------------------------------
re3 <- test.harmonic(freq,p=5.79e-06,fft=TRUE)
print(re3$sig,digits=3,row.names=FALSE)

## ----eval=FALSE----------------------------------------------------------
#  #### directly display trelliscope panels, no dataset is returned
#  tre(lis3,plot.ind=TRUE,plot.tre=TRUE,plot.tre.path = tempdir())

## ----echo=FALSE,out.width = "100%"---------------------------------------
knitr::include_graphics("ind_plot.png")

## ----eval=FALSE----------------------------------------------------------
#  #### return a dataset with trelliscope panels
#  data(var3)
#  tre.ind <- suppressWarnings(tre(lis3,plot.ind=TRUE,varlis=var3))
#  
#  ## prepare for visualization; no lists are allowed
#  tre.ind$activity_ind <- tre.ind$activity_all <- NULL
#  trelliscopejs::trelliscope(tre.ind,name = "Day Activity Plot", nrow = 2, ncol = 2,path=tempdir())

## ----echo=FALSE,out.width = "100%"---------------------------------------
knitr::include_graphics("ind_plot_filter.png")

## ----eval=FALSE----------------------------------------------------------
#  tre(lis3,varlis=var3,plot.ind=FALSE,plot.tre=TRUE,plot.tre.path = tempdir())

## ----echo=FALSE,out.width = "100%"---------------------------------------
knitr::include_graphics("day_plot.png")

