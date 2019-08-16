#' trelliscope auxillary function
#'
#' Plot day observations.
#'
#' @param id individual ID.
#' @param id_Nday the ith day observation of the individual.
#' @param act_ori day observation.
#' @param act_ind individual mean observation.
#' @param act_all global mean observation.
#' @param act_max the upper limit of activity (y-axis).
#' @param band smoothing band paramter.
#'
#' @keywords internal
#'
#' @return individual to day observations.

#### activity plot
act_plot <- function(id,id_Nday,act_ori,act_ind,act_all,act_max,band,ori=F,lw=T) {
  timeOfDay <- function(num) {
    hour <- floor(num/60)
    min <- num - 60*hour
    min <- paste(0,min,sep="")
    min <- substr(min,nchar(min)-1,nchar(min))
    return(paste(hour,":",min,sep=""))
  }
  timeOfDay <- Vectorize(timeOfDay,vectorize.args="num")
  xtime <- timeOfDay(1:1440)

  len <- length(act_ind)
  xseq <- seq(24/len,24,by=24/len)
  d <- band*len
  lws <- function(x,ff) lowess(x[c((len-d+1):len,1:len,1:d)],f=ff,iter=1)$y[(d+1):(d+len)]
  vec <- lws(act_ori,band)
  vec_ind <- lws(act_ind,band)
  vec_all <- lws(act_all,band)
  #vec <- act_ori
  title_name <- paste("ID = ",id," #Day = ",id_Nday,sep="")

  if(lw==T & ori==F) {
    rbokeh::figure(title=title_name,legend_location = "top_left",
                   width=600, height=300,
                   #xlab="Time of the Day", ylab="Activity Count",
                   xlim=c(0,24), ylim=c(0, max(c(act_max,act_ind,act_all))+50),
                   tools=c("pan","wheel_zoom","box_zoom","resize",
                           "box_select","reset","save","help")) %>%
      #ly_points(xseq, act_ori, color="black", size=1, hover= "Time @xtime", alpha=0.5) %>%
      ly_lines(xseq,act_ori,color="black",alpha=0.2,legend=F) %>%
      ly_points(xseq,vec,color="white",size=2,alpha=0,
                hover=data.frame(Time=xtime,Activity=floor(vec)),legend=F) %>%
      ly_lines(xseq,vec,color="black",width=2,legend="activity lowess") %>%
      ly_lines(xseq,vec_ind,width=2,color="blue",legend="individual mean lowess") %>%
      ly_lines(xseq,vec_all,color="red",width=2,legend="global mean lowess") %>%
      x_axis(label="Time of the Day",
             desired_num_ticks = 10, num_minor_ticks= 2) %>%
      y_axis(label="Activity Count") %>%
      theme_axis(which=c("x","y"),axis_label_text_font = "Helvetica",
                 axis_label_text_font_size = "12pt", axis_label_text_font_style = "normal") %>%
      theme_title(text_font_size="12pt",text_font="Helvetica") %>%
      theme_legend(label_text_font_size = "8pt")
  } else if (lw==T & ori==T) {
    rbokeh::figure(title=title_name,legend_location = "top_left",
                   width=600, height=300,
                   #xlab="Time of the Day", ylab="Activity Count",
                   xlim=c(0,24), ylim=c(0, max(c(act_max,act_ind,act_all))+50),
                   tools=c("pan","wheel_zoom","box_zoom","resize",
                           "box_select","reset","save","help")) %>%
      #ly_points(xseq, act_ori, color="black", size=1, hover= "Time @xtime", alpha=0.5) %>%
      ly_lines(xseq,act_ori,color="black",alpha=0.2,legend=F) %>%
      ly_points(xseq,vec,color="white",size=2,alpha=0,
                hover=data.frame(Time=xtime,Activity=floor(vec)),legend=F) %>%
      ly_lines(xseq,act_ind,width=2,color="blue",alpha=0.2,legend="individual mean") %>%
      ly_lines(xseq,act_all,color="red",alpha=0.2,width=2,legend="global mean") %>%
      ly_lines(xseq,vec,color="black",width=2,legend="activity lowess") %>%
      ly_lines(xseq,vec_ind,width=2,color="blue",legend="individual mean lowess") %>%
      ly_lines(xseq,vec_all,color="yellow",width=2,legend="global mean lowess") %>%
      x_axis(label="Time of the Day",
             desired_num_ticks = 10, num_minor_ticks= 2) %>%
      y_axis(label="Activity Count") %>%
      theme_axis(which=c("x","y"),axis_label_text_font = "Helvetica",
                 axis_label_text_font_size = "12pt", axis_label_text_font_style = "normal") %>%
      theme_title(text_font_size="12pt",text_font="Helvetica") %>%
      theme_legend(label_text_font_size = "8pt")
  } else if(lw==F & ori==T) {
    rbokeh::figure(title=title_name,legend_location = "top_left",
                   width=600, height=300,
                   #xlab="Time of the Day", ylab="Activity Count",
                   xlim=c(0,24), ylim=c(0, max(c(act_max,act_ind,act_all))+50),
                   tools=c("pan","wheel_zoom","box_zoom","resize",
                           "box_select","reset","save","help")) %>%
      #ly_points(xseq, act_ori,color ="black", size=1, hover= "Time @xtime", alpha=0.5) %>%
      ly_lines(xseq,act_ori,color="black",alpha=0.2,legend=F) %>%
      ly_points(xseq,vec,color="white",size=2,alpha=0,
                hover=data.frame(Time=xtime,Activity=floor(vec)),legend=F) %>%
      ly_lines(xseq,act_ind,width=2,color="blue",alpha=0.2,legend="individual mean") %>%
      ly_lines(xseq,act_all,color="red",alpha=0.2,width=2,legend="global mean") %>%
      x_axis(label="Time of the Day",
             desired_num_ticks = 10, num_minor_ticks= 2) %>%
      y_axis(label="Activity Count") %>%
      theme_axis(which=c("x","y"),axis_label_text_font = "Helvetica",
                 axis_label_text_font_size = "12pt", axis_label_text_font_style = "normal") %>%
      theme_title(text_font_size="12pt",text_font="Helvetica") %>%
      theme_legend(label_text_font_size = "8pt")
  } else {
    stop("Error: at least plot one of the original and lowess!")
  }
}
