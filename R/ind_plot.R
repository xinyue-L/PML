#' trelliscope auxillary function
#'
#' plot individual observations.
#'
#' @param id individual ID.
#' @param act_ind individual mean observation.
#' @param act_all global mean observation.
#' @param band smoothing band paramter.
#'
#' @keywords internal
#'
#' @return individual to day observations.

#### individual plot
ind_plot <- function(id,act_ind,act_all,band,ori=T,lw=F) {
  timeOfDay <- function(num) {
    hour <- floor(num/60)
    min <- num - 60*hour
    min <- paste(0,min,sep="")
    min <- substr(min,nchar(min)-1,nchar(min))
    return(paste(hour,":",min,sep=""))
  }
  timeOfDay <- Vectorize(timeOfDay,vectorize.args = "num")
  xtime <- timeOfDay(1:1440)

  len <- length(act_ind)
  xseq <- seq(24/len,24,by=24/len)
  d <- band*len
  lws <- function(x,ff) lowess(x[c((len-d+1):len,1:len,1:d)],f=ff,iter=1)$y[(d+1):(d+len)]
  vec_ind_s <- lws(act_ind,band)
  vec_ind <- act_ind
  vec_all_s <- lws(act_all,band)
  delta <- act_ind-act_all
  df <- data.frame(Time=xtime,Individual_Mean=act_ind,Population_Mean=act_all,Delta=delta)
  title_name <- paste("ID = ",id, sep="")

  if(lw==F & ori==T) {
    rbokeh::figure(title=title_name,legend_location = "top_left",
                   width=600, height=300,
                   #xlab="Time of the Day", ylab="Activity Count",
                   xlim=c(0,24), ylim=c(0, max(c(act_ind,act_all))+50),
                   tools=c("pan","wheel_zoom","box_zoom","resize",
                           "box_select","reset","save","help")) %>%

      ly_lines(xseq,vec_ind,width=1,color="blue",alpha=0.2,legend="ind mean") %>%
      ly_points(xseq,vec_ind_s,color="white",size=2,alpha=0,
                hover=df,legend=F) %>%
      ly_lines(xseq,act_all,color="red",width=2,legend="global mean") %>%
      x_axis(label="Time of the Day",
             desired_num_ticks = 10, num_minor_ticks= 2) %>%
      y_axis(label="Activity Count") %>%
      theme_axis(which=c("x","y"),axis_label_text_font = "Helvetica",
                 axis_label_text_font_size = "12pt", axis_label_text_font_style = "normal") %>%
      theme_title(text_font_size="12pt",text_font="Helvetica") %>%
      theme_legend(label_text_font_size = "8pt")
  } else if(lw==T & ori==T) {
    rbokeh::figure(title=title_name,legend_location = "top_left",
                   width=600, height=300,
                   #xlab="Time of the Day", ylab="Activity Count",
                   xlim=c(0,24), ylim=c(0, max(c(act_ind,act_all))+50),
                   tools=c("pan","wheel_zoom","box_zoom","resize",
                           "box_select","reset","save","help")) %>%

      ly_lines(xseq,vec_ind,width=1,color="blue",alpha=0.2,legend="ind mean") %>%
      ly_points(xseq,vec_ind_s,color="white",size=2,alpha=0,
                hover=df,legend=F) %>%
      ly_lines(xseq,vec_ind_s,width=2,color="blue",legend="ind mean lowess") %>%
      ly_lines(xseq,act_all,color="red",width=2,legend="global mean") %>%
      ly_lines(xseq,vec_all_s,color="yellow",width=2,legend="global mean lowess") %>%
      x_axis(label="Time of the Day",
             desired_num_ticks = 10, num_minor_ticks= 2) %>%
      y_axis(label="Activity Count") %>%
      theme_axis(which=c("x","y"),axis_label_text_font = "Helvetica",
                 axis_label_text_font_size = "12pt", axis_label_text_font_style = "normal") %>%
      theme_title(text_font_size="12pt",text_font="Helvetica") %>%
      theme_legend(label_text_font_size = "8pt")
  } else if(lw==T & ori==F) {
    rbokeh::figure(title=title_name,legend_location = "top_left",
                   width=600, height=300,
                   #xlab="Time of the Day", ylab="Activity Count",
                   xlim=c(0,24), ylim=c(0, max(c(act_ind,act_all))+50),
                   tools=c("pan","wheel_zoom","box_zoom","resize",
                           "box_select","reset","save","help")) %>%

      ly_lines(xseq,vec_ind,width=1,color="blue",alpha=0.2,legend="ind mean") %>%
      ly_points(xseq,vec_ind_s,color="white",size=2,alpha=0,
                hover=df,legend=F) %>%
      ly_lines(xseq,vec_ind_s,width=2,color="blue",legend="ind mean lowess") %>%
      ly_lines(xseq,vec_all_s,color="yellow",width=2,legend="global mean lowess") %>%
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

