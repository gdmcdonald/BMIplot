require(dplyr)
require(ggplot2)

#Define the standard BMI region boundaries
BMIHeightLines<-function(min_weight,max_weight){
  data.frame(x=floor(min_weight):ceil(max_weight))%>%
  mutate(y0=0*x,
         yHealthyMax=sqrt(x/18.5)*100,
         yOverweightMax=sqrt(x/25)*100,
         yObeseMax=sqrt(x/30)*100,
         y500=0*x+500)
}

#Define where the labels should go
text_out<-function(num,min_weight,max_weight,min_ht,max_ht,aspect_ratio){
  height_fraction=c(0.75,0.6,0.4,0.25)
  label_BMIs<-c(14,21.5,27.5,36)
  curve<-data.frame(xc=floor(min_weight):ceil(max_weight))%>%
    mutate(yc=sqrt(xc/label_BMIs[num])*100)%>%
    filter(yc>min_ht,yc<max_ht)
  
  coords<-curve%>%
    summarise(x=quantile(xc,height_fraction[num],names=FALSE),
              y=quantile(yc,height_fraction[num],names=FALSE))%>%
    mutate(ang=180/pi*atan(aspect_ratio*50/sqrt(label_BMIs[num]*x)))
  return(coords)
}

#Define a function to add the background and labels to a ggplot object
addBMIBackgroud<-function(my_plot,aspect_ratio=1,textTop=TRUE,jitter=FALSE,alpha=FALSE){
  xlims<-ggplot_build(my_plot)$layout$panel_ranges[[1]]$x.range
  ylims<-ggplot_build(my_plot)$layout$panel_ranges[[1]]$y.range
  df<-BMIHeightLines(xlims[1],xlims[2])
  t1<-text_out(1,xlims[1],xlims[2],ylims[1],ylims[2],aspect_ratio)
  t2<-text_out(2,xlims[1],xlims[2],ylims[1],ylims[2],aspect_ratio)
  t3<-text_out(3,xlims[1],xlims[2],ylims[1],ylims[2],aspect_ratio)
  t4<-text_out(4,xlims[1],xlims[2],ylims[1],ylims[2],aspect_ratio)
  
  print(t1)
  
  if(textTop==FALSE){
  my_plot$layers<-c(geom_ribbon(data=df, aes(x=x,ymin = y500, ymax = yHealthyMax), fill = "yellow", alpha=0.3),
                    geom_ribbon(data=df, aes(x=x,ymin = yHealthyMax, ymax = yOverweightMax), fill = "green", alpha=0.3),
                    geom_ribbon(data=df, aes(x=x,ymin = yOverweightMax, ymax = yObeseMax), fill = "orange", alpha=0.3),
                    geom_ribbon(data=df, aes(x=x,ymin = y0, ymax = yObeseMax), fill = "red", alpha=0.3),
                    geom_text(data=t1,aes(x=x[1],y=y[1],angle=ang[1],label="Underweight",fontface="bold"),color="black"),
                    geom_text(data=t2,aes(x=x[1],y=y[1],angle=ang[1],label="Healthy",fontface="bold"),color="black"),
                    geom_text(data=t3,aes(x=x[1],y=y[1],angle=ang[1],label="Overweight",fontface="bold"),color="black"),
                    geom_text(data=t4,aes(x=x[1],y=y[1],angle=ang[1],label="Obese",fontface="bold"),color="black"),
                    my_plot$layers)
  }else{
    my_plot$layers<-c(geom_ribbon(data=df, aes(x=x,ymin = y500, ymax = yHealthyMax), fill = "yellow", alpha=0.3),
                      geom_ribbon(data=df, aes(x=x,ymin = yHealthyMax, ymax = yOverweightMax), fill = "green", alpha=0.3),
                      geom_ribbon(data=df, aes(x=x,ymin = yOverweightMax, ymax = yObeseMax), fill = "orange", alpha=0.3),
                      geom_ribbon(data=df, aes(x=x,ymin = y0, ymax = yObeseMax), fill = "red", alpha=0.3),
                      my_plot$layers,
                      geom_text(data=t1,aes(x=x[1],y=y[1],angle=ang[1],label="Underweight",fontface="bold"),color="black"),
                      geom_text(data=t2,aes(x=x[1],y=y[1],angle=ang[1],label="Healthy",fontface="bold"),color="black"),
                      geom_text(data=t3,aes(x=x[1],y=y[1],angle=ang[1],label="Overweight",fontface="bold"),color="black"),
                      geom_text(data=t4,aes(x=x[1],y=y[1],angle=ang[1],label="Obese",fontface="bold"),color="black"))
  }
  my_plot<-my_plot+coord_cartesian(ylim=c(ylims[1],ylims[2]),
                           xlim=c(xlims[1],xlims[2]))
  return(my_plot)
}

#Define a function to create the BMI plot from a dataframe with default parameters
makeBMIplot<-function(bmidata,weight="Weight",height="Height",colFactor=NULL,aspect_ratio=NULL,jitter=FALSE,alpha=1,textTop=TRUE)
{
  if (is.null(aspect_ratio)){aspect_ratio<-3-1.15*is.null(colFactor)}
  
  if(jitter==TRUE)
  {
    p1<-ggplot()+
      xlab("Weight (kg)")+ylab("Height (cm)")+
      ggtitle("People by Height, Weight and BMI")+theme_classic()+
      geom_jitter(data=bmidata,
                  aes_string(x = weight,
                             y = height,
                             color = colFactor,
                             shape = colFactor),
                  alpha=alpha)
    return(addBMIBackgroud(p1,aspect_ratio=aspect_ratio,textTop=textTop))
  }else{
    p1<-ggplot()+
      xlab("Weight (kg)")+ylab("Height (cm)")+
      ggtitle("People by Height, Weight and BMI")+theme_classic()+
      geom_point(data=bmidata,
                 aes_string(x = weight,
                            y = height,
                            color = colFactor,
                            shape = colFactor),alpha=alpha)
    return(addBMIBackgroud(p1,aspect_ratio=aspect_ratio,textTop=textTop))
  }
  
}

# ###########
# #Examples
# ###########
# 
# #Set up demo data set
# demo_data=read.csv("http://people.ucsc.edu/~cdobkin/NHIS%202007%20data.csv")%>%
#   mutate(Weight=weight/2.20462, #convert pounds to kg
#          Height=height*2.54,    #convert inches to cm
#          Gender=recode(SEX,`1`="Male",`2`="Female"))%>%
#   filter(Height<200,Weight<200) #remove ridiculous entries
# 
# 
# #Specify lots
# makeBMIplot(sample_n(demo_data,100),weight="Weight",height="Height",colFactor="Gender",aspect_ratio = 3)
# 
# #Specify colour factor
# makeBMIplot(sample_n(demo_data,100),colFactor="Gender")
# 
# #Don't specify colour factor
# makeBMIplot(sample_n(demo_data,100),alpha=0.3)
# 
# #add jitter and alpha to counteract overplotting
# makeBMIplot(demo_data,weight="Weight",height="Height",colFactor="Gender",aspect_ratio = 3,jitter=TRUE,alpha=0.4)
# 
