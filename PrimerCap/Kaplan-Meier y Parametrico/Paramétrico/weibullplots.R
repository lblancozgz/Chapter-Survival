library(ggplot2)
library(tidyverse)
library(readxl)
library(patchwork)
library(lattice)
library(lubridate)
library(RColorBrewer)
library(readxl)
library(ggpubr)
library(rstatix)
setwd("//home/laurablanco/Doctorado/Trabajo_2021/season_2021")
wei_week<- read_excel("/home/laurablanco/Doctorado/Trabajo_2021/season_2021/Weibull_week.xlsx", 
                           col_types = c("numeric", "text", "text", 
                                         "numeric", "numeric"))
wei_weeksc_plot <- ggplot(data = wei_week, 
                       aes(x = week,
                           y = scale)) +
  geom_point(aes(color = location, 
                 shape = method),
             size = 3,
             alpha = 0.8) +
  geom_line(aes(color = location, 
                shape = method), size = 0.45) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Weibull scale, total lived by mosquitoes along the season",
       subtitle = "No weather variables included here",
       x = "Time (weeks)",
       y = "Scale (η)",
       color = "Location",
       shape = "Method of capture") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  theme_bw()

wei_weeksc_plot + scale_x_continuous(breaks = c(25,26,27,28,29,30,31,32,33,34,35,
                                                36,37,38,39,40,41,42,43,44,45))

weifaced1 <- wei_weeksc_plot + facet_grid(location ~ method, scales = "fixed")
weifaced1+ scale_x_continuous(breaks = c(25,26,27,28,29,30,31,32,33,34,35,
                                        36,37,38,39,40,41,42,43,44,45))

wei_weeksc_plot <- ggplot(data = wei_week, 
                          aes(x = week,
                              y = scale)) +
  geom_point(aes(color = method, shape = method),
             size = 3,
             alpha = 0.8) +
  geom_line(aes(color = method), size = 0.45) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Weibull scale, total lived by mosquitoes along the season",
       subtitle = "No weather variables included here",
       x = "Time (weeks)",
       y = "Scale (η)",
       color = "Method of capture") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  theme_bw()
weifaced2 <- wei_weeksc_plot + facet_grid(~location, scales = "fixed")
weifaced2+ scale_x_continuous(breaks = c(25,26,27,28,29,30,31,32,33,34,35,
                                         36,37,38,39,40,41,42,43,44,45))

wei_weeksc_plot + scale_x_continuous(breaks = c(25,26,27,28,29,30,31,32,33,34,35,
                                                36,37,38,39,40,41,42,43,44,45))

wei_weeksh_plot <- ggplot(data = wei_week, 
                          aes(x = week,
                              y = shape)) +
  geom_point(aes(color = location, 
                 shape = method),
             size = 3,
             alpha = 0.8) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Weibull shape, total lived by mosquitoes along the season",
       subtitle = "No weather variables included here",
       x = "Time (weeks)",
       y = "Shape (β)",
       color = "Location",
       shape = "Method of capture") +
  theme(legend.position = c(0.2, 0.7),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") + 
  theme_minimal()

wei_weeksh_plot

#Urban BG
curve(dweibull(x, shape=2.943411, scale = 3.75347), from=0, to=65, col='pink', ylim = c(0,0.3))
curve(dweibull(x, shape=1.398958, scale = 5.231901), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.43996, scale = 14.29082), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=0.9231326, scale = 5.7587355), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.016116, scale = 8.809591), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.30612, scale = 13.24956), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=0.9612027, scale = 20.5702607), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.542637, scale = 23.433393), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=0.8229461, scale = 18.0159551), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.560648, scale = 22.927822), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=0.7973819, scale = 14.6430820), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.296549, scale = 29.785581), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.323143, scale = 26.585871), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.071938, scale = 19.316290), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.309941, scale = 18.921714), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.622437, scale = 26.352340), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.132697, scale = 15.239922), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.277628, scale = 16.363839), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=1.051868, scale = 12.356797), from=0, to=65, col='pink', add=TRUE)
curve(dweibull(x, shape=0.8956078, scale = 16.2671083), from=0, to=65, col='pink', add=TRUE)
mean(urbanbg$scale)
curve(dweibull(x, shape=1.284716, scale = 16.5937), from=0, to=65, col='#D8007A',lwd = 2, add=TRUE)

#Urban HB
curve(dweibull(x, shape=4.75067, scale = 37.74245), from=0, to=85, col='pink', ylim = c(0,0.15))
curve(dweibull(x, shape=1.700909, scale = 29.363671), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=2.121546, scale = 30.442421), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.574665, scale = 39.860143), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=8.54401, scale = 45.64307), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=2.595175, scale = 32.819266), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=2.188417, scale = 40.158568), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=2.209929, scale = 24.687006), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=10.45242, scale = 36.80189), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=0.947406, scale = 17.474804), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=2.185543, scale = 14.297458), from=0, to=85, col='pink', add=TRUE)
mean(urbanhb$scale)
curve(dweibull(x, shape=3.570063, scale = 31.7537), from=0, to=85, col='#D8007A',lwd = 2, add=TRUE)


#PeriUrban BG
curve(dweibull(x, shape=1.251876, scale = 20.947946), from=0, to=85, col='pink', ylim = c(0,0.15))
curve(dweibull(x, shape=1.600479, scale = 24.441497), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=0.8468364, scale = 10.6335386), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=0.8320795, scale = 9.3418840), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.089248, scale = 15.889203), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=0.9190215, scale = 21.6123547), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=0.8966453, scale = 9.5613264), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.563222, scale = 22.731951), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.197434, scale = 17.088519), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.373729, scale = 23.639422), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=0.9564166, scale = 13.8717521), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.045457, scale = 13.149772), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.671611, scale = 14.660381), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.294091, scale = 18.268343), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.204845, scale = 9.313416), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.029823, scale = 11.251011), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.135504, scale = 9.547003), from=0, to=85, col='pink', add=TRUE)
mean(peribg$shape)
curve(dweibull(x, shape=1.171078, scale = 15.64408), from=0, to=85, col='#D8007A',lwd = 2, add=TRUE)

#Peri Urban HB
curve(dweibull(x, shape=1.029793, scale = 11.119225), from=0, to=85, col='pink', ylim = c(0,0.1))
curve(dweibull(x, shape=1.863708, scale = 24.637444), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.178608, scale = 28.572757), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=4.441668, scale = 42.350759), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.488962, scale = 19.173266), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.592778, scale = 39.695345), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.46492, scale = 23.80346), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.511113, scale = 31.422719), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.317806, scale = 10.929866), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.09272, scale = 10.72452), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.338228, scale = 24.529614), from=0, to=85, col='pink', add=TRUE)
curve(dweibull(x, shape=1.948333, scale = 16.658845), from=0, to=85, col='pink', add=TRUE)
mean(perihb$shape)
curve(dweibull(x, shape=1.689053, scale = 23.63482), from=0, to=85, col='#D8007A',lwd = 2, add=TRUE)

#We are going to do the parameter space plot only with one location and method
urbanbg <- wei_week[!(wei_week$location=="Peri_Urban" | wei_week$method=="HB"),]
urbanhb <- wei_week[!(wei_week$location=="Peri_Urban" | wei_week$method=="BG"),]
peribg <- wei_week[!(wei_week$location=="Urban" | wei_week$method=="HB"),]
perihb <- wei_week[!(wei_week$location=="Urban" | wei_week$method=="BG"),]

library(paletteer)
urbanbg$week <- as.factor(urbanbg$week)
plot <- urbanbg %>%
  ggplot(aes(shape,scale)) +
  geom_point(aes(color=week)) + geom_path(aes(colour = week), group = 1) + theme_bw() 
plot + scale_color_manual(labels = c(25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44),
                            values = c("#860000", "#860000", "#860000", "#FB000D", "#FB000D","#FB000D",
                                       "#FD343E", "#FD343E", "#FF786D", "#FF786D", "#86B0D6", "#86B0D6",
                                       "#86B0D6", "#3A87CC", "#3A87CC", "#3A87CC", "#0859A1", "#0859A1",
                                       "#06447C",  "#06447C"))


#mean weibulls
meanwei <- c()
mean1 <- 3.753470* gamma(1+1/2.9434110)
mean_weibull <- function(shape,scale){
  (1/scale)*gamma(1+(1/shape))
}

for(i in wei_week$week){
  mean1 <- mean_weibull(wei_week$shape, wei_week$scale)
  mean1<- as.data.frame(mean1)
  meanwei <- cbind(wei_week, mean1)
}

wei_mean_plot <- ggplot(data = meanwei, 
                          aes(x = week,
                              y = mean1)) +
  geom_point(aes(color = as.factor(week), 
                 shape = method),
             size = 3,
             alpha = 0.8) +
  geom_line(aes(color = as.factor(week), group = 1, 
                shape = method), size = 0.45) +
  scale_color_viridis_d() +
  labs(title = "Weibull mean, total lived by mosquitoes along the season",
       subtitle = "No weather variables included here",
       x = "Time (weeks)",
       y = "Mean (E(X))") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") 

weifaced1 <- wei_mean_plot + facet_grid(location ~ method, scales = "fixed")

#varianza weibull

var_weibull <- function(shape,scale){
  (1/(scale^2))*(gamma(1+(2/shape)) - (gamma(1+(1/shape)))^2)
}

for(i in wei_week$week){
  var1 <- var_weibull( wei_week$scale,wei_week$shape)
  var1<- as.data.frame(var1)
  varwei <- cbind(meanwei, var1)
}

wei_meanvar_plot <- ggplot(data = varwei, 
                        aes(x = week,
                            y = mean1)) +
  geom_point(aes(color = as.factor(week), 
                 shape = method),
             size = 3,
             alpha = 0.8) +
  geom_line(aes(color = as.factor(week), group = 1, 
                shape = method), size = 0.45) +
  geom_errorbar(aes(ymin=mean1-var1, ymax=mean1+var1, color = as.factor(week), group = 1, 
                    shape = method), width=.2,
                position=position_dodge(0.9)) +
  scale_color_viridis_d() +
  labs(title = "Weibull mean, total lived by mosquitoes along the season",
       subtitle = "No weather variables included here",
       x = "Time (weeks)",
       y = "Mean (E(X))") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") 

weifaced1 <- wei_meanvar_plot + facet_grid(location ~ method, scales = "fixed")
weifaced1
