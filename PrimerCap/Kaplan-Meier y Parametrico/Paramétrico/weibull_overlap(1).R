library(survival)
library(survminer)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(readxl)
library(lubridate)
library(fitdistrplus)
library(gridExtra)
library(viridis)
setwd("/home/laurablanco/Doctorado/Trabajo_2021/season_2021")
#Dataframe with Laboratory included
datasurv <- read_excel("/home/laurablanco/Doctorado/Trabajo_2021/season_2021/data_analysis_cens.xlsx", 
                       col_types = c("numeric", "date", "numeric", 
                                     "date", "numeric", "numeric", "numeric", 
                                     "numeric"))


datasurv$`hl/bg` <- factor(datasurv$`hl/bg`, 
                           levels = c("1", "2"), 
                           labels = c("HB", "BG"))
datasurv <- datasurv %>%      #change name of the method of capture column
  rename(method = 'hl/bg')


datasurv$location <- factor(datasurv$location, 
                            levels = c("1", "2", "3"), 
                            labels = c("Peri_Urban", "Urban", "Laboratory"))

datasurv <- datasurv %>%
  add_column(day = NA)
head(datasurv)
datasurv$day <- lubridate::yday(ymd(datasurv$start_date))

datasurv <- datasurv %>%
  add_column(month = NA)
datasurv$month <- lubridate::month(ymd(datasurv$start_date))

sq <- seq(min(datasurv$day), max(datasurv$day))
weibull_ubg <- c()

for(i in 11:length(sq)){
  
  urbanbg <- datasurv[datasurv$day %in% sq[(i-10):i]  & datasurv$location=="Urban" & datasurv$method == "BG",]
  if(nrow(urbanbg) > 2){
    urbanbgdist<- fitdist(urbanbg$total_lived, distr = "weibull", method = "mle")
    weibull_ubg <- rbind(weibull_ubg, data.frame(day = sq[i],
                                        shapemean = urbanbgdist$estimate[1],
                                        scalemean = urbanbgdist$estimate[2]))
  }

}

df <- data.frame(month = c(rep(1, 31), rep(2, 28), rep(3, 31),
                           rep(4, 30), rep(5, 31), rep(6, 30),
                           rep(7, 31), rep(8, 31), rep(9, 30),
                           rep(10, 31), rep(11, 30), rep(12, 31)),
                 day = 1:365)

for(i in seq_len(nrow(weibull_ubg))){
  weibull_ubg$month[i] <- df$month[weibull_ubg$day[i] == df$day]
}

plot <- weibull_ubg %>%
  ggplot(aes(shapemean,scalemean)) +
  geom_point(aes(color=factor(month)), size = 2.5) +
  scale_x_continuous(limits = c(0,6), breaks = c(0,1.5,3,4.5,6))+
  scale_y_continuous(limits = c(0,40), breaks = c(0,10,20,30,40))+
  labs(y= "Scale", x = "Shape") +
  theme_bw()+
  scale_color_manual(name= "Overlapped weeks \n coloured by month",
                     values = c("#440154FF", "#39568CFF", "#1F968BFF", "#55C667FF", "#FDE725FF", "#de7065ff"),
                     breaks = c("6","7", "8", "9", "10", "11"),
                     labels = c("June","July", "August", "September", "October", "November"))+
  theme(legend.position = "none")
plotubg<-plot + ggtitle("Urban BG")
plotubg
#URBAN HB
weibullhb <- c()
for(i in 11:length(sq)){
  
  urbanhb <- datasurv[datasurv$day %in% sq[(i-10):i]  & datasurv$location=="Urban" & datasurv$method == "HB",]
  if(nrow(urbanhb) > 2){
    urbanhbdist<- fitdist(urbanhb$total_lived, distr = "weibull", method = "mle")
    weibullhb <- rbind(weibullhb, data.frame(day = sq[i],
                                                 shapemean = urbanhbdist$estimate[1],
                                                 scalemean = urbanhbdist$estimate[2]))
  }
  
}

df <- data.frame(month = c(rep(1, 31), rep(2, 28), rep(3, 31),
                           rep(4, 30), rep(5, 31), rep(6, 30),
                           rep(7, 31), rep(8, 31), rep(9, 30),
                           rep(10, 31), rep(11, 30), rep(12, 31)),
                 day = 1:365)
for(i in seq_len(nrow(weibullhb))){
  weibullhb$month[i] <- df$month[weibullhb$day[i] == df$day]
}

plot <- weibullhb %>%
  ggplot(aes(shapemean,scalemean)) +
  geom_point(aes(color=factor(month)), size = 2.5) +
  scale_x_continuous(limits = c(0,6), breaks = c(0,1.5,3,4.5,6))+
  scale_y_continuous(limits = c(0,40), breaks = c(0,10,20,30,40))+
  labs(y= "Scale", x = "Shape") +
  theme_bw()+
  scale_color_manual(name= "Overlapped weeks \n coloured by month",
                     values = c("#440154FF", "#39568CFF", "#1F968BFF", "#55C667FF", "#FDE725FF","#de7065ff"),
                     breaks = c("6","7", "8", "9", "10", "11"),
                     labels = c("June","July", "August", "September", "October", "November"))+
  theme(legend.position = "none")
plot_uhb<-plot + ggtitle("Urban HB")
plot_uhb
#PERIURBAN BG
weibull_pbg <- c()

for(i in 11:length(sq)){
  
  periurbanbg <- datasurv[datasurv$day %in% sq[(i-10):i]  & datasurv$location=="Peri_Urban" & datasurv$method == "BG",]
  if(nrow(periurbanbg) > 2){
    periurbanbgdist<- fitdist(periurbanbg$total_lived, distr = "weibull", method = "mle")
    weibull_pbg <- rbind(weibull_pbg, data.frame(day = sq[i],
                                                 shapemean = periurbanbgdist$estimate[1],
                                                 scalemean = periurbanbgdist$estimate[2]))
  }
  
}

df <- data.frame(month = c(rep(1, 31), rep(2, 28), rep(3, 31),
                           rep(4, 30), rep(5, 31), rep(6, 30),
                           rep(7, 31), rep(8, 31), rep(9, 30),
                           rep(10, 31), rep(11, 30), rep(12, 31)),
                 day = 1:365)

for(i in seq_len(nrow(weibull_pbg))){
  weibull_pbg$month[i] <- df$month[weibull_pbg$day[i] == df$day]
}

plot <- weibull_pbg %>%
  ggplot(aes(shapemean,scalemean)) +
  geom_point(aes(color=factor(month)), size = 2.5) +
  scale_x_continuous(limits = c(0,6), breaks = c(0,1.5,3,4.5,6))+
  scale_y_continuous(limits = c(0,40), breaks = c(0,10,20,30,40))+
  labs(y= "Scale", x = "Shape") +
  theme_bw()+
  scale_color_manual(name= "Overlapped weeks \n coloured by month",
                     values = c("#440154FF", "#39568CFF", "#1F968BFF", "#55C667FF", "#FDE725FF", "#de7065ff"),
                     breaks = c("6","7", "8", "9", "10", "11"),
                     labels = c("June","July", "August", "September", "October", "November")) +
  theme(legend.position = "none")
plot_pbg<- plot + ggtitle("Peri_Urban BG")
plot_pbg

#PERIURBAN HB

weibullphb <- c()
sq <- seq(min(datasurv$day), max(datasurv$day))
for(i in 11:length(sq)){
  periurbanhb <- datasurv[datasurv$day %in% sq[(i-10):i]  & datasurv$location=="Peri_Urban" & datasurv$method == "HB",]
  if(nrow(periurbanhb) > 2){
    periurbanhbdist <- fitdist(periurbanhb$total_lived, "weibull", method = "mle")
    weibullphb <- rbind(weibullphb, data.frame(day = sq[i],
                                             shapemean = periurbanhbdist$estimate[1],
                                             scalemean = periurbanhbdist$estimate[2]))
  }
}

df <- data.frame(month = c(rep(1, 31), rep(2, 28), rep(3, 31),
                           rep(4, 30), rep(5, 31), rep(6, 30),
                           rep(7, 31), rep(8, 31), rep(9, 30),
                           rep(10, 31), rep(11, 30), rep(12, 31)),
                 day = 1:365)
for(i in seq_len(nrow(weibullphb))){
  weibullphb$month[i] <- df$month[weibullphb$day[i] == df$day]
}

plot <- weibullphb %>%
  ggplot(aes(shapemean,scalemean)) +
  geom_point(aes(color=factor(month)), size = 2.5) +
  labs(y= "Scale", x = "Shape") +
  scale_x_continuous(limits = c(0,6), breaks = c(0,1.5,3,4.5,6))+
  scale_y_continuous(limits = c(0,40), breaks = c(0,10,20,30,40))+
  theme_bw()+
  scale_color_manual(name= "Overlapped weeks \n coloured by month",
                     values = c("#440154FF", "#39568CFF", "#1F968BFF", "#55C667FF", "#FDE725FF"),
                     breaks = c("6","7", "8", "9", "10"),
                     labels = c("June","July", "August", "September", "October"))+
  theme(legend.position = "none")
plot_phb <- plot + ggtitle("PeriUrban HB")
plot_phb

Fig<- ggarrange(plotubg, plot_uhb, plot_pbg, plot_phb, nrow=2, ncol = 2, 
                common.legend = T, legend="bottom", labels = c("a", "b", "c", "d"), 
                            font.label = list(size = 10, color = "black", face = "bold"),hjust = 0.001)
Fig +  scale_color_manual(name= "Overlapped weeks \ncoloured by month", 
                          values = c("#440154FF", "#39568CFF", "#1F968BFF", "#55C667FF", "#FDE725FF", "#de7065ff"),
                          breaks = c("6","7", "8", "9", "10", "11"),
                          labels = c("June","July", "August", "September", "October", "November"))
  