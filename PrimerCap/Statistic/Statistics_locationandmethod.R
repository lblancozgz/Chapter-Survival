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
library(ggsci)
setwd("C:\\Users\\lblan\\OneDrive\\Escritorio\\CEAB\\2022\\First_chapter")

datastat <- read_excel("C:/Users/lblan/OneDrive/Escritorio/CEAB/2022/First_chapter/data_analysis_cens.xlsx", 
                       col_types = c("numeric", "date","numeric", "date", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))
datset_field <- datastat[!(datastat$location=="3"),]
datset_field_bg <- datastat[!(datastat$location=="3" | datastat$`hl/bg` == "1"),]
datset_field_hb <- datastat[!(datastat$location=="3" | datastat$`hl/bg` == "2"),]

#Creamos dataframe del jardín botánico para cada tipo de método de captura
datset_jar <- datastat[!(datastat$location=="2" | datastat$location=="3"),]
datset_jar_hl<-datastat[!(datastat$location=="2" | datastat$location=="3"  | datastat$`hl/bg` == "2"),]
datset_jar_bg<-datastat[!(datastat$location=="2" | datastat$location=="3" | datastat$`hl/bg` == "1"),]
ks.test(datset_jar_bg$total_lived, "pnorm", mean(datset_jar_bg$total_lived, na.rm=T), sd(datset_jar_bg$total_lived,na.rm=T)) #pvalor <0.05 con trampas BG-sentinel
ks.test(datset_jar_hl$total_lived, "pnorm", mean(datset_jar_hl$total_lived, na.rm=T), sd(datset_jar_hl$total_lived,na.rm=T)) #pvalor >0.05 con HL.
test_jar_bghl <- wilcox.test(datset_jar$total_lived~datset_jar$`hl/bg`, exact = FALSE, paired = FALSE)
test_jar_bghl #p-value <0.05. There are significative differences between 2 methods of capture in the Botanical Garden

#Creamos dataframe de Palafolls para cada tipo de método de captura
datset_pal<-datastat[!(datastat$location=="1" | datastat$location=="3"),]
datset_pal_hl<-datastat[!(datastat$location=="1" | datastat$location=="3"| datastat$`hl/bg` == "2"),]
datset_pal_bg<-datastat[!(datastat$location=="1" | datastat$location=="3"| datastat$`hl/bg` == "1"),]
ks.test(datset_pal_bg$total_lived, "pnorm", mean(datset_pal_bg$total_lived, na.rm=T), sd(datset_pal_bg$total_lived,na.rm=T)) #pvalor <0.05 con trampas BG-sentinel
ks.test(datset_pal_hl$total_lived, "pnorm", mean(datset_pal_hl$total_lived, na.rm=T), sd(datset_pal_hl$total_lived,na.rm=T)) #pvalor >0.05 con HL.
test_pal_bghl <- wilcox.test(datset_pal$total_lived~datset_pal$`hl/bg`, exact = FALSE, paired= FALSE)
test_pal_bghl #p-value <0.5. There are significative differences between 2 methods of capture in Palafolls

#WE SEE IF THERE ARE SIGNIFICANT DIFFERENCES BETWEEN THE SURVIVAL BY FIELD LOCATION (URBAN VS SEMI-URBAN)
ks.test(datset_jar$total_lived, "pnorm", mean(datset_jar$total_lived, na.rm=T), sd(datset_jar$total_lived,na.rm=T)) #pvalor <0.05 en Jardin
ks.test(datset_pal$total_lived, "pnorm", mean(datset_pal$total_lived, na.rm=T), sd(datset_pal$total_lived,na.rm=T)) #pvalor <0.05 en Palafolls
test_location <- wilcox.test(datset_field$total_lived~datset_field$location, exact = FALSE, paired = FALSE)
test_location#p-value <0.5. There are significative differences between survival in 2 field locations.

#Prueba t de Student para dos muestras independientes (con distribucion normal)
t.test(datset_pal_hl$total_lived,datset_jar_hl$total_lived) #p-value <0.05, SIGNIFICANT DIFFERENCES BETWEEN SURVIVAL BY HL METHOD IN THE FIELD LOCATIONS
wilcox.test(datset_field_bg$total_lived~datset_field_bg$location,exact = FALSE, paired = FALSE) #pvalue<0.5. There are significative differences betwwen survival by bg method in the field locations

#loop, in each location, diferences in survival depending on capture method
#due to difference in size of females BG and females HL
x<- 0
for (i in 1:999) {
  dataurbanloop <-datset_pal %>%
    group_by(`hl/bg`)%>%
    sample_n(79)
  result <- wilcox.test(total_lived~`hl/bg`, data=datset_pal)
  if (result$p.value <0.05){
    x= x+1
  }
}



x<- 0
for (i in 1:999) {
  dataperiloop<- datset_jar %>%
    group_by(`hl/bg`)%>%
    sample_n(78)
  result <- wilcox.test(total_lived~`hl/bg`, data=datset_jar)
  if (result$p.value <0.05){
    x= x+1
  }
}

#datset_jar$`hl/bg` <- factor(datset_jar$`hl/bg`, 
 #                          levels = c("1", "2"), 
  #                         labels = c("HB", "BG"))

#datset_pal$`hl/bg` <- factor(datset_pal$`hl/bg`, 
 #                            levels = c("1", "2"), 
  #                           labels = c("HB", "BG"))

#datset_field$location <- factor(datset_field$location, 
 #                           levels = c("1", "2", "3"), 
  #                          labels = c("Peri_Urban", "Urban", "Laboratory"))

#datset_field$`hl/bg` <- factor(datset_field$`hl/bg`, 
 #                            levels = c("1", "2"), 
  #                           labels = c("HB", "BG"))

#datset_field_bg$location <- factor(datset_field_bg$location, 
 #                               levels = c("1", "2", "3"), 
  #                              labels = c("Peri_Urban", "Urban", "Laboratory"))
#datset_field_hb$location <- factor(datset_field_hb$location, 
 #                               levels = c("1", "2", "3"), 
  #                              labels = c("Peri_Urban", "Urban", "Laboratory"))
#datset_field <- datset_field %>% 
 # rename(method = `hl/bg`)

#boxplots
location<- ggboxplot(datset_field, x = "location", y = "total_lived",
          color = "location", palette = "jco")
location +  stat_compare_means(method = "wilcox.test") + ggtitle("Wilcoxon test - Location")

#method periurban
p <- ggboxplot(datset_jar, x = "hl/bg", y = "total_lived",
               color = "hl/bg", palette = "jco")
p + stat_compare_means(method = "wilcox.test") + ggtitle("Wilcoxon test - method in Peri_urban") 

#method urban
u <- ggboxplot(datset_pal, x = "hl/bg", y = "total_lived",
               color = "hl/bg", palette = "jco")
u + stat_compare_means(method = "wilcox.test") + ggtitle("Wilcoxon test - method in Urban") 

#method bg in all
bg <- ggboxplot(datset_field_bg, x = "location", y = "total_lived",
               color = "location", palette = "jco")
bg + stat_compare_means(method = "wilcox.test") + ggtitle("Wilcoxon test - BG in both locations")

#method hb in all
hb <- ggboxplot(datset_field_hb, x = "location", y = "total_lived",
                color = "location", palette = "jco")
hb + stat_compare_means(method = "t.test") + ggtitle("T-Student; HB in both locations")
