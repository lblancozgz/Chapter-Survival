#upload libraries
library(survival)
library(survminer)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ggpubr)
library(ggsci)
library(readxl)
library(showtext)
library(lubridate)

#upload and prepare data
setwd("C:\\Users\\lblan\\OneDrive\\Escritorio\\CEAB\\2022\\First_chapter")
data_analysis <- read_excel("C:/Users/lblan/OneDrive/Escritorio/CEAB/2022/First_chapter/data_analysis_cens.xlsx", 
                            col_types = c("numeric", "date","numeric", "date", 
                                          "numeric", "numeric", "numeric", "numeric"))

#Now, data of weather (RH and Temperature) in both locations in the field (HOBO's data)
jardin_clima <- read_excel("C:/Users/lblan/OneDrive/Escritorio/CEAB/2022/Jardin_clima_total.xlsx", 
                           col_types = c("date", "numeric", "numeric"))

palafolls_clima <- read_excel("C:/Users/lblan/OneDrive/Escritorio/CEAB/2022/Palafolls_clima_total.xlsx", 
                              col_types = c("date", "numeric", "numeric"))

#URBAN DATAFRAME OF WEATHER
temperaturemeanpal<- palafolls_clima%>% #v#calculating means of temperature and rhper day (HOBO makes 3 measures per day)
  group_by(DATE)%>%
  summarise(meantemperature=mean(TEMPERATURE),meanrh = mean(RH) )

temperatureminpal<- palafolls_clima%>% #v#calculating mins of temperature and rhper day (HOBO makes 3 measures per day)
  group_by(DATE)%>%
  summarise(mintemperature=min(TEMPERATURE),minrh = min(RH) )

temperaturemaxpal<- palafolls_clima%>% #v#calculating max of temperature and rhper day (HOBO makes 3 measures per day)
  group_by(DATE)%>%
  summarise(maxtemperature=max(TEMPERATURE),maxrh = max(RH) )
palafolls_clima<- merge(temperaturemaxpal, temperaturemeanpal, by = "DATE")
palafolls_clima_total<- merge(palafolls_clima, temperatureminpal, by = "DATE")
remove(palafolls_clima)
remove(temperaturemaxpal)
remove(temperatureminpal)
remove(temperaturemeanpal)

#SEMI-URBAN DATAFRAME OF WEATHER
temperaturemeanjar<- jardin_clima%>% #calculating means of temperature and rh per day
  group_by(DATE)%>%
  summarise(meantemperature=mean(TEMPERATURE),meanrh = mean(RH))

temperatureminjar<- jardin_clima%>% #calculating min of temperature and rh per day
  group_by(DATE)%>%
  summarise(mintemperature=min(TEMPERATURE),minrh = min(RH))

temperaturemaxjar<- jardin_clima%>% #calculating max of temperature and rh per day
  group_by(DATE)%>%
  summarise(maxtemperature=max(TEMPERATURE),maxrh = max(RH))
jardin_clima<- merge(temperaturemaxjar, temperaturemeanjar, by = "DATE")
jardin_clima_total<- merge(jardin_clima, temperatureminjar, by = "DATE")
remove(jardin_clima)
remove(temperaturemaxjar)
remove(temperaturemeanjar)
remove(temperatureminjar)

jardin_clima_total$location <- "1" #creating new columns according to the locations
jardin_clima_total$location<- as.numeric(jardin_clima_total$location)
palafolls_clima_total$location<- "2"
palafolls_clima_total$location<- as.numeric(palafolls_clima_total$location)
#renaming column DATE to match with start_date column from our data_analysis dataframe
jardin_clima_total <- jardin_clima_total %>% 
  rename(start_date = DATE)
palafolls_clima_total <- palafolls_clima_total %>% 
  rename(start_date = DATE)

datos_semi <- inner_join(data_analysis, jardin_clima_total, by = c("start_date", "location"), all= TRUE) #Combining temperature of each location with the data
datos_urban <- inner_join(data_analysis, palafolls_clima_total, by = c("start_date", "location"), all= TRUE)
datos_lab<- subset(data_analysis, location=="3")
datos_lab$meantemperature <- NA
datos_lab$meanrh <- NA
datos_lab$mintemperature <- NA
datos_lab$minrh <- NA
datos_lab$maxtemperature <- NA
datos_lab$maxrh <- NA
datos_field_lab <- rbind(datos_semi,datos_urban, datos_lab) #merging all to have a dataframe completed (data survival + weather)
datos_field<- rbind(datos_semi, datos_urban)
clima_field <- rbind(jardin_clima_total, palafolls_clima_total)
#library(writexl)
#write_xlsx(clima_field, "C:\\Users\\lblan\\OneDrive\\Escritorio\\CEAB\\2022\\First_chapter\\clima_field.xlsx")

#Dichotomize location and method, and change data labels
datos_field_lab$`hl/bg` <- factor(datos_field_lab$`hl/bg`, 
                                  levels = c("1", "2"), 
                                  labels = c("HB", "BG"))
datos_field_lab$location <- factor(datos_field_lab$location, 
                                   levels = c("1", "2", "3"), 
                                   labels = c("Peri_Urban", "Urban", "Laboratory"))
datos_field$`hl/bg` <- factor(datos_field$`hl/bg`, 
                              levels = c("1", "2"), 
                              labels = c("HB", "BG"))
datos_field$location <- factor(datos_field$location, 
                               levels = c("1", "2"), 
                               labels = c("Peri_Urban", "Urban"))

hist(datos_field_lab$total_lived) 

datos_field <- datos_field %>%
  rename(method = 'hl/bg')

#Fit survival with Kaplan-Meier method
##LOCATION (LAB AND FIELD)
survfit(Surv(total_lived, censored) ~ location, data = datos_field_lab, type = "kaplan-meier") 

ggsurvplot(survfit(Surv(total_lived, censored) ~ location, data = datos_field_lab, type = "kaplan-meier"),pval = TRUE)

Fig1 <-  survfit(Surv(total_lived, censored) ~ location, data = datos_field_lab, type = "kaplan-meier") %>%
  ggsurvplot(
    surv.median.line = "hv",
    legend.title = "Location",
    legend.labs = c("Peri-Urban", "Urban", "Laboratory"),
    ylab="Survival probability", xlab="Total of days lived",
    legend= c(0.85,0.75),
    surv.plot.heigh = 1.30,
    break.x.by = 10,
    censor = F,
    font.tickslab = c(22),
    font.y = c(25),
    font.x = c(25),
    conf.int = TRUE,
    risk.table = TRUE,
    tables.col = "black",
    risk.table.pos="out",
    risk.table.title="",
    risk.table.fontsize = 8,
    font.family = "Lato",
    tables.height = 0.25,
    tables.theme = theme_bw(),
    palette =c("#3B4992FF", "#BB0021FF", "#00828099"),
    ggtheme = theme_bw((base_size=25)))
Fig1

##METHOD (FIELD)
datos_field_lab <- datos_field_lab %>% 
 rename(method = `hl/bg`)

survfit(Surv(total_lived, censored) ~ method, data = datos_field_lab, type = "kaplan-meier")

ggsurvplot(survfit(Surv(total_lived, censored) ~ method, data = datos_field_lab, type = "kaplan-meier"),pval = TRUE)

Fig2 <-  survfit(Surv(total_lived, censored) ~ method, data = datos_field_lab, type = "kaplan-meier") %>%
  ggsurvplot(
    surv.median.line = "hv",
    ylab="Survival probability", xlab="Total of days lived",
    surv.plot.heigh = 1.30,
    break.x.by = 10,
    font.tickslab = c(16),
    font.y = c(16),
    font.x = c(16),
    conf.int = TRUE,
    risk.table = TRUE,
    tables.col = "black",
    risk.table.pos="out",
    risk.table.title="",
    risk.table.fontsize = 4.75,
    font.family = "Lato",
    tables.height = 0.25,
    tables.theme = theme_bw(),
    palette =c("#3B4992FF", "#BB0021FF"),
    ggtheme = theme_bw((base_size=16)))

Fig2


#This is a plot about the survival between locations by method of capture#
Fig3<- survfit(Surv(total_lived, censored)~ method + location, datos_field_lab, conf.type="log-log")%>%
  ggsurvplot(
    conf.int = T, 
    facet.by = "location",
    legend.title = "Method of capture", 
    short.panel.labs = T,
    legend.labs = c("BG-Traps", "Human Bating"),
    ylab="Survival probability", xlab="Total of days lived ",
    legend= c(0.87,0.8),
    surv.plot.heigh = 1.30,
    break.x.by = 10,
    font.tickslab = c(11),
    font.y = c(14),
    font.x = c(14),
    font.family = "Lato",
    palette =c("#3B4992FF", "#BB0021FF"),
    ggtheme = theme_bw(base_size=11),
  )

#This is a plot about the survival between same methods of capture in the different locations#
Fig4<- survfit(Surv(total_lived, censored)~ location + method, datos_field_lab, conf.type="log-log")%>%
  ggsurvplot(
    conf.int = T, 
    facet.by = "method",
    legend.title = "Location", 
    short.panel.labs = T,
    legend.labs = c("Peri-Urban", "Urban"),
    ylab="Survival probability", xlab="Total of days lived",
    legend= c(0.87,0.8),
    surv.plot.heigh = 1.30,
    break.x.by = 10,
    font.tickslab = c(11),
    font.y = c(14),
    font.x = c(14),
    font.family = "Lato",
    palette =c("#3B4992FF", "#BB0021FF"),
    ggtheme = theme_bw(base_size=11),
  )

FIG5<- ggarrange(Fig3 + rremove("ylab") + rremove("xlab"), Fig4 + rremove("ylab") + rremove("xlab"), nrow = 2, labels =c("A", "B")) 

library(grid)

FIGURA5COMPLETE<- annotate_figure(FIG5, left = textGrob("Survival Probability", rot = 90, vjust = 0.1, gp = gpar(cex = 1.3)),
                                  bottom = textGrob("Total of days lived", gp = gpar(cex = 1.3)))
FIGURA5COMPLETE

survdiff(Surv(total_lived, censored)~location + strata(method), data = datos_field)
ggsurvplot(survfit(Surv(total_lived, censored)~location + strata(method), data = datos_field, type = "kaplan-meier"),pval = TRUE)

Figp<- survfit(Surv(total_lived, censored)~location + strata(method), data = datos_field, conf.type="log-log")%>%
  ggsurvplot(
    conf.int = F, 
    censor = F,
    legend.title= "",
    legend.labs = c("Peri-Urban with HB", "Peri-Urban with BG", "Urban with HB", "Urban with BG"),
    ylab="Survival probability", xlab="Total of days lived",
    legend= c(0.82,0.8),
    surv.plot.heigh = 1.30,
    break.x.by = 10,
    linetype = c("solid", "dotted", "solid", "dotted"),
    size = 1.25,
    font.tickslab = c(18),
    font.y = c(22),
    font.x = c(22),
    font.family = "Lato",
    palette =c("#3B4992FF", "#3B4992FF", "#BB0021FF",  "#BB0021FF"),
    ggtheme = theme_bw(base_size=22),
  )
Figp<- ggpar(Figp, font.legend = 22)
Figp

#ESTIMATION OF THE MEAN, MEDIAN AND PERCENTILES
survloc <- survfit(Surv(total_lived, censored) ~ location, datos_field_lab) #location 
survmet <- survfit(Surv(total_lived, censored) ~ method, datos_field_lab) #capture method
survlocfi <- survfit(Surv(total_lived, censored) ~ location, datos_field) #location field

print(survloc, print.rmean = TRUE)
print(survmet, print.rmean = TRUE)
print(survlocfi, print.rmean = TRUE)

quantile(survloc, c(0.05, 0.5, 0.95))
quantile(survmet, c(0.05, 0.5, 0.95))
quantile(survlocfi, c(0.05, 0.5, 0.95))

#LOG-RANK TEST
survdiff(Surv(total_lived, censored) ~ location, data = datos_field_lab, rho = 0)  #Prueba log-rank LOCATION
survdiff(Surv(total_lived, censored) ~ location, data = datos_field_lab, rho = 1)  #Prueba log-rank LOCATION

survdiff(Surv(total_lived, censored) ~ method, data = datos_field, rho = 0) #Prueba log-rank METHOD
survdiff(Surv(total_lived, censored) ~ method, data = datos_field, rho = 1)  #Prueba log-rank METHOD

survdiff(Surv(total_lived, censored) ~ location, data = datos_field, rho = 0) #Prueba log-rank LOCATION FIELD
survdiff(Surv(total_lived, censored) ~ location, data = datos_field, rho = 1)  #Prueba log-rank LOCATION FIELD

#ESTIMATION OF THE CUMULATIVE RISK FUNCTION
R <- survloc %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(R, 60)
ggsurvplot(survloc, fun = "cumhaz", xlab = "time (days)", censor = T, 
           ylab = "cumulative risk", title = "cumulative risk", legend.title = "Location")

S <- survmet %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(S, 60)
ggsurvplot(survmet, fun = "cumhaz", xlab = "time (days)", censor = T, 
           ylab = "cumulative risk", title = "cumulative risk", legend.title = "Method")

SF <- survlocfi %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(S, 60)
ggsurvplot(survlocfi, fun = "cumhaz", xlab = "time (days)", censor = T, 
           ylab = "cumulative risk", title = "cumulative risk", legend.title = "Method")



