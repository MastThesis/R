#########################################################################
#
# Author: Laura Campigotto
# Date: 18.07.2022
# Project: Quantification of nine amine containing metabolites 
#
#########################################################################
# I) used functions

source("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/LC_col_2.R")
source("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ANALYTIC.R")
source("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/barplot_function.R")

#########################################################################
#
#
# II) ESI Parameters
#########################################################################
# set directory of ESI data

setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")
#  Outline (For a) capillary temperature, b) capillary voltage 
#               c) gas flow and sheat gas d) heater e) spray voltage
#               f) tube length  
#1) TIC  
#2) EIC of all substances
#3) EIC of TMA
#4) Barblot intensity TMA 

#########################################################################


# a) Capillary Temperature
#
#1) TIC 
#-------------------------
#    i) seperate

names <- c(expression("capillary T: "*150*degree*C),
          expression("capillary T: "*200*degree*C),
          expression("capillary T: "*250*degree*C),
          expression("capillary T: "*300*degree*C),
          expression("capillary T: "*350*degree*C),
          expression("capillary T: "*380*degree*C),
          expression("capillary T: "*400*degree*C))
max_time <- rep(8,7)
ylim_max <- c(7e6,5e6,4.5e6,4e6,3.5e6,3.5e6,3.5e6)


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_SEPERATE.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mar=c(3.5,5,2,2))
par(mfrow=c(3,3))
TIC_separate("capT_TIC.xlsx",names,max_time,ylim_max)
dev.off()


#    i) together


names <- "Capillary Temperature"
max_time <- 8
legend <- c(expression("capillary T: "*150*degree*C),
            expression("capillary T: "*200*degree*C),
            expression("capillary T: "*250*degree*C),
            expression("capillary T: "*300*degree*C),
            expression("capillary T: "*350*degree*C),
            expression("capillary T: "*380*degree*C),
            expression("capillary T: "*400*degree*C))


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_TOGETHER.pdf",width = 7.26, height = 4.26, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 
par(mfrow=c(1,1))
par(mar=c(3.5,5,2,2))
TIC_together("capT_TIC.xlsx",names,max_time,legend)
dev.off()

#
#2) EIC of all substances
#-------------------------

# i) All in one 3x3 matrix
file <- c("capT_150.xlsx",
          "capT_200.xlsx",
          "capT_250.xlsx",
          "capT_300.xlsx",
          "capT_350.xlsx",
          "capT_380.xlsx",
          "capT_400.xlsx") 
time <- 8
name <- c(expression("capillary T: "*150*degree*C),
          expression("capillary T: "*200*degree*C),
          expression("capillary T: "*250*degree*C),
          expression("capillary T: "*300*degree*C),
          expression("capillary T: "*350*degree*C),
          expression("capillary T: "*380*degree*C),
          expression("capillary T: "*400*degree*C))
type <- ""
ylim_max <- c(6e6,5e6,5e6,4e6,3.5e6,3.5e6,3e6)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_together.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mfrow=c(3,3))
par(mar=c(3.5,5.5,2,2))
for(i in 1:7){
EIC(file[i], time, name=name[i], type, ylim_max[i])
}
dev.off()

# for each temperature

Temp <- c(150,200,250,300,350,380,400)
par(mfrow=c(1,1))
par(mar=c(3.5,7,2,2))

for(i in 1:7){
  pdf(paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_",Temp[i],".pdf"),width = 7.26, height = 4.26, bg = "white",          
      colormodel = "cmyk")  #, #paper = "a4r") 
  par(mar=c(3.5,6,2,2))
  EIC(file[i], time, name=name[i], type, ylim_max[i])
  dev.off()
  }

#
#3) EIC of TMA
#-------------------------

# i) EIC TMA all together

fileT <- c("capT_150.xlsx",
          "capT_200.xlsx",
          "capT_250.xlsx",
          "capT_300.xlsx",
          "capT_350.xlsx",
          "capT_380.xlsx",
          "capT_400.xlsx")
legend <- c(expression("capillary T: "*150*degree*C),
          expression("capillary T: "*200*degree*C),
          expression("capillary T: "*250*degree*C),
          expression("capillary T: "*300*degree*C),
          expression("capillary T: "*350*degree*C),
          expression("capillary T: "*380*degree*C),
          expression("capillary T: "*400*degree*C))
sheet <- "TMA"
xmin <- 3
xmax <- 5
name <- "Capillary Temperature: TMA"
ylim_max=3.4e5
xlab=T
ylab=T
linesT=c(F,T,T,T,T,T,T)

lineColT=c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_together.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  
    for(i in 1:7){
      par(mar=c(3.5,6,2,2))
      file = fileT[i]
      lines = linesT[i]
      lineCol = lineColT[i]
      EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
    }
    
    legend("topright",bty="n",legend=legend,pch=15,
           col = lineColT,pt.cex = 1.2)
dev.off()


# ii) Seperate EIC TMA

Temp <- c(150,200,250,300,350,380,400)
par(mfrow=c(1,1))
ylim_maxT =c(3.5e5,2e5,1.42e5, 1.2e5,1.2e5,1.2e5,1.2e5)
for(i in 1:7){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  pdf(  paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA",Temp[i],".pdf"),
        width = 7.26, height = 4.26, bg = "white")       
  par(mar=c(3.5,6,2,2))
  par(mfrow=c(1,1))
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright",legend="TMA",bty="n")
  dev.off()
}

#
#4) Barblot intensity TMA 
#-------------------------

main = "Capillary T (Intensities TMA)" 
type <- "intensity"
data <- c("capT_150.xlsx",
           "capT_200.xlsx",
           "capT_250.xlsx",
           "capT_300.xlsx",
           "capT_350.xlsx",
           "capT_380.xlsx",
           "capT_400.xlsx")
directory <- setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")


name <- c(expression("T: "*150*degree*C),
            expression("T: "*200*degree*C),
            expression("T: "*250*degree*C),
            expression("T: "*300*degree*C),
            expression("T: "*350*degree*C),
            expression("T: "*380*degree*C),
            expression("T: "*400*degree*C))
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/4) Barplot TMA/barplot_CT.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  

EIC_barplot(main,data,type,directory,name)
dev.off()



#################################################################################
#################################################################################
#################################################################################


#########################################################################


# b) Capillary Voltage
#
#1) TIC 
#-------------------------
#    i) seperate

names <- c(expression("capillary Volt.: "*1*V),
           expression("capillary Volt.: "*5*V),
           expression("capillary Volt.: "*10*V),
           expression("capillary Volt.: "*20*V),
           expression("capillary Volt.: "*30*V),
           expression("capillary Volt.: "*40*V),
           expression("capillary Volt.: "*50*V))
max_time <- rep(8,7)
ylim_max <- c(7e6,5e6,4.5e6,4e6,3.5e6,3.5e6,3.5e6)


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_SEPERATE.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mar=c(3.5,5,2,2))
par(mfrow=c(3,3))
TIC_separate("capV_TIC.xlsx",names,max_time,ylim_max)
dev.off()


#    i) together


names <- "Capillary Voltage"
max_time <- 8
legend <- c(expression("capillary Volt.: "*1*V),
                     expression("capillary Volt.: "*5*V),
                     expression("capillary Volt.: "*10*V),
                     expression("capillary Volt.: "*20*V),
                     expression("capillary Volt.: "*30*V),
                     expression("capillary Volt.: "*40*V),
                     expression("capillary Volt.: "*50*V))


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_TOGETHER.pdf",width = 7.26, height = 4.26, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 
par(mfrow=c(1,1))
par(mar=c(3.5,5,2,2))
TIC_together("capV_TIC.xlsx",names,max_time,legend)
dev.off()

#
#2) EIC of all substances
#-------------------------

# i) All in one 3x3 matrix
file <- c("capV_1.xlsx",
          "capV_5.xlsx",
          "capV_10.xlsx",
          "capV_20.xlsx",
          "capV_30.xlsx",
          "capV_40.xlsx",
          "capV_50.xlsx") 
time <- 8
name <- c(expression("capillary Volt.: "*1*V),
          expression("capillary Volt.: "*5*V),
          expression("capillary Volt.: "*10*V),
          expression("capillary Volt.: "*20*V),
          expression("capillary Volt.: "*30*V),
          expression("capillary Volt.: "*40*V),
          expression("capillary Volt.: "*50*V))
type <- ""
ylim_max <- c(2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_together.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mfrow=c(3,3))
par(mar=c(3.5,5.5,2,2))
for(i in 1:7){
  EIC(file[i], time, name=name[i], type, ylim_max[i])
}
dev.off()

# for each temperature

Temp <- c(1,5,10,20,30,40,50)
par(mfrow=c(1,1))
par(mar=c(3.5,7,2,2))

for(i in 1:7){
  pdf(paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_",Temp[i],".pdf"),width = 7.26, height = 4.26, bg = "white",          
      colormodel = "cmyk")  #, #paper = "a4r") 
  par(mar=c(3.5,6,2,2))
  EIC(file[i], time, name=name[i], type, ylim_max=2e6)
  dev.off()
}

#
#3) EIC of TMA
#-------------------------

# i) EIC TMA all together

fileT <- c("capV_1.xlsx",
          "capV_5.xlsx",
          "capV_10.xlsx",
          "capV_20.xlsx",
          "capV_30.xlsx",
          "capV_40.xlsx",
          "capV_50.xlsx") 
legend <-   c(expression("capillary Volt.: "*1*V),
              expression("capillary Volt.: "*5*V),
              expression("capillary Volt.: "*10*V),
              expression("capillary Volt.: "*20*V),
              expression("capillary Volt.: "*30*V),
              expression("capillary Volt.: "*40*V),
              expression("capillary Volt.: "*50*V))
sheet <- "TMA"
xmin <- 3
xmax <- 5
name <- "Capillary Voltage: TMA"
ylim_max=1e5
xlab=T
ylab=T
linesT=c(F,T,T,T,T,T,T)

lineColT=c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_together.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  
for(i in 1:7){
  par(mar=c(3.5,6,2,2))
  file = fileT[i]
  lines = linesT[i]
  lineCol = lineColT[i]
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
}

legend("topright",bty="n",legend=legend,pch=15,
       col = lineColT,pt.cex = 1.2)
dev.off()


# ii) Seperate EIC TMA
Temp <- c(1,5,10,20,30,40,50)
par(mfrow=c(1,1))
ylim_maxT =c(1e5,1e5,1e5, 1e5,1e5,1e5,1e5)

for(i in 1:7){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  pdf(  paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA",Temp[i],".pdf"),
        width = 7.26, height = 4.26, bg = "white")       
  par(mar=c(3.5,6,2,2))
  par(mfrow=c(1,1))
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright",legend="TMA",bty="n")
  dev.off()
}

#
#4) Barblot intensity TMA 
#-------------------------

main = "Capillary V (Intensities TMA)" 
type <- "intensity"
data <- c("capV_1.xlsx",
           "capV_5.xlsx",
           "capV_10.xlsx",
           "capV_20.xlsx",
           "capV_30.xlsx",
           "capV_40.xlsx",
           "capV_50.xlsx") 
name <-   c(expression("cap. Volt.: "*1*V),
              expression("cap. Volt.: "*5*V),
              expression("cap. Volt.: "*10*V),
              expression("cap. Volt.: "*20*V),
              expression("cap. Volt.: "*30*V),
              expression("cap. Volt.: "*40*V),
              expression("cap. Volt.: "*50*V))
  
directory <- setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")



pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/4) Barplot TMA/barplot_CT.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  

EIC_barplot(main,data,type,directory,name)
dev.off()





#################################################################################
#################################################################################
#################################################################################


#########################################################################


# c) Heater Temperature
#
#1) TIC 
#-------------------------
#    i) seperate


names <- c( expression("T: "*0*degree*C),
            expression("T: "*100*degree*C),
            expression("T: "*200*degree*C),
            expression("T: "*300*degree*C),
            expression("T: "*400*degree*C))
max_time <- rep(8,5)
ylim_max <- c(7e6,5e6,4.5e6,4e6,6e6)


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_SEPERATE.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mar=c(3.5,5,2,2))
par(mfrow=c(3,3))
TIC_separate("heater_TIC.xlsx",names,max_time,ylim_max)
dev.off()


#    i) together


names <- "Capillary Voltage"
max_time <- 8
legend <- c( expression("T: "*0*degree*C),
             expression("T: "*100*degree*C),
             expression("T: "*200*degree*C),
             expression("T: "*300*degree*C),
             expression("T: "*400*degree*C))


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_TOGETHER.pdf",width = 7.26, height = 4.26, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 
par(mfrow=c(1,1))
par(mar=c(3.5,5,2,2))
TIC_together("heater_TIC.xlsx",names,max_time,legend)
dev.off()

#
#2) EIC of all substances
#-------------------------

# i) All in one 3x3 matrix
file <- c("heater_0.xlsx",
          "heater_100.xlsx",
          "heater_200.xlsx",
          "heater_300.xlsx",
          "heater_400.xlsx")
time <- 8
name <- c( expression("T: "*0*degree*C),
           expression("T: "*100*degree*C),
           expression("T: "*200*degree*C),
           expression("T: "*300*degree*C),
           expression("T: "*400*degree*C))
type <- ""
ylim_max <- c(2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_together.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mfrow=c(2,3))
par(mar=c(3.5,5.5,2,2))
for(i in 1:5){
  EIC(file[i], time, name=name[i], type, ylim_max[i])
}
dev.off()

# for each temperature

Temp <- c(0,100,200,300,400)
par(mfrow=c(1,1))
par(mar=c(3.5,7,2,2))

for(i in 1:5){
  pdf(paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_",Temp[i],".pdf"),width = 7.26, height = 4.26, bg = "white",          
      colormodel = "cmyk")  #, #paper = "a4r") 
  par(mar=c(3.5,6,2,2))
  EIC(file[i], time, name=name[i], type, ylim_max=2e6)
  dev.off()
}

#
#3) EIC of TMA
#-------------------------

# i) EIC TMA all together

fileT <- c("heater_0.xlsx",
           "heater_100.xlsx",
           "heater_200.xlsx",
           "heater_300.xlsx",
           "heater_400.xlsx")
legend <-  c( expression("T: "*0*degree*C),
              expression("T: "*100*degree*C),
              expression("T: "*200*degree*C),
              expression("T: "*300*degree*C),
              expression("T: "*400*degree*C))
sheet <- "TMA"
xmin <- 3
xmax <- 5
name <- "Heater Temperature: TMA"
ylim_max=1e5
xlab=T
ylab=T
linesT=c(F,T,T,T,T,T,T)

lineColT=c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_together.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  
for(i in 1:5){
  par(mar=c(3.5,6,2,2))
  file = fileT[i]
  lines = linesT[i]
  lineCol = lineColT[i]
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
}

legend("topright",bty="n",legend=legend,pch=15,
       col = lineColT,pt.cex = 1.2)
dev.off()


# ii) Seperate EIC TMA
Temp <- c(0,100,200,300,400)
par(mfrow=c(1,1))
ylim_maxT =c(1e5,1e5,1e5, 1e5,1e5,1e5,1e5)

for(i in 1:5){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  pdf(  paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA",Temp[i],".pdf"),
        width = 7.26, height = 4.26, bg = "white")       
  par(mar=c(3.5,6,2,2))
  par(mfrow=c(1,1))
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright",legend="TMA",bty="n")
  dev.off()
}

#
#4) Barblot intensity TMA 
#-------------------------

main = "Heater Temperature (Intensities TMA)" 
type <- "intensity"
data <- c("heater_0.xlsx",
          "heater_100.xlsx",
          "heater_200.xlsx",
          "heater_300.xlsx",
          "heater_400.xlsx")
name <-   c( expression("T: "*0*degree*C),
             expression("T: "*100*degree*C),
             expression("T: "*200*degree*C),
             expression("T: "*300*degree*C),
             expression("T: "*400*degree*C))

directory <- setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")



pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/4) Barplot TMA/barplot_CT.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  

EIC_barplot(main,data,type,directory,name)
dev.off()



#################################################################################
#################################################################################
#################################################################################


#########################################################################


# d) Spray Voltage 
#
#1) TIC 
#-------------------------
#    i) seperate

names <- c(expression("spray Volt.: "*2*V),
           expression("spray Volt.: "*3*V),
           expression("spray Volt.: "*4*V),
           expression("spray Volt.: "*5*V))
max_time <- rep(8,4)
ylim_max <- c(3e6,3e6,3e6,3e6)


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_SEPERATE.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mar=c(3.5,5,2,2))
par(mfrow=c(2,2))
TIC_separate("sprayV_TIC.xlsx",names,max_time,ylim_max)
dev.off()


#    i) together


names <- "Spray Voltage"
max_time <- 8
legend <- c(expression("spray Volt.: "*2*V),
            expression("spray Volt.: "*3*V),
            expression("spray Volt.: "*4*V),
            expression("spray Volt.: "*5*V))


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_TOGETHER.pdf",width = 7.26, height = 4.26, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 
par(mfrow=c(1,1))
par(mar=c(3.5,5,2,2))
TIC_together("sprayV_TIC.xlsx",names,max_time,legend)
dev.off()

#
#2) EIC of all substances
#-------------------------

# i) All in one 3x3 matrix
file <- c("sprayV_2.xlsx",
          "sprayV_3.xlsx",
          "sprayV_4.xlsx",
          "sprayV_5.xlsx")
time <- 8
name <- c(expression("spray Volt.: "*2*V),
          expression("spray Volt.: "*3*V),
          expression("spray Volt.: "*4*V),
          expression("spray Volt.: "*5*V))
type <- ""
ylim_max <- c(2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_together.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mfrow=c(2,2))
par(mar=c(3.5,5.5,2,2))
for(i in 1:4){
  EIC(file[i], time, name=name[i], type, ylim_max[i])
}
dev.off()

# for each temperature

Temp <- c(2,3,4,5)
par(mfrow=c(1,1))
par(mar=c(3.5,7,2,2))
time=6
ylim_max<-c(2e6,1.8e6,1.6e6,1.6e6)
for(i in 1:4){
  pdf(paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_",Temp[i],".pdf"),width = 7.26, height = 4.26, bg = "white",          
      colormodel = "cmyk")  #, #paper = "a4r") 
  par(mar=c(3.5,6,2,2))
  EIC(file[i], time, name=name[i], type, ylim_max[i])
  dev.off()
}

#
#3) EIC of TMA
#-------------------------

# i) EIC TMA all together

fileT <-  c("sprayV_2.xlsx",
            "sprayV_3.xlsx",
            "sprayV_4.xlsx",
            "sprayV_5.xlsx")
legend <-   c(expression("spray Volt.: "*2*V),
              expression("spray Volt.: "*3*V),
              expression("spray Volt.: "*4*V),
              expression("spray Volt.: "*5*V))
sheet <- "TMA"
xmin <- 3
xmax <- 5
name <- "Spray Voltage: TMA"
ylim_max=0.9e5
xlab=T
ylab=T
linesT=c(F,T,T,T,T,T,T)

lineColT=c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_together.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  
for(i in 1:4){
  par(mar=c(3.5,6,2,2))
  file = fileT[i]
  lines = linesT[i]
  lineCol = lineColT[i]
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
}

legend("topright",bty="n",legend=legend,pch=15,
       col = lineColT,pt.cex = 1.2)
dev.off()


# ii) Seperate EIC TMA
Temp <- c(2,3,4,5)
par(mfrow=c(1,1))
ylim_maxT =rep(0.9e5,4)

for(i in 1:4){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  pdf(  paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA",Temp[i],".pdf"),
        width = 7.26, height = 4.26, bg = "white")       
  par(mar=c(3.5,6,2,2))
  par(mfrow=c(1,1))
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright",legend="TMA",bty="n")
  dev.off()
}

#
#4) Barblot intensity TMA 
#-------------------------

main = "Spray V (Intensities TMA)" 
type <- "intensity"
data <- c("sprayV_2.xlsx",
          "sprayV_3.xlsx",
          "sprayV_4.xlsx",
          "sprayV_5.xlsx")
name <-   c(expression("spray Volt.: "*2*V),
            expression("spray Volt.: "*3*V),
            expression("spray Volt.: "*4*V),
            expression("spray Volt.: "*5*V))

directory <- setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")



pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/4) Barplot TMA/barplot_CT.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  

EIC_barplot(main,data,type,directory,name)
dev.off()





#################################################################################
#################################################################################
#################################################################################


#########################################################################


# e) Tube lens
#
#1) TIC 
#-------------------------
#    i) seperate

names <- c(expression("tube lens Volt.: "*20*V),
           expression("tube lens Volt.: "*40*V),
           expression("tube lens Volt.: "*60*V),
           expression("tube lens Volt.: "*80*V),
           expression("tube lens Volt.: "*100*V))
           
max_time <- rep(8,5)
ylim_max <- c(2e6,2.1e6,2.5e6,2.5e6,2e6)


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_SEPERATE.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mar=c(3.5,5,2,2))
par(mfrow=c(3,2))
TIC_separate("tube_TIC.xlsx",names,max_time,ylim_max)
dev.off()


#    i) together


names <- "Tube Lens Volt."
max_time <- 8
legend <- c(expression("tube lens Volt.: "*20*V),
            expression("tube lens Volt.: "*40*V),
            expression("tube lens Volt.: "*60*V),
            expression("tube lens Volt.: "*80*V),
            expression("tube lens Volt.: "*100*V))


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_TOGETHER.pdf",width = 7.26, height = 4.26, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 
par(mfrow=c(1,1))
par(mar=c(3.5,5,2,2))
TIC_together("tube_TIC.xlsx",names,max_time,legend)
dev.off()

#
#2) EIC of all substances
#-------------------------

# i) All in one 3x3 matrix
file <- c("tube_20.xlsx",
          "tube_40.xlsx",
          "tube_60.xlsx",
          "tube_80.xlsx",
          "tube_100.xlsx")
time <- 8
name <- c(expression("tube lens Volt.: "*20*V),
          expression("tube lens Volt.: "*40*V),
          expression("tube lens Volt.: "*60*V),
          expression("tube lens Volt.: "*80*V),
          expression("tube lens Volt.: "*100*V))
type <- ""
ylim_max <- c(2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6,2.2e6)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_together.pdf",width = 7.26, height = 5.53, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mfrow=c(3,2))
par(mar=c(3.5,5.5,2,2))
for(i in 1:5){
  EIC(file[i], time, name=name[i], type, ylim_max[i])
}
dev.off()

# for each temperature

Temp <- c(20,40,60,80,100)
par(mfrow=c(1,1))
par(mar=c(3.5,7,2,2))
ylim_max <- c(1.5e6,2e6,2.3e6,2e6,1e6)
for(i in 1:5){
  pdf(paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_",Temp[i],".pdf"),width = 7.26, height = 4.26, bg = "white",          
      colormodel = "cmyk")  #, #paper = "a4r") 
  par(mar=c(3.5,6,2,2))
  EIC(file[i], time, name=name[i], type, ylim_max[i])
  dev.off()
}

#
#3) EIC of TMA
#-------------------------

# i) EIC TMA all together

fileT <- c("tube_20.xlsx",
           "tube_40.xlsx",
           "tube_60.xlsx",
           "tube_80.xlsx",
           "tube_100.xlsx")
legend <- c(expression("tube lens Volt.: "*20*V),
            expression("tube lens Volt.: "*40*V),
            expression("tube lens Volt.: "*60*V),
            expression("tube lens Volt.: "*80*V),
            expression("tube lens Volt.: "*100*V))
sheet <- "TMA"
xmin <- 3
xmax <- 5
name <- "Tube Lens Voltage: TMA"
ylim_max=1.5e5
xlab=T
ylab=T
linesT=c(F,T,T,T,T,T,T)

lineColT=c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_together.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  
for(i in 1:5){
  par(mar=c(3.5,6,2,2))
  file = fileT[i]
  lines = linesT[i]
  lineCol = lineColT[i]
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
}

legend("topright",bty="n",legend=legend,pch=15,
       col = lineColT,pt.cex = 1.2)
dev.off()


# ii) Seperate EIC TMA
Temp <- c(20,40,60,80,100)
par(mfrow=c(1,1))
ylim_maxT =c(1e5,1e5,1e5, 1e5,1.5e5)

for(i in 1:5){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  pdf(  paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA",Temp[i],".pdf"),
        width = 7.26, height = 4.26, bg = "white")       
  par(mar=c(3.5,6,2,2))
  par(mfrow=c(1,1))
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright",legend="TMA",bty="n")
  dev.off()
}

#
#4) Barblot intensity TMA 
#-------------------------

main = "Tube Lens V (Intensities TMA)" 
type <- "intensity"
data <- c("tube_20.xlsx",
          "tube_40.xlsx",
          "tube_60.xlsx",
          "tube_80.xlsx",
          "tube_100.xlsx")
name <-   c(expression("Volt.: "*20*V),
            expression("Volt.: "*40*V),
            expression("Volt.: "*60*V),
            expression("Volt.: "*80*V),
            expression("Volt.: "*100*V))

directory <- setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")



pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/4) Barplot TMA/barplot_CT.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  

EIC_barplot(main,data,type,directory,name)
dev.off()

#########################################################################
#########################################################################
#########################################################################
#########################################################################


# f) sheat gas & gas flow
#
#1) TIC 
#-------------------------
#    i) seperate

names <- c("10-5", "10-20", "10-50",
           "30-5", "30-20", "30-50",
           "60-5", "60-20", "60-50",
           "90-5", "90-20", "90-50")
max_time <- rep(8,12)
ylim_max <- rep(3e6,12)


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_SEPERATE.pdf",width = 7.26, height = 10, bg = "white",          
    colormodel = "cmyk",paper = "a4") 


par(mar=c(3.5,5,2,2))
par(mfrow=c(4,3))
TIC_separate("gas_TIC.xlsx",names,max_time,ylim_max)
dev.off()


#    i) together


names <- "Sheat Gas-Gas Flow"
max_time <- 8
legend <- c("10-5", "10-20", "10-50",
            "30-5", "30-20", "30-50",
            "60-5", "60-20", "60-50",
            "90-5", "90-20", "90-50")


pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/1) TIC/TIC_TOGETHER.pdf",width = 7.26, height = 4.26, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 
par(mfrow=c(1,1))
par(mar=c(3.5,5,2,2))
TIC_together("gas_TIC.xlsx",names,max_time,legend)
dev.off()

#
#2) EIC of all substances
#-------------------------

# i) All in one 4x3 matrix
file <- c("gas_10-5.xlsx","gas_10-20.xlsx","gas_10-50.xlsx",
          "gas_30-5.xlsx","gas_30-20.xlsx","gas_30-50.xlsx",
          "gas_60-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx",
          "gas_90-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx"
          )
  
  
time <- 8
name <- c("10-5", "10-20", "10-50",
          "30-5", "30-20", "30-50",
          "60-5", "60-20", "60-50",
          "90-5", "90-20", "90-50")
type <- ""
ylim_max <- rep(3e6,12)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_together.pdf",width = 7.26, height = 10, bg = "white",          
    colormodel = "cmyk")  #, #paper = "a4r") 


par(mfrow=c(4,3))
par(mar=c(3.5,5.5,2,2))
for(i in 1:12){
  EIC(file[i], time, name=name[i], type, ylim_max[i])
}
dev.off()

# for each temperature

Temp <- c("10-5", "10-20", "10-50",
          "30-5", "30-20", "30-50",
          "60-5", "60-20", "60-50",
          "90-5", "90-20", "90-50")
par(mfrow=c(1,1))
par(mar=c(3.5,7,2,2))
ylim_max<-c(rep(2e6,3),rep(2.7e6,3),rep(3.2e6,3),rep(3.2e6,3))
for(i in 1:12){
  pdf(paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/2) EIC/EIC_",Temp[i],".pdf"),width = 7.26, height = 4.26, bg = "white",          
      colormodel = "cmyk")  #, #paper = "a4r") 
  par(mar=c(3.5,6,2,2))
  EIC(file[i], time, name=name[i], type, ylim_max[i])
  dev.off()
}

#
#3) EIC of TMA
#-------------------------

# i) EIC TMA all together

fileT <- c("gas_10-5.xlsx","gas_10-20.xlsx","gas_10-50.xlsx",
           "gas_30-5.xlsx","gas_30-20.xlsx","gas_30-50.xlsx",
           "gas_60-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx",
           "gas_90-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx"
)
legend <- c("10-5", "10-20", "10-50",
            "30-5", "30-20", "30-50",
            "60-5", "60-20", "60-50",
            "90-5", "90-20", "90-50")
sheet <- "TMA"
xmin <- 3
xmax <- 5
name <- "Capillary Temperature: TMA"
ylim_max=1e5
xlab=T
ylab=T
linesT=c(F,T,T,T,T,T,T,T,T,T,T,T)

lineColT=c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black",
           "#70380f","#96b300","#ff4d4d","#004700","#ddcb40","#470000")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_together.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  
for(i in 1:12){
  par(mar=c(3.5,6,2,2))
  file = fileT[i]
  lines = linesT[i]
  lineCol = lineColT[i]
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
}

legend("topright",bty="n",legend=legend,pch=15,
       col = lineColT,pt.cex = 1.2)
dev.off()


# ii) Seperate EIC TMA

Temp <- c("10-5", "10-20", "10-50",
          "30-5", "30-20", "30-50",
          "60-5", "60-20", "60-50",
          "90-5", "90-20", "90-50")
par(mfrow=c(1,1))
ylim_maxT = rep(8e4,12)
for(i in 1:12){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  pdf(  paste0("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA",Temp[i],".pdf"),
        width = 7.26, height = 4.26, bg = "white")       
  par(mar=c(3.5,6,2,2))
  par(mfrow=c(4,3))
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright",legend="TMA",bty="n")
  dev.off()
}

# iii)
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/3) EIC TMA/EIC_TMA_matrix.pdf",
      width = 7.26, height = 10, bg = "white")
par(mar=c(3.5,6,2,2))
par(mfrow=c(4,3))
for(i in 1:12){
  file = fileT[i]
  lines = F
  lineCol = "black"
  name=legend[i]
  ylim_max=ylim_maxT[i]
  EIC_SINGLE(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax)
  legend("topright","TMA",bty="n")
}
dev.off()
#
#4) Barblot intensity TMA 
#-------------------------

main = "Sheat gas- Gas flow (Intensities TMA)" 
type <- "intensity"
data <- c("gas_10-5.xlsx","gas_10-20.xlsx","gas_10-50.xlsx",
          "gas_30-5.xlsx","gas_30-20.xlsx","gas_30-50.xlsx",
          "gas_60-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx",
          "gas_90-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx"
)
directory <- setwd("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/ESI tune")


name <- c("10-5", "10-20", "10-50",
          "30-5", "30-20", "30-50",
          "60-5", "60-20", "60-50",
          "90-5", "90-20", "90-50")
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/4) Barplot TMA/barplot_CT.pdf",width = 7.26, height = 4.26, bg = "white",colormodel = "cmyk")  

EIC_barplot(main,data,type,directory,name)
dev.off()


############################################################################

gasFlow <- c("10-5", "10-20", "10-50",
             "30-5", "30-20", "30-50",
             "60-5", "60-20", "60-50",
             "90-5", "90-20", "90-50")

col=rep(c("#72791C","#1269B0","#A8322D"), 4)

name <- c("Sheath gas 10 abr", "Sheath gas 30 abr", 
          "Sheath gas 60 abr", "Sheath gas 90 abr")

lines <- rep(c(F, T, T), 4)
time <- 6
type="RT"

# adaptiert all ylim pro graph
ylim_max<-c(rep(2.5e4,3),rep(6e4,3),rep(9e4,3),rep(9e4,3))
sheet="TMA"
xlab=T
ylab=T

data <- matrix(c("gas_10-5.xlsx","gas_10-20.xlsx","gas_10-50.xlsx",
          "gas_30-5.xlsx","gas_30-20.xlsx","gas_30-50.xlsx",
          "gas_60-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx",
          "gas_90-5.xlsx","gas_60-20.xlsx","gas_60-50.xlsx"
),nrow=4,byrow = T)

# layout isch als ersatz für par(mfrow=c(2,2))
xmin=2
xmax=6
pdf("C:/Users/Avrohom Moyal/Downloads/PROJEKT LAURA/PLOTS/5) Sheat gas 4 plot/EIC_MAIN_COOL_PLOT.pdf",width = 7.26, height = 4.26, bg = "white")        
    
layout(matrix(c(1,2,3,4,5,5),ncol=2,byrow = T),heights = c(7,7,1.5) )

par(mar=c(3.8,6.2,3,2))
for(i in 1:4){
  file = data[i,1]
  EIC_SINGLE(file,sheet="TMA", name[i],ylim_max=ylim_max[i*3-2],xlab=T,ylab=T,lineCol= col[1], lines=F,xmin,xmax)
  for(j in 1:2){
    file=data[i,j+1]
    EIC_SINGLE(file,sheet="TMA", name[i],ylim_max=ylim_max[i*3-2],xlab=T,ylab=T,lineCol= col[j+1], lines=T,xmin,xmax)
    
  }
  
}             


# also falls mer das TMA im graph wegneh will denn muss mer nur im function file
# linie 259 lösche oder es # vorher lege damit wieder verwende chasch 

# legende
par(mar=c(0,0,0,0))
plot.new()
legend("center",col = col[1:3], legend = c("Auxilliary gas: 5 abr",
                                           "Auxilliary gas: 20 abr",
                                           "Auxilliary gas: 50 abr"),
       bty = "n",horiz = T, pch=15,pt.cex = 2,cex=1.2)

dev.off()







