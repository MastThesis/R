#===========================================================
# R-script
# Experiment: ms27
# Author: Laura Campigotto
# Date: 14.04.2022
#===========================================================

#===========================================================
# libraries used 

library("readxl", quietly = T) # xlsx to R


#===========================================================

# Function TIC Benchmarking


TIC_separate <- function(file,names,max_time,ylim_max){
  options(scipen = 999)
  sheets <- excel_sheets(file)
  names <- names
  for(i in 1:length(sheets)){
    
    data <- read_excel(file,sheet = sheets[i])
    data <- data.frame(data)
    
    time <- data[,1]
    intensity <- data[,2]
    
    plot(time, intensity,
         xlab = "", 
         ylab = "",
         yaxt = "n",
         main = names[i],
         ylim = c(0,ylim_max[i] ),
         xlim = c(0, max_time[i]),
         type = "l",
         cex = 1,
         col = "black", cex.main=0.8,
         mgp=c(1,0.5,0)
    )
    
   
    yaxis<-axis(2,labels = F,tick=F)
    yaxis<-format(yaxis,scientific=T)
    axis(2, at = yaxis, labels = yaxis, las = 1, cex.axis = 0.7)
    mtext(side = 2, text = "Intensity", line = 3.8, cex = 0.8)
    mtext(side = 1, text = "Time [min]", line = 2, cex = 0.8)
  }
  
  
}



TIC_together <- function(file,names,max_time,legend){
  sheets <- excel_sheets(file)
  col <- c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black",
           "#70380f","#96b300","#ff4d4d","#004700","#ddcb40","#470000")
  col <- col[1:length(sheets)]
  max=0
  for(i in 1:length(sheets) ){
    data <- read_excel(file, sheet = sheets[i])
    data=data.frame(data)
    if(max(data[,2]) > max){
      max = max(data[,2])
    }else{
      max = max
    }
  }
  
  
    plot(0,0,ylim=c(0,1.2*max),xlim=c(0,max_time),
         main=names,yaxt="n",ylab="",xlab="",type="n")
    
    yaxis<-axis(2,labels = F,tick=F)
    yaxis<-format(yaxis,scientific=T)
    axis(2, at = yaxis, labels = yaxis, las = 1, cex.axis = 0.7)
    mtext(side = 2, text = "Intensity", line = 3.8, cex = 1)
    mtext(side = 1, text = "Time [min]", line = 2, cex = 1)
    
    for(i in 1:length(sheets)){
    i=4
      data <- read_excel(file,sheet = sheets[i])
      data <- data.frame(data)
      
      time <- data[,1]
      intensity <- data[,2]
      
      lines(time, intensity,xlim=c(0,max_time),
           type = "l",
           cex = 1,
           col = col[i],
      )
      
      
      
    }
    
    legend("topright",legend=legend,col=col,pch=15, pt.cex=1.1,bty="n" )
}
  
  
  














# Function TIC Benchmarking

benchmarking_TIC <- function(file){
  options(scipen = 999)
  sheets <- excel_sheets(file)
  names <- c("TIC BEH Amide", "TIC ACE HILIC A", "TIC ACE HILIC B", "TIC ACE HILIC N")
  max_time <- c(10, 30, 30, 30)
  
  par(mar = c(4, 5, 2.5, 1), mfrow = c(2,2))
  
  for(i in 1:length(sheets)){
    
    data <- read_excel(file,sheet = sheets[i])
    data <- data.frame(data)
    
    time <- data[,1]
    intensity <- data[,2]
   
    plot(time, intensity,
         xlab = "", 
         ylab = "",
         yaxt = "n",
         main = names[i],
         ylim = c(0, 2e6),
         xlim = c(0, max_time[i]),
         type = "l",
         cex = 1,
         col = "black",
    )
    
    label = c(0,
              expression("0.5 x " *10^{6}),
              expression("1.0 x " *10^{6}),
              expression("1.5 x " *10^{6}),
              expression("2.0 x " *10^{6})
              )
    axis(2, at = c(0,5e5,1.0e6,1.5e6,2.0e6), labels = label, las = 1, cex.axis = 0.7)
    mtext(side = 2, text = "Intensity", line = 3.8, cex = 1)
    mtext(side = 1, text = "Time [min]", line = 2.5, cex = 1)
  }
  
}


#===========================================================
# Function EIC 
options(scipen=999)




EIC <- function(file, Time, name, type, ylim_max){
  options(scipen=999)
  sheets <- excel_sheets(file)
  max_time <- Time
  

  max=0
  for(i in 1:length(sheets) ){
    data <- read_excel(file, sheet = sheets[i])
    data=data.frame(data)
    if(max(data[,2]) > max){
      max = max(data[,2])
    }else{
      max = max
    }
  }
    
    data <- read_excel(file,sheet = sheets[1])
    data <- data.frame(data)
    
    time <- data[,1]
    intensity <- data[,2]

    plot(time[1], intensity[1],
         xlab = "", 
         ylab = "",
         yaxt = "n",
         main = name,
         ylim = c(0, ylim_max), #ylim = c(0, 1.15*max),
         xlim = c(0, max_time),
         type = "l",
         cex = 1,
         col = "black",
         las = 1
    )
    
    
    yax <- axis(2, label=F,tick=F)
    yax <- format(yax,scientific = T)
    axis(2,at=yax,label=yax,las=2)
    mtext(side = 2, text = "Intensity", line = 4.2, cex = 1)
    mtext(side = 1, text = "Time [min]", line = 2.0, cex = 1)
    
    
    maximas <- matrix(nrow = 9, ncol = 3)
    colnames(maximas)<-c("name","RT","Int")
    maximas[,1] <- sheets
    for(i in 1:9){
      data <- read_excel(file, sheet = sheets[i])
      data <- data.frame(data)
      time <- data[,1]
      intensity <- data[,2]
      maximas[i,2] <- time[which(intensity == max(intensity))]
      maximas[i,3] <- max(intensity)
    }
    
    maximas_sort <- maximas[order(as.numeric(maximas[,2]),decreasing = F),]
    
    
    legend_name <- rep(NA,9)
    RT <- format(round(as.numeric(maximas_sort[,2]),digits = 2),scientific = F)
    int <- format(signif(as.numeric(maximas_sort[,3]),digits = 2),scientific = T)
    
    
    
    for(i in 1:9){
      if(type=="RT"){
      legend_name[i] <- paste0(i,": ",maximas_sort[i,1]," (RT: ",RT[i],")")
      }else if(type == "int"){
      legend_name[i] <- paste0(i,": ",maximas_sort[i,1]," (Int: ",int[i],")")  
      }else if(type==""){
        legend_name[i] <- paste0(i,": ",maximas_sort[i,1]) 
      }
    }
    
    
    
    for (i in 1:length(sheets)){
      data <- read_excel(file, sheet = sheets[i])
      data <- data.frame(data)
      time <- data[,1]
      intensity <- data[,2]
      lines(time, intensity)
      
      max_int <- max(intensity)
      max_t <- time[which(intensity == max(intensity))]
      
      #text(max_t,max_int,sheets[i],offset = 1.3, pos = 3, srt =90)
      number <- which(as.numeric(maximas_sort[,2]) == max_t)
      text(max_t,max_int,number,offset = 1.3, pos = 3)
      
      
    }
    
    
    
    legend("topright",legend = legend_name,bty = "n", cex = 0.7)
}



EIC_SINGLE<-function(file,sheet, name, ylim_max, xlab, ylab, lineCol,lines,xmin,xmax){
 
  options(scipen=999)

 

  
  data <- read_excel(file,sheet = sheet)
  data <- data.frame(data)
  
  time <- data[,1]
  intensity <- data[,2]
  if(lines == F){
  plot(time, intensity,
       xlab = "", 
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       main = "",
       ylim = c(0,ylim_max), #ylim = c(0, 1.15*max),
       xlim = c(xmin, xmax),
       type = "l",
       cex = 1,
       col = lineCol,
       las = 1,
       cex.main=0.7
  )
    mtext(3,text=name,line=0.8,cex=0.9)
  }else{
    lines(time, intensity, col = lineCol)
  }
  
  Y<-axis(2,labels = F,tck=F)
  Y<-Y[which(Y<=ylim_max)]
  Y<-format(Y,scientific = T)
  axis(2,at=Y,labels = Y,las=2)
  
  if(xlab==T){
    
    axis(1)
    mtext(side = 1, text = "Time [min]", line = 2.5, cex = 1)
    
  }
  #legend("topright",legend = sheet,bty = "n", cex = 1)
  
  if(ylab==T){
  mtext(side = 2, text = "Intensity", line = 4.8, cex = 1)
  }
  
  maximas <- matrix(nrow = 1, ncol = 3)
  colnames(maximas)<-c("name","RT","Int")
  maximas[,1] <- sheet
  
  
    maximas[1,2] <- round(time[which(intensity == max(intensity))],digits = 2)
    maximas[1,3] <- signif(max(intensity),digits = 2)
  
 # return(list(maximas))
  
  
  
  
 
  
  
  
  
  
  
  
  
}




