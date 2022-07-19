

EIC_barplot<-function(main,data,type,directory,name){

library("readxl", quietly = T) # xlsx to R
if(type=="RT"){
  options(scipen = 999)
}else{
  options(scipen = 999)
}


length <- length(data)
names <- excel_sheets(paste0(data[1]))

frame <- matrix(nrow = length, ncol = 9)
colnames(frame) <- names
rownames(frame) <- name
for(j in 1:length(data)){

    for(i in 1:9){
        DAT <- read_xlsx(data[j],sheet = names[i])
        DAT <- data.frame(DAT)
        MAX <- DAT[which(DAT$Intensity== max(DAT$Intensity)),]
        MAX <- c(MAX[1],MAX[2])
        if(type == "RT"){
          frame[j,i] <- as.numeric(MAX[1])
        }else{
          frame[j,i] <- as.numeric(MAX[2])
        }
        
        
    }

}

if(type=="RT"){
  ylab = "RT"
}else{
  ylab = "Intensity"
}


if(type=="RT"){
par(mar=c(4,6.2,2,2))
layout(rbind(1,2), heights=c(7,1)) 
color <- c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black",
           "#70380f","#96b300","#ff4d4d","#004700","#ddcb40","#470000")
color <- color[1:length(data)]
barplot(frame,beside = T,names.arg = names,col = color,las = 2,ylab = "",
        main = main,cex.main = 0.7)

YAXIS = axis(2,labels = F,tick = F)
YAXIS = format(YAXIS,scientific=T )
axis(2,at=YAXIS,labels = YAXIS,las=T)
mtext(side = 2, text = ylab, line = 1.8, cex = 1)
}else{
  par(mar=c(4,6.2,2,2))
  layout(rbind(1,2), heights=c(7,1)) 
  color <- c("#72791C", "#1269B0", "#A8322D","#767171","#C55A11","#83136E","black",
  "#70380f","#96b300","#ff4d4d","#004700","#ddcb40","#470000")
  color <- color[1:length(data)]
  barplot(frame,beside = T,names.arg = names,col = color,las = 2,ylab = "",
          main = main,cex.main = 0.7,yaxt="n")
  YAXIS = axis(2,labels = F,tick = F)
  YAXIS = format(YAXIS,scientific=T )
  axis(2,at=YAXIS,labels = YAXIS,las=T)
  mtext(side = 2, text = ylab, line = 4.2, cex = 1)
}




par(mar=c(0, 5, 0, 2))

plot.new()
legend("center",col = color, legend = name,bty = "n",horiz = T, pch=15,pt.cex = 1.1,cex=0.8)
}







