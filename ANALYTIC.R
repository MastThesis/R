#===============================================================
# funktion_validation
#===============================================================
#HELP PRINT
library(insight, quietly = T) # colour print for help output

print_color("For help use help function \n","red")
print_color('For Input help use:  help("input") \n',"red")
print_color('For Output help use:  help("output") \n',"red")

help<-function(help){
  
if(help=="input"){
print_color("HELP: \n","red")
print_color("4 parameters: name,directory,Type and plot \n","red")
print("",quote=F)
print_color("1. name: Name of the Substance to be analysed \n","blue")
print('e.g: name = "PETU" ',quote=F)
print("",quote=F)
print_color("2. directory: Directory where Excel File is saved \n","blue")
print('e.g: directory = "C:/Users/Avrohom Moyal/Downloads"',quote=F)
print("",quote=F)
print_color("3. Type: What form of concentration values used \n","blue")
print('a) "Theory": Theoretical weight in without correction [mg/mL]',quote=F)
print('b) "Experimental": Weight in corrections [mg/mL]',quote=F)
print('c) "Molarity": Same as Experimental but in [muM]',quote=F)
print('e.g: Type = "Molarity"',quote=F)
print("",quote=F)
print_color("4. plot: Which plot to obtain as output \n","blue")
print('a) "plot_area": Concentration~Area with regression line and R^2',quote=F)
print('b) "plot_rt": Concentration~RT with regression line',quote=F)
print('c) "plot_CV": Concentration~Coefficiant of Variation with 0.2 treshold',quote=F)
print('d) "plot_residuals": Concentration~Residuals of plot (a)',quote=F)
print('e.g: plot = "plot_area"',quote=F)
print("",quote=F)
print("",quote=F)
}else{
print_color("List of table outputs \n","red")
print("",quote=F)
print_color("1. DATA: Raw input data (concentrations/area/RT)   \n \n","red")
print_color("2. AREA_TABLE: Area and mean for all 3 Runs (conc./run 1/run 2/run 3/mean)   \n \n","red")
print_color("3. RT_TABLE: RT and mean for all 3 Runs (conc./run 1/run 2/run 3/mean)   \n \n","red")
print_color("4. LINEAR_REG_A: Summary of linear regression area   \n \n","red")
print_color("5. LINEAR_REG_rt: Summary of linear regression rt   \n \n","red")
print_color("6. SD_TABLE: Standard deviation for Area and RT (conc/area/RT) \n  \n","red")
print_color("7. CV_TABLE: Coefficiant of Variation for Area and RT (conc/area/RT) \n \n","red")
print_color("8. Grubbs_AREA: Two sided grubbs test for area data  \n \n","red")
print_color("9. Grubbs_RT: Two sided grubbs test for RT data  \n \n","red")
print_color("10. final_chart: summary of: RANGE/REGRESSION EQUATION/R^2/LOD \n \n","red")
print_color("11. COR: Correlation between conc and area  \n \n","red")
print_color("All function can be called using $ sign  \n","red")
}
}

par(mfrow=c(1,1),mar=c(4,4,4,4))


LAURA <- function(name, directory, Type, plot){
  options(scipen = 3)
  #-------------------------------------------------------------
  # libraries used in function 
  library("readxl", quietly = T) # xlsx to R
  library(outliers, quietly = T) # gubbs test
 
  #-------------------------------------------------------------
 
  
  
  #-------------------------------------------------------------
  dev <- function(x, y, sy){
    arrows(x, y - sy, x, y + sy, code = 3, angle = 90, length = 0.04)
  }
  #-------------------------------------------------------------
  # import data
  
  setwd(directory)
  data      <- read_excel("name.xlsx", sheet = name)
  plot=plot
  # to find out how many rows 
  data      <- data.frame(data)
  
  if(length(data[,1]) == 19){
    data <- read_excel(name2, col_names = T)
    } 
  
  # dataframe with conc / area / RT
  data <- data.frame(data)
  colnames(data) <- c("conc.","area","RT")
  rownames(data) <- c ("1.1","1.2","1.3","1.4","1.5","1.6",
                       "2.1","2.2","2.3","2.4","2.5","2.6",
                       "3.1","3.2","3.3","3.4","3.5","3.6")
  exact <- data[1:6,1]
  
  # substances and weight-ins
  Substance <- matrix(ncol = 4, nrow = 9)
  colnames(Substance) <- c("name", "Weight in", "Conc. Exp.", "Molecular Weight")
  
  Substance[,1] <- c("PETU", "DEA", "TEA",
                     "TMA", "CHO", "TMAO",
                     "BET", "ACAR", "CAR")
  Substance[,2] <- c(20.2, "-", "-",
                     17.93, 18.316, 20.73,
                     20.75, 20.51, 17.12)
  Substance[,3] <- c(20.2/20, 1, 1,
                     17.93/20, 18.316/20, 20.73/20,
                     20.75/20, 20.51/20, 17.12/20)
  Substance[,4] <- c(180.27, 73.14, 101.19, 
                     59.11, 104.17, 75.11, 
                     117.15, 203.24, 161.20)
  #-------------------------------------------------------------
  # Concentrations vector
    if( Type == "Theory"){
    concentration <- data[1:6, 1]
    
    }else if( Type == "Experimental"){
    dilution <- 1000 / data[1:6, 1]
    concentration <- as.numeric(
      Substance[which(Substance[,1] == name), 3]) / dilution * 1000 #ug/mL
    
    }else{
    dilution <- 1000 / data[1:6, 1]
    concentration <- ((as.numeric(Substance[which(Substance[,1]== name),3])
                      /dilution)
                      /as.numeric(Substance[which(Substance[,1]==name),4])) * 10^6 #muM
  }
  data[,1] = concentration 
  #-------------------------------------------------------------
  # xlab in uM ug/mL
  
  if( Type == "Molarity"){
    label <- expression(Concentration*" "*"["*mu*M*"]")
    
  }else{
    label <- expression(Concentration*" "*"["*mu*g~mL^{-1}*"]")
  }
  #===============================================================
  # Extract area
  AREA <- cbind(concentration,
                data[1:6,2], 
                data[7:12,2],
                data[13:18,2])
  colnames(AREA) <- c("conc.","run 1","run 2","run 3")
  
  Mean_AREA <- rep(NA, 6)
  for(i in 1:6){
    Mean_AREA[i] <- mean(AREA[i, 2:4])
  }
  
  AREA_TABLE <- cbind(AREA, Mean_AREA)
  colnames(AREA_TABLE) <- c("conc.","run 1","run 2","run 3","mean")
  #-------------------------------------------------------------
  # Extract RT
  RT <- cbind(concentration,
              data[1:6,3],
              data[7:12,3],
              data[13:18,3])
  colnames(RT) <- c("conc.","run 1","run 2","run 3")
  
  Mean_RT<-rep(NA, 6)
  for(i in 1:6){
    Mean_RT[i] <- mean(RT[i,2:4])
  }  
  
  RT_TABLE<-cbind(RT,Mean_RT)
  colnames(RT_TABLE)<-c("conc.","run 1","run 2","run 3","mean")
  #===============================================================
  #-------------------------------------------------------------
  # Standard deviation 
  SD_TABLE <- matrix(ncol = 3, nrow = 6)
  colnames(SD_TABLE) <- c("conc.","area","RT")
  SD_TABLE[,1] <- concentration
  for(i in 1:6){
    SD_TABLE[i,2] <- sd(AREA[i,2:4])
    SD_TABLE[i,3] <- sd(RT[i,2:4])
  }
  #-------------------------------------------------------------
  # PLOT + LINEAR REGRESSION CONC~AREA
  model <- lm(Mean_AREA ~ concentration)
  Linear_regression_A <- summary(model)
  
  X <- seq(0.8 * min(concentration), 
           1.1 * max(concentration), 0.01)
  Y <- predict(object = model, 
               newdata = list(concentration = X), 
               interval = "confidence")
  
  min <- min(Mean_AREA - SD_TABLE[,2])
  max <- max(Mean_AREA + SD_TABLE[,2])
  if(plot=="plot_area"){
  plot(concentration, Mean_AREA,
       xlab = "", 
       ylab = "",
       yaxt = "n",
       main = paste0(name,"  area"),
       ylim = c(min, max),
       xlim=c(min(X), max(X)),
       pch = 1,
       cex = 1,
       col = "white",
       mgp = c(2.5,0.5,0)
      )
  Y_A<-axis(2,labels = F,tck=F)
  Y_A <- format(Y_A,scientific=T )
  axis(2,at=Y_A,labels = Y_A,las=2,cex.axis = 0.7)
  mtext(side=1,text = label,line=2,cex=0.7)
  mtext(side=2,text = "area",line=3.2,cex=0.7)
  
  sd_A <- SD_TABLE[, 2]
  dev(concentration, Mean_AREA, sd_A)
  points(concentration, Mean_AREA, pch = 1, cex = 1, col = "black")
  
  matlines(X, Y, lwd = c(1, 0.5, 0.5), 
          lty = c(1, 2, 2), 
           col = c("red", "black", "black"))
  mylabel=bquote(italic(R)^2 == .(format(Linear_regression_A$r.squared , digits = 4)))
  legend("topleft",legend=mylabel,bty = "n",cex = 0.7)
  }
  
  correlation<-cor(concentration,Mean_AREA,method = "pearson")
  #-------------------------------------------------------------
  # PLOT + LINEAR REGRESSION CONC~RT
  model <- lm(Mean_RT ~ concentration)
  Linear_regression_RT <- summary(model)
  
  X <- seq(0.8 * min(concentration), 
           1.1 * max(concentration), 0.01)
  Y<-predict(object = model, 
             newdata = list(concentration = X),
             interval = "confidence")
  
  min <- min(Mean_RT - SD_TABLE[,3])
  max <- max(Mean_RT + SD_TABLE[,3])
  if(plot=="plot_rt"){
  plot(concentration, Mean_RT,
       xlab = label, 
       ylab = expression(area),
       main = paste0(name,"  RT"),
       ylim = c(min, max),
       xlim = c(min(X), max(X)),
       pch = 1,
       cex = 1)
  
  matlines(X, Y, lwd = c(1, 0.5, 0.5), 
           lty = c(1, 2, 2), 
           col = c("red", "black", "black"))
  sd_RT <- SD_TABLE[, 3]
  dev(concentration, Mean_RT, sd_RT)
  }
  
  #-------------------------------------------------------------
  # RESIDUALS PLOT OF AREA
  if(plot=="plot_residuals"){
  residual <- Linear_regression_A$residuals
  
  plot(concentration, residual,
       xlab = label, 
       ylab = "residual",
       main=paste0(name," Residuals Plots"),
       pch = 1,
       cex = 1)
  
  abline(h = 0, col = "black", lty = 2)
  }
  #-------------------------------------------------------------
  # Coefficient of variation of area
  
  CV_TABLE <- matrix(ncol = 3, nrow = 6)
  colnames(CV_TABLE) <- c("conc.", "area", "RT")
  CV_TABLE[,1] <- concentration
  
  for(i in 1:6){
    CV_TABLE[i,2]<-SD_TABLE[i,2] / Mean_AREA[i]
    CV_TABLE[i,3]<-SD_TABLE[i,3] / Mean_RT[i]
    
  }
  if(plot=="plot_CV"){
  plot(concentration, CV_TABLE[,2], 
       main=paste0(name," CV"),
       xlab = label,
       ylab="Coefficient of Variation",
       cex = 1,
       pch = 1)
  abline(h = 0.2, col = "black", lty = 2)
  }

    
 
  #===============================================================
  # grubbs tests
  #---------------------------------------------------------------
  Grubbs_AREA <- matrix(ncol = 3, nrow = 6)
  colnames(Grubbs_AREA) <- c("conc.","outlier","p-value")
  Grubbs_AREA[,1] <- concentration
  
  for(i in 1:6){
    test<-grubbs.test(AREA[i, 2:4],type=11,two.sided = T)
    pvalue <- as.numeric(test$p.value)
    if(pvalue < 0.05){
      Grubbs_AREA[i,2] <- "NO"
      if(pvalue == 0){
        pvalue <- "< 2.2e-16"
      }
      Grubbs_AREA[i,3] <- pvalue
      
    }
    else{
      Grubbs_AREA[i,2] <- "YES"
      Grubbs_AREA[i,3] <- pvalue
    }
  }
  #---------------------------------------------------------------
  Grubbs_AREA <- matrix(ncol = 3, nrow = 6)
  colnames(Grubbs_AREA) <- c("conc.","outlier","p-value")
  Grubbs_AREA[,1] <- concentration
  
  for(i in 1:6){
    test<-grubbs.test(AREA[i, 2:4],type=11,two.sided = T)
    pvalue <- as.numeric(test$p.value)
    if(pvalue < 0.05){
      Grubbs_AREA[i,2] <- "NO"
      if(pvalue == 0){
        pvalue <- "< 2.2e-16"
      }
      Grubbs_AREA[i,3] <- pvalue
      
    }
    else{
      Grubbs_AREA[i,2] <- "YES"
      Grubbs_AREA[i,3] <- pvalue
    }
  }
  #---------------------------------------------------------------
  Grubbs_RT<-matrix(ncol=3, nrow = 6)
  colnames(Grubbs_RT)<-c("conc.","outlier","p-value")
  Grubbs_RT[,1] <- concentration
  options(warn = -1)
  
  for(i in 1:6){
    test <- try(grubbs.test(RT[i,2:4], type = 11, two.sided = T),silent = T)
    if(test[[1]] == "Error in uniroot(f, c(0.001, 0.9999), q = q[i], n = n) : \n  f() values at end points not of opposite sign\n"){
      pvalue <- 0 
    }else{
      pvalue <- as.numeric(test$p.value)
    }
    if(pvalue < 0.05){
      Grubbs_RT[i,2] <- "NO"
      if(pvalue == 0){
        pvalue <- "< 2.2e-16"
      }
      Grubbs_RT[i,3] <- pvalue
    }
    else{
      Grubbs_RT[i,2] <- "YES"
      Grubbs_RT[i,3] <- pvalue
    }
  }
  options(warn = 1)
  #---------------------------------------------------------------
  # Extract linear regrassion 
  
  # Y=aX+b
  a = Linear_regression_A$coefficients[2]
  b = Linear_regression_A$coefficients[1]
  if(b>0){
    
    Formula <- paste0("Y = ",round(a,digits = 4)," X + ",round(abs(b),digits = 4) )
  } else{
    Formula <- paste0("Y = ",round(a,digits = 4)," X - ",round(abs(b),digits = 4) )
  }
  
  
  range <- paste0(min(exact),"-",max(exact))
  
  R <- round(Linear_regression_A$r.squared, digits=4)
  
  LOQ <- min(exact)
  
  FINAL_OUTPUT<-c(range, Formula, R, LOQ)
  
  results <- list(data,
                  AREA_TABLE, 
                  RT_TABLE,
                  Linear_regression_A,
                  Linear_regression_RT,
                  SD_TABLE,
                  CV_TABLE,
                  Grubbs_AREA,
                  Grubbs_RT,
                  FINAL_OUTPUT,
                  correlation
                  )
  
  names(results)<-c("DATA",
                    "AREA_TABLE",
                    "RT_TABLE",
                    "LINEAR_REG_A",
                    "LINEAR_REG_rt",
                    "SD_TABLE",
                    "CV_TABLE",
                    "Grubbs_AREA",
                    "Grubbs_RT",
                    "final_chart",
                    "COR")
                    
  return(results)
}
