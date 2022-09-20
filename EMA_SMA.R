
# n number of days per average

SMA<-function(n,name,f,t,column){
  
  ff<-as.Date(f)-3*n
  
  data_prime<-getSymbols(name,src="yahoo",from=f,to=t, auto.assign = F)
  data_prime<-data.frame(data_prime)
  
  data<-getSymbols(name,src="yahoo",from=ff,to=t, auto.assign = F)
  data<-data.frame(data)
  
  data<-data[(length(data[,1])-length(data_prime[,1])+1-(n-1)):length(data[,1]),]
  
  colnames(data)<-c("Open","High","Low","Close","Volume","Adjusted")
  
  matrix<-cbind(rownames(data)[n:length(data[,1])],rep(NA,length(data[,1])-n+1) )
  matrix<-data.frame(matrix)
  for(i in 1:(length(data[,1])-n+1) ){
    data_cut<-data[i:(n-1+i),column]
    matrix[i,2]<-mean(data_cut)
  }
  colnames(matrix)<-c("Date","SMA")
  return(matrix)
  
  
  
}


EMA<-function(n,name,f,t,column){
  data<-getSymbols(name,src="yahoo",from=f,to=t, auto.assign = F)
  data<-data.frame(data)
  colnames(data)<-c("Open","High","Low","Close","Volume","Adjusted")
  
  
  M<-2/(n+1)
  matrix<-cbind(rownames(data)[1:length(data[,1])],rep(NA,length(data[,1])) )
  matrix<-data.frame(matrix)
  ema_count<-rep(NA,length(data[,1]))
  ema_count[1]<-mean(data[1:5,column])
  
  for(i in 2:length(data[,1]) ){
    ema<-((data[i,column]-ema_count[i-1])*M)+(ema_count[i-1])
    ema_count[i]<-ema
  }
  matrix[,2]<-ema_count
  colnames(matrix)<-c("Date","SMA")
  return(matrix)
  
  
  
}



BB<-function(n,name,f,t){
  ff<-as.Date(f)-3*n
  
  data_prime<-getSymbols(name,src="yahoo",from=f,to=t, auto.assign = F)
  data_prime<-data.frame(data_prime)
  
  data3<-getSymbols(name,src="yahoo",from=ff,to=t, auto.assign = F)
  data<-data.frame(data3)
  
  
  data<-data3[(length(data[,1])-length(data_prime[,1])+1-(n-1)):length(data[,1]),]
  
  colnames(data)<-c("Open","High","Low","Close","Volume","Adjusted")
  
  data_prime<-data.frame(data)
  rowname<-rownames(data_prime)
  
  data<-matrix(data[,c(2,3,4)],ncol = 3)
  HLC<-matrix(as.numeric(data),ncol=3)
  colnames(HLC)<-c("High","Low","Close")
  rownames(HLC)<-rowname
  BB<-BBands(HLC, n , sd = 2)
  nas_index<-is.na(BB[,1])
  nas_index<-which(nas_index!=T)
  BB<-BB[nas_index,]
  return(BB)
}










