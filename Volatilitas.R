#estimasi volatilitas
setwd("D:\\KULIAH SEMT 4\\PSK\\PSK R\\UAS")

X=scan()
vola<-function(X,n,tau){
  
  m<-length(X)
  y<-array(NA,dim=c(m,2))
  
  for(i in 2:m){
    y[i,1]=log(X[i]/X[i-1])}
  
    Rt.bar=mean(y[,1],na.rm=TRUE)
    y[,2]=(y[,1]-Rt.bar)^2
  
  jumlah=sum(y[,2], na.rm = TRUE)
  sigma=sqrt(1/(n-1)*jumlah)
  volatilitas=sigma*sqrt(tau)
  return(volatilitas)
  
}

vola(X, 251, 252)
