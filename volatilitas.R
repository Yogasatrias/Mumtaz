#volatility

X=scan()

vola<-function(X,n, tau){
  m<-length(X)
  y<-array(NA, dim=c(m, 2))
  
  for(t in 2:m){
    y[t,1]=log(X[t]/X[t-1])}
  
  Rt.bar=mean(y[,1], na.rm= TRUE)
  y[,2]=(y[,1]-Rt.bar)^2
  
  jumlah=sum(y[,2], na.rm= TRUE)
  sigma=sqrt((1/(m-1))*jumlah)
  volatilitas=sigma*sqrt(tau)
  return(volatilitas)
  }

array(NA, dim=c(258, 2))
vola(X, 257, 252)

