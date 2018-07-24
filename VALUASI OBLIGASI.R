setwd("D:\\KULIAHH\\SEMESTER 4\\PSK R")
#P: Harga obligasi
#f: pokok hutang atau nilai nominal
#r: bunga obligasi (kupon %)
#i: yield(bunga investasi lain selain obligasi)
#m: dibayar sebanyak m kali dlm setahun
#ttm: jatuh tempo


#menghitung valuasi obligasi
valobligasi <- function (f, r, i, ttm, m){
  t=ceiling(ttm*m+1)
  b=ttm*m-floor(ttm*m)
  k=1-b
  ibin=i/m
  rbin=r/m
  vbin=1/(1+ibin)
  an=(1-vbin^t)/ibin
  Bt=f*rbin*an+f*vbin^t
  
  #THEORITICAL METHOD
  Bf1=Bt*(1+ibin)^k
  frk1=f*rbin*((1+ibin)^k-1)/ibin
  Bm1=Bf1-frk1
  cat("valuasi obligas untuk Theoritical Method: ",Bm1, "\n")
  
  #PRACTICAL METHOD
  Bf2=Bt*(1+k*ibin)
  frk2=k*f*rbin
  Bm2=Bf2-frk2
  cat("valuasi obligas untuk Practical Method: ",Bm2,"\n")
  
  #SEMI-THEORITICAL METHOD
  Bf3=Bt*(1+ibin)^k
  frk3=k*f*rbin
  Bm3=Bf3-frk3
  cat("valuasi obligas untuk Semi-Theoritical Method: ",Bm3, "\n")
}

#untuk mencari ttm jika yang diketahui tanggalnya
hariini=as.Date("8/6/18", "%d/%m/%y")
jatuhtempo=as.Date("15/8/23", "%d/%m/%y")
hari = jatuhtempo-hariini
hari
