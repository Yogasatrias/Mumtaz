setwd("D:\\KULIAH SEMT 4\\PSK\\PSK R\\UAS\\Obligasi")
#P: Harga obligasi
#f: pokok hutang atau nilai nominal
#r: bunga obligasi (kupon %)
#i: yield(bunga investasi lain selain obligasi)
#t: jangka waktu (tahun)
#m: dibayar sebanyak m kali dlm setahun

obligasi<-function(f, r, i, t, m){
  n = t*m
  rbin = r/m
  ibin = i/m
  vbin = 1/(1+ibin)
  an = (1-(vbin^n))/ibin
  P = (f*rbin*an)+(f*(vbin^n))
  print(P)
}