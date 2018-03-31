pv<- function(num, a, i, t, m=TRUE)
  switch(num, 
         satu = {
           tunggal = a/(1+i*t)
           print(tunggal)
         },
         dua = {
           nominal = a/((1+(i/m))^(m*t))
           print(nominal)
         },
         tiga = {
           kontinu = a/exp(i*t)
           print(kontinu)
         }
  )

