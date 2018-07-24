bunga <- function(num, a, k, t, m=TRUE)
  switch(num, 
         satu = {
           tunggal = ((a/k)-1)/t
           print(tunggal)
         },
         dua = {
           nominal = (((a/k)^(1/(m*t))-1))*m
           print(nominal)
         },
         tiga = {
           kontinu = (log(a/k))/t
           print(kontinu)
         }
  )

