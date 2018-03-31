pilih <- function(num, k,i,t,m=TRUE){
  j=i/m
  n=t*m
  v=1/(1+j)
  switch(num, 
         satu = {
           an = k*((1-v^n)/j)
           sn = k*((1+j)^(n-1)/j)
           cat("PV annuitas akhir =", an, "\n")
           cat("Nilai akumulasi annuitas akhir =", sn, "\n")
         },
         dua = {
           an = k*((1-v^n)/j*v)
           sn = k*(((1+j)^n-1/j*v))
           cat("PV annuitas awal =", an, "\n")
           cat("Nilai akumulasi annuitas awal =", sn, "\n")
           
         }
  )
}
pilih("satu",2000,0.08,4)
pilih("dua",2000,0.08,4)
pilih("satu",2000,0.08,4,2)
pilih("dua",2000,0.08,4,2)

