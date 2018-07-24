pilih <- function(num, k,i,n){
  v=1/(1+i)
  switch(num, 
         satu = {
           an = k*((1-v^n)/i)
           sn = k*((1+i)^(n-1)/i)
           cat("PV annuitas akhir =", an, "\n")
           cat("Nilai akumulasi annuitas akhir =", sn, "\n")
         },
         dua = {
           an = k*((1-v^n)/i*v)
           sn = k*(((1+i)^n-1/i*v))
           cat("PV annuitas awal =", an, "\n")
           cat("Nilai akumulasi annuitas awal =", sn, "\n")
           
         }
  )
}
pilih("satu",2000,0.08,4)
pilih("dua",2000,0.08,4)
