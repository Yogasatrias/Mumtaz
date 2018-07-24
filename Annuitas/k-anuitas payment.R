angsuran <- function(num,nilai,i,t,m=TRUE){
  n=t*m
  j=i/m
  v=1/(1+j)
  switch(num, 
         satu = {
           kan = nilai/((1-v^n)/j)
           ksn = nilai/(((1+j)^n-1)/j)
           cat("PV annuitas akhir =", kan, "\n")
           cat("Nilai akumulasi annuitas akhir =", ksn, "\n")
         },
         dua = {
           kan = nilai/((1-v^n)/(j*v))
           ksn = nilai/(((1+j)^n-1)/(j*v))
           cat("PV annuitas awal =", kan, "\n")
           cat("Nilai akumulasi annuitas awal =", ksn, "\n")
           
         }
  )
}
