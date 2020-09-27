horner<-function(co,x){
  n<-co
  i<-0
  for(k in co[2: length(co)]){
    n<-x*n+k
    i<-i+2
  }
  return(cat("Numero de operaciones: ", i, 
             "\nNumero de multiplicaciones: ", 1/2,
             "\nNumero de sumas: ",i/2 ))
}

x0<- -2
co<-c(2,0,-3,3,-4)
horner(co,x0)