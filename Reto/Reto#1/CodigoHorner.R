#IMPORTANTE CORRER ESTE PROGRAMA USANDO EL SOURCE, YA QUE USA READLINES()
library(Rmpfr) #Aplica la libreria Rmpfr para el manejo de mas cifras significativas
options(digits = 16)
metododehorner = function(a , x0){
  
  sizeofa <- length(a) 
  y = a[[sizeofa]]
  z = a[[sizeofa]]
  w = a[[sizeofa]]
  j = sizeofa
  if(sizeofa < 3)
  {
    w = 0
    if(sizeofa < 2)
    {
      z = 0
      if(sizeofa < 1)
      {
        y = 0
      }
    }
  }

if( j > 1)
{
  repeat
  {
    j = j - 1
    y = x0 * y + a[[j]]
    if(j >= 2) z = x0*z + y
    if(j >= 3) w= x0*w + z
    
    if(j == 1) break;
  }
}
  
  cat ("P(Xo) = ", y, " Q(Xo) = ", z ,"(Derivada) R(Xo) = ", w, " (2nda Derivada)")
}
#Esta n describe el grado del polinomio de la función a ingresar
{polinomio <- readline(prompt = "Ingrese el grado del polinomio de la funcion: ")}
n = as.integer(polinomio)
n = n + 1
#Este vector guarda las constantes del polinomio organizados 
#de forma a0 + a1 ... an, en este orden
a = vector("complex", n)
i = 1
repeat{
  const <- readline (prompt = "Ingrese el valor de a teniendo en cuenta el orden de a0 + a1x^1 + ... anx^n: ")
  a[[i]] = as.complex(const)
  if(i == n) break;
  i = i + 1
}
#Xo es el valor inicial dado
val0<- readline(prompt = "Ingrese el valor inicial x0: ")
Xo = as.complex(val0)
metododehorner(a, Xo)

