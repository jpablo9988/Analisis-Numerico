
##Punto 2
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#ejercicio 1
sumaTriInf <- function(M){
  cont = 0
  sumaMatriz = 0
  n <- nrow(M)
  colu <- ncol(M)
  if(n ==  colu){
    i <- NULL
    j <- NULL
    for(i in 1:n){
      for(j in 1:i){
         sumaMatriz = sumaMatriz + M[i,j]
         cont = cont +1
      }
    }
    cat("La suma de la matriz triangular inferior es: ", sumaMatriz, "\n")
  }else{
    print("No es una matriz cuadratica")
  }
  return(sumaMatriz)
}
z <- c(5,10,15,20,25,30,35,40,45,50)
sumas <- NULL

for(l in z){
  matriz<-matrix(1,ncol=l,nrow = l)
  sumas <-c(sumas,sumaTriInf(matriz))
}
plot(z,sumas,xlab="Tamaño de la matriz",ylab="Num de Sumas")
lines(z,sumas, col = "red")
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#ejercicio2

sumaCuadrado <- function(x){
  suma = 0
  for(i in 1:x)
  {
    suma = suma + (i^2)
  }
  cat("La suma es: ", suma, "\n")
  return (suma)
}

z <- c(10,20,30,40,50,60,70,80,90,100)

sumasCuadrado <- c()
for(l in z)
{
  sumasCuadrado <- c(sumasCuadrado, sumaCuadrado(l))
}

plot(z,sumasCuadrado,xlab="Tamaño de la matriz",ylab="Num de Sumas")
lines(z,sumasCuadrado, col = "red")

#ejercicio resuelto con la formula
formSumaCuad <- function(x)
{
  cat("La suma es: ", x*(x+1)*(2*x+1)/6, "\n")
  return(x*(x+1)*(2*x+1)/6)
}
sumasCuadradoFor <- c()
for(l in z)
{
  sumasCuadradoFor <- c(sumasCuadradoFor, formSumaCuad(l))
}
plot(z,sumasCuadradoFor,xlab="Tamaño de la matriz",ylab="Num de Sumas")
lines(z,sumasCuadradoFor, col = "red")
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#ejercicio 3
#derivada = 0.00052*t^3+4.26*x

derivada = function(t) 4.26*t-0.0052*t^3
original = function(t) 6+2.13*t^2-0.0013*t^4
polyroot(c(0,4.26,0,-0.0052))
uniroot(derivada, c(10,30),tol=1e-9, maxiter = 100)

#Brent

brent <- function(f){
  a <- 28
  b <- 29
  #Declaracion de las variables
  machep <- 1e-8
  t <- 1e-8
  sa = a
  sb = b
  fa = f(sa)
  fb = f(sb)
  c = sa
  fc = fa
  e = sb - sa
  d = e
  s = fb/fa
  num = 0
  #Inicio de Iteraciones
  while(1)
  {
    if(abs(fc)<abs(fb))
    {
      sa = sb
      sb = c
      c = sa
      fa = fb
      fb = fc
      fc = fa
    }
    tol = 2.0 * machep * abs(s) + t
    m = 0.5 * (c - sb)
    if(abs(m) <= tol | fb ==0)
    {
      cat("M: ", abs(m), " Tol: ", tol, " fb: ", fb)
      break;
    }
    if(abs(e) < tol | abs(fa)<=abs(fb))
    {
      e = m
      d = e
    }
    else
    {
      s = fa/fb
      if(sa == c)
      {
        p = 2*m*s #interpolacion lineal
        q = 1-s
      }
      else
      {
        q = fa/fc #interpolacion cuadratica
        r = fb/fc
        p = s*(2*m*q*(q-r)-(sb - sa)*(r-1)) #inversa
        q = (q-1) * (r-1) * (s-1)
      }
      if(0.0<p)
      {
        q = -q
      }
      else
      {
        p = -p
      }
      s = e
      e=d
      if(2 * p < 3*m*q - abs(tol*q) & p < abs(0.5 * s * q))
      {
        d = p/q
      }
      else
      {
        e = m
        d = e
      }
    }
    sa = sb
    fa = fb
    if(tol < abs(d))
    {
      sb = sb + d
    }
    else if ( 0 < m )
    {
      sb = sb + tol
      
    }
    else
    {
      sb = sb - tol
    }
    fb = f(sb)
    if((0 < fb & 0 < fc) | (fb<= 0 & fc <= 0))
    {
      c = sa
      fc = fa
      e = sb - sa
      d = e
    }
    num  = num + 1
  }
  cat("El resultado es: ", sb, "Se obtuvo el resultado en la iteracion numero: ", num)
}
brent(derivada)

#Resultado de la altura maxima del cohete
6+2.13*(28.62220762)^2-0.0013*(28.62220762)^4