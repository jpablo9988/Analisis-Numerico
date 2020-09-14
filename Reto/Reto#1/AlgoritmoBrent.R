library(Rmpfr) #Aplica la libreria Rmpfr para el manejo de mas cifras significativas
options(digits = 16)
f <- function(x) (x^3) - (2*x^2) + (x*4/3) - (8/27)
#Funcion que se le debe encontrar las raices

#Declaracion de intervalos [a,b]
a <- 0
b <- 1
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
