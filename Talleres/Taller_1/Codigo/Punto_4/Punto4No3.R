library(Rmpfr)
library(Rcpp)
options(digits = 16) ##Sigras significativas máximas mostradas.

iteracion = function(g, t, x0,tol, maxIter, opt, ...){
  #FUNCION 1
  fun <- match.fun(g)
  g <- function(x) fun(x, ...)
  #FUNCION 2
  fun <- match.fun(t)
  t <- function(x) fun(x, ...)
  
  dx = 1
  k = 1
  # iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  xa = x0
  xd = x0
  xi = x0
  graphg = vector("double", 10)
  grahpl = vector("double", 10)
  FLAGA = TRUE
  FLAGB = TRUE
  repeat{

    
    if(FLAGA == TRUE)
    {
    #Steffanson
    if(opt==1)
    {
    xb = mpfr(g(xa), 16)
    xc = mpfr(g(xb), 16)
    xfa = xa - (xb - xa)^2/(xc -2*xb + xa)
    }else{ ##Aitken
      xb = g(xa)
      xc = g(xb)
      xfa <- (xa*xc - xb^2) / (xc - 2*xb + xa)
    }
    
    dxa = abs(xa - xfa)
    if(dxa < tol) FLAGA = FALSE
    xa = xfa
    }
    
    if(FLAGB == TRUE)
    {
    #Con xf, hacen parte de la segunda funcion
      if(opt==1)
      {
    xe = mpfr(t(xd), 16)
    xf = mpfr(t(xe), 16)
    xfb = xd - (xe - xd)^2/(xf -2*xe + xd)
      }
      else{ ##Aitken
        xe = g(xd)
        xf = g(xe)
        xfb <- (xd*xf - xe^2) / (xf - 2*xe + xd)
      }
    dxb = abs(xd - xfb)
    if(dxb < tol) FLAGB = FALSE
    xd = xfb
    }

    
      
      #Imprimir estado
      cat("Valores de la funcion f(t): t_", k, "= ")
      print(xfa) 
      cat("Valores de la funcion g(t): t_", k, "= ")
      print(xfb) 
      k = k+1

    if((FLAGA == FALSE && FLAGB == FALSE)|| k > maxIter) break;
  }
  if( dx > tol ){
    cat("Final del Programa ")
    
  } else{
    cat("x* es aproximadamente ")
    print(xfa)
    cat("para f(t) y ")
    print(xfb)
    cat("para g(t).")
        
    cat(" con error menor que ", tol)
  }
  
}


x0f = function(a, b)
{
  x0 = (a + b) / 2
  return(x0)
}

#Tolerancia
Tol <- 1e-16
#Cambiando estos intervalos hicimos las pruebas
IntervaloI<- 3
IntervaloR<- 3
x0 = x0f(IntervaloI, IntervaloR)
#Funciones Siguientes fueron las que probamos
f <- function(x) {3 * (sin(x))^3 - 1}
h <- function(x) {4 * sin(x) * cos(x)}
cat("Resultados de la Iteración de Aitken con tolerancia 1e-8: \n")
iteracion(f, h ,X0, Tol, 100, 1)

