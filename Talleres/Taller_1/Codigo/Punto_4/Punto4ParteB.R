library(Rmpfr)
library(Rcpp)
options(digits = 16) ##Sigras significativas máximas mostradas.

iteracion = function(g, x0,tol, maxIter, opt, ...){
  
  fun <- match.fun(g)
  g <- function(x) fun(x, ...)
  
  dx = 1
  k = 1
  O = 0 #Contador para el Método de Aitken
  # iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  xa = x0
  xi = x0
  l = vector("double", 100)
  plot(0:100, 0 : 100, type = "n", xlab = "Numero de iteraciones", ylab = "Valores X")  ##Set up the graph
  repeat{
    xa = mpfr(g(xa), 16)
    xb = mpfr(g(xa), 16)
    xc = mpfr(g(xb), 16)
    #xa = g(xa)
    #xb = g(xa)
    #xc = g(xb)
      #print(l)
    if(opt == 0) 
      {
      xf <- (xa*xc - xb^2) / (xc - 2*xb + xa)
      }
    if(opt == 1) 
      {
      xf = xa - (xb - xa)^2/(xc -2*xb + xa)
      }
      dx = abs(xi - xf)
      xi= xf
      
      #Imprimir estado
      cat("x_", k, "= ")
      print(xf) 
      cat("\n")
      k = k+1
      points(k, xf)
      lines(k , xf)
      
    xa = xf
    
    if(dx< tol|| k > maxIter) break;
  }
  if( dx > tol ){
    cat("Se llego al numero de iteraciones máximas ")
    
  } else{
    cat("x* es aproximadamente ")
    print(xa)
        
    cat(" con error menor que ", tol)
  }
  
}


x0f = function(a, b)
{
  x0 = (a + b) / 2
  return(x0)
}

#Error
Error1e8 <- 1e-8
Error1e16 <- 1e-16
#Cambiando estos intervalos hicimos las pruebas
IntervaloI<- 0
IntervaloR<- 4
x0 = x0f(IntervaloI, IntervaloR)
#Funciones Siguientes fueron las que probamos
f <- function(x) {x^2 - cos(x)}
cat("Resultados de la Iteración de Aitken con tolerancia 1e-8: \n")
iteracion(f,X0, Error1e8, 100, 0)
cat("Resultados de la Iteración de Steffensen con tolerancia 1e-8: \n")
iteracion(f,X0, Error1e8, 100, 1)
cat("Resultados de la Iteración de Aitken con tolerancia 1e-16: \n")
iteracion(f,X0, Error1e16, 100, 0)
cat("Resultados de la Iteración de Steffensen con tolerancia 1e-16: \n")
iteracion(f,X0, Error1e16, 100, 1)
