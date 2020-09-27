library(Rmpfr)
library(Rcpp)
options(digits = 16) ##Sigras significativas máximas mostradas.

iteracion = function(g, x0,tol, maxIter, ...){
  
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

    xa = g(xa)
    xb = g(xa)
    xc = g(xb)
    if(k == 1)
    {
    cat("Sin Aitken: \nResultado 1: ", xa, "\n")
    cat("Resultado 2: ", xb, "\n")
    cat("Resultado 3: ", xc, "\n")
    cat("Aitken: \n")
    }
    
      #print(l)
      xf <- (xa*xc - xb^2) / (xc - 2*xb + xa)
      
      dx = abs(xi - xf)
      xi= xf
      
      #Imprimir estado
      cat("Resultado", k, "= ")
      print(xf) 
      k = k+1
      points(k, xf)
      lines(k , xf)
      
    xa = xf
    
    if(dx< tol|| k > maxIter) break;
  }
  if( dx > tol ){
    cat("Final del Programa de Comparación. ")
    
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

#Tolerancia
Tol <- 1e-8
#Cambiando estos intervalos hicimos las pruebas
IntervaloI<- 1
IntervaloR<- 1
x0 = x0f(IntervaloI, IntervaloR)
#Funciones Siguientes fueron las que probamos
f <- function(x) {cos(1/x)}
cat("Resultados de la Iteración de Aitken con tolerancia 1e-8: \n")
iteracion(f,X0, Tol, 3)

