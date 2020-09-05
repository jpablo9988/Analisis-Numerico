library(Rmpfr)
options(digits = 12) ##Sigras significativas máximas mostradas.
CTolerancia <- 1 #Para la restricciones de intervalos...
RestriccionesAplican <-0  #Este valor se puede modificar si se quiere que las
#Restricciones apliquen. Colocar en 0 por usar el metodo de Steffensen
metodoaitkensteffensen = function (l)
{
  v = l[[1]] - (((l[[2]] - l[[1]])^2)/ (l[[1]] - (2*l[[2]]) + l[[3]]))
  return (v)
}
iteracionpuntofijo = function(g, x0,tol, maxIter){
  dx = 1
  k = 1
  O = 0 #Contador para el Método de Aitken
  l = vector ( "double", 3)
  # iteración hasta que abs(x1 - x0) <= tol o se alcance maxIteraciones
  repeat{
    x1 = g(x0)
    if(k == 1)
    {
      x3 = x1
    }
    O = O+1
    l[[O]]<-x1 
    x0 = x1
    if(O == 3)
    {
      #print(l)
      x2 = metodoaitkensteffensen(l)
      l[(-1:-3)]
      #CORRE AITKEN
      O = 0;
      dx = abs(x2 - x3)
      x3= x2
      x0 = x2
      
      #Imprimir estado
      cat("x_", k, "= ", x1, "\n")
      k = k+1
    }

    if(dx< tol|| k > maxIter) break;
  }
  if( dx > tol ){
    cat("No hubo convergencia ")
    
  } else{
    cat("x* es aproximadamente ", x1, " con error menor que ", tol)
  }
  
}
evaluacionintervalos = function (iIzp, iDer, g)
{
  flagL = 1
  flagR = 1
  flagC = 1
  val1 = g (iIzp)
  cat("Comprobación Intervalo Izquierdo (Tolerancia de 0,5): ", val1, "\n")
  val2 = g (iDer)
  cat("Comprobación Intervalo Derecho (Tolerancia de 0,5): ", val2,"\n")
  resultadoL <- val1 - iIzp
  resultadoR <- val2 - iDer
  C1 = (iIzp + iDer) / 2
  val3 = g (C1)
  cat("Comprobación Intervalo Medio (Tolerancia de 0,5): ", val3,"\n")
  resultadoC = val3 - C1
  if(abs(resultadoL) < CTolerancia)
  {
    flagL = 0
  }
  if(abs(resultadoR) < CTolerancia)
  {
    flagR = 0
  }
  if(abs(resultadoC)< CTolerancia)
  {
    flagC = 0
  }
  #Como la comprobación se refiere a un "parecido", tendremos una tolerancia
  #Para revisar si es aproximado al valor -> TOL = 0.5
  if((val1 >iIzp && val1 < iDer) || flagL == 0)
  {
    
    if(((val2 >iIzp) && (val2 < iDer)) || flagR == 0)
    {
      if(((val3 >iIzp) && (val3 < iDer)) || flagR == 0)
      {
        return (1)
      }
      else
      {
        return(0)
      }
      
    }
    else
    {
      return(0)
    }
  }
  else
  {
    return (0)
  }

}
puntofijo = function(iIzq, iDer, g, ValorX, Tol, It)
{
  if(RestriccionesAplican == 0)
  {
    gx <- function (x) {eval( g, envir=list(x))}
    iteracionpuntofijo(gx, ValorX, Tol, It)
  }
  else
  {
  fder<- D(g , 'x')
  x<-iIzq
  izq = eval(fder)
  cat("Derivada con Cota Izquierda: ",izq, "\n")
  x<-iDer
  der = eval(fder)
  cat("Derivada con Cota Derecha: ", der, "\n")
  gx <- function (x) {eval( g, envir=list(x))}
  if(evaluacionintervalos(iIzq, iDer, gx) == 1)
  {
    if(izq < 1 && izq > -1)
    {
      
      if(der < 1 && der > -1)
      {
        iteracionpuntofijo(gx, ValorX, Tol, It)
      }
      else
      {
        cat("La función ingresada con los parametros no es valida")
      }
    }
    else
    {
      cat("La función ingresada con los parametros no es valida")
    }
  }
  else
  {
    cat("La función ingresada con los parametros no es valida")
  }
  }

}
#X inicial para hacer el método
X0 = 10
#Error
Error <- 1e-8
#Cambiando estos intervalos hicimos las pruebas
IntervaloI<- 0
IntervaloR<- 10
#Funciones Siguientes fueron las que probamos
f <- expression (cos(2*x)) 
f.1<- expression (cos(2*x)^2 - x^2 + x)
f2 <- expression (1 / (sin(x)))
f2.1 <- expression (asin(1/x))
f3 <- expression(sqrt(2*x^2 - (4/3)*x + (8/27)))
f3.1 <- expression ((-3/7)* x^3 + (6/7)*x^2 + (8/63))
puntofijo(IntervaloI, IntervaloR, f3.1,X0, Error, 100) #Acá se llama la funcion, el ultimo numero dice las iteraciones maximas

