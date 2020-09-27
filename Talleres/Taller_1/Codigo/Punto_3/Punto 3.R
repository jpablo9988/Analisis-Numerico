##Punto 3
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#1.
fx<-function(x){
  log(x+2)
}
gx<-function(x){
  sin(x)
}
#a)
e1=1e-16
e2=1e-8
X<-function(x,e){
  respuestas<-c()
  Fanterior=fx(x)
  Factual=fx(Fanterior)
  Fsiguiente=0
  Faux=0
  Ganterior=gx(x)
  Gactual=gx(Ganterior)
  Gsiguiente=0
  Gaux=0
  iteraciones=0
  repeat
  {
    Fsiguiente=Factual-((fx(Factual)*(Factual-Fanterior))/(fx(Factual)-fx(Fanterior)))
    Gsiguiente=Gactual-((gx(Gactual)*(Gactual-Ganterior))/(gx(Gactual)-gx(Ganterior)))
    iteraciones = iteraciones+1
    if(abs(abs(Fsiguiente)-abs(Gsiguiente))<=e|is.nan(abs(abs(Fsiguiente)-abs(Gsiguiente))))
    {
      break
    }
    Faux=Factual
    Factual=Fsiguiente
    Fanterior=Faux
    Gaux=Gactual
    Gactual=Gsiguiente
    Ganterior=Gaux
    respuestas[iteraciones]<-Fsiguiente
    print(iteraciones)
    print(Gsiguiente)
    print(Fsiguiente)
  }
  print(iteraciones)
  print(Gsiguiente)
  print(Fsiguiente)
  plot(respuestas,type='o',main="Convergencia",xlab="Iteraciones",ylab="Respuestas")
}

#b)
Y<-function(x,e){
  respuestas<-c()
  Fanterior=fx(x)
  Factual=fx(Fanterior)
  Fsiguiente=0
  Faux=0
  Ganterior=gx(x)
  Gactual=gx(Ganterior)
  Gsiguiente=0
  Gaux=0
  iteraciones=0
  repeat{
    Fsiguiente=Factual-fx(Factual)*((Factual-Fanterior)/(fx(Factual)-fx(Fanterior)))
    Gsiguiente=Gactual-gx(Gactual)*((Gactual-Ganterior)/(gx(Gactual)-gx(Ganterior)))
    iteraciones = iteraciones+1
    
    if(abs(abs(Fsiguiente)-abs(Gsiguiente))<=e|is.nan(abs(abs(Fsiguiente)-abs(Gsiguiente))))
    {
      break
    }
    Faux=Factual
    Factual=Fsiguiente
    Fanterior=Faux
    Gaux=Gactual
    Gactual=Gsiguiente
    Ganterior=Gaux
    respuestas[iteraciones]<-Fsiguiente
    print(iteraciones)
    print(Gsiguiente)
    print(Fsiguiente)
  }
  print(iteraciones)
  print(Gsiguiente)
  print(Fsiguiente)
  plot(respuestas,type='o',main="Convergencia",xlab="Iteraciones",ylab="Respuestas")
}

X(900000000000000000000000000,e1)
Y(900000000000000000000000000,e2)

#Newton
library(Matrix)

ec1=matrix(c())

ecuaciones <- function(ec1,ec2,x0,tol)
{
  x=x0
  error=1
  n=0
  while(error>tol)
  {
    dx=inv(ec1)/ec2
    error=norm(ec1)/norm(ec2)
    x=x+dx
    n+1
  }
  print(n)
}
#2.

f1 <- function(x) exp(x) + x -1
df1 <- function(x) exp(x) + 1
df2 <- function(x) exp(x)

newton <- function(x) {
  respuestas<-c()
  i=0;
  xa=0
  repeat{
    i=i+1
    x<-x-f1(x)/df1(x)
    if (f1(x) == 0) break
    error<-abs((x-xa)/x)
    if(error<1e-30) break
    respuestas[i]<-x
    xa=x
  }
  plot(respuestas, main = "Convergencia", type='o',xlab = "Iteraciones")
  print(x)
}
newton(1)

#3.
newtonGeneralizado <- function(x) {
  respuestas<-c()
  xa=0
  i=0;
  repeat{
    i=i+1
    x<-(f1(x)*df1(x))/((df1(x)^2)-(f1(x)*df2(x)))
    if (f1(x) == 0) break
    error<-abs((x-xa)/x)
    if(error<1e-1|i>10) break
    respuestas[i]<-x
    xa=x
  }
  plot(respuestas, main = "Convergencia", type='o',xlab = "Iteraciones")
  print(x)
}
newtonGeneralizado(1)