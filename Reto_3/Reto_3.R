#Código del Reto #3
#Realizado por...Juan Pablo Amorocho, Nicolas Pérez Fonseca, Jose Alejandro Lopez Garcia y
#Diego Fernando Trujillo Alvarez
#Ingreso de Datos:
l = 0.03200
n = 1.82466
b = 1.61305
uc =10.44186
ux = 0.30010
h = 0.5
k = 1
#Funciones: 
Mean <-function(l,n,uc,ux,h){
  temp <- (l/n)*uc*ux*h
  return(temp) 
}

Variance <-function(l,n,b,uc,ux,h){
  temp <- ((2*l)/(n^3))*((n*h)-1+exp(-n*h))*(2*uc*(ux^2)+(((b^2)*(ux^2)*uc*(uc-1))/((b^2)-(n^2))))-((2*l)*((b*h)-1+exp(-b*h))*(((ux^2)*uc*(uc-1))/((b^2)*((b^2)-(n^2)))))
  return(temp) 
}

Covarianza <- function(l,n,b,uc,ux,h,k){
  temp <- ((2*l)/(n^3))*((1+exp((-n)*h))^2)*(exp((-n)*(k-1)*h))*((uc*(ux^2))+((((b^2)*(ux^2)*uc*(uc-1))/(2*((b^2)-(n^2))))))-(2*l)*((1-exp(-1*b*h))^2)*exp((-b)*(k-1)*h)*((uc*(ux^2) * (uc - 1))/((2*b^2)* ((b^2)-(n^2))))
  return(temp)
}
#Valores Iniciales: 
esperanza=Mean(l,n,uc,ux,h)
esperanza

varianza=Variance(l,n,b,uc,ux,h)
varianza

covarianza=Covarianza(l,n,b,uc,ux,h,k)
covarianza

varianza=Variance(l,n,b,uc,ux,h)
varianza

# vanilla RK4 escrito en R (09Dec2011) v1.1m, Modificado el (24Noviembre2020)
#	Código Runge-Kutta modificado para Sistemas de Ecuaciones.
# Conceptos obtenidos de:
#  http://en.wikipedia.org/wiki/Runge-Kutta_methods
#  http://www.cms.livjm.ac.uk/etchells/notes/ma200/2ndodes.pdf
#
#	El código fue modificado para cumplir con las especificaciones de este proyecto.
# Es funcional para cinco variables incognitas en un sistema de ecuación.


rk4 <- function(f1,f2, f3, h, linit, nilit, binit, ucinit, uxinit, kinit, limit) {
  l  <- linit  #0.03200
  n  <- nilit  #1.82466
  b  <- binit  #1.61305
  uc <- ucinit #10.44186
  ux <- uxinit #0.30010
  k  <- kinit  #1
  
  lplot  <- c()
  nplot  <- c()
  bplot  <- c()
  ucplot <- c()
  uxplot <- c()
  k<-c()
  j<-c()
  p<-c()
  s<-c()
  q<-c()
  
  counter = 1
  while (limit > 0) {
    
    
    k[1] <- h*f1(l,n,uc, ux,h)
    j[1] <- h*f2(l,n,b,uc, ux,h)
    p[1] <- h*f3(l,n,b,uc,ux,h,1)
    s[1] <- h*f3(l,n,b,uc,ux,h,2)
    q[1] <- h*f3(l,n,b,uc,ux,h,3)
    k[1]
    k[2] <- h*f1(l+ 0.5*k[1],n + 0.5*j[1],uc + 0.5*s[1], ux + 0.5*q[1],h)
    j[2] <- h*f2(l + 0.5*k[1],n + 0.5*j[1],b + 0.5*p[1],uc + 0.5*s[1],ux + 0.5*q[1],h)
    p[2] <- h*f3(l + 0.5*k[1],n + 0.5*j[1],b + 0.5*p[1],uc + 0.5*s[1],ux + 0.5*q[1],h,1)
    s[2] <- h*f3(l + 0.5*k[1],n + 0.5*j[1],b + 0.5*p[1],uc + 0.5*s[1],ux + 0.5*q[1],h,2)
    q[2] <- h*f3(l + 0.5*k[1],n + 0.5*j[1],b + 0.5*p[1],uc + 0.5*s[1],ux + 0.5*q[1],h,3)
    
    k[3] <- h*f1(l + 0.5*k[2],n + 0.5*j[2],uc + 0.5*s[2], ux + 0.5*q[2],h)
    j[3] <- h*f2(l + 0.5*k[2],n + 0.5*j[2],b + 0.5*p[2],uc + 0.5*s[2],ux + 0.5*q[2],h)
    p[3] <- h*f3(l + 0.5*k[2],n + 0.5*j[2],b + 0.5*p[2],uc + 0.5*s[2],ux + 0.5*q[2],h,1)
    s[3] <- h*f3(l + 0.5*k[2],n + 0.5*j[2],b + 0.5*p[2],uc + 0.5*s[2],ux + 0.5*q[2],h,2)
    q[3] <- h*f3(l + 0.5*k[2],n + 0.5*j[2],b + 0.5*p[2],uc + 0.5*s[2],ux + 0.5*q[2],h,3)
    
    k[4] <- h*f1(l + k[3],n + j[3],uc + s[3], ux + q[3] ,h)
    j[4] <- h*f2(l + k[3],n + j[3],b + p[3],uc + s[3],ux + q[3] ,h)
    p[4] <- h*f3(l + k[3],n + j[3],b + p[3],uc + s[3],ux + q[3] ,h,1)
    s[4] <- h*f3(l + k[3],n + j[3],b + p[3],uc + s[3],ux + q[3] ,h,2)
    q[4] <- h*f3(l + k[3],n + j[3],b + p[3],uc + s[3],ux + q[3] ,h,3)
    
    l = l + (1./6)*(k[1] + 2*k[2] + 2*k[3] + k[4])
    n = n + (1./6)*(j[1] + 2*j[2] + 2*j[3] + j[4])
    b = b + (1./6)*(p[1] + 2*p[2] + 2*p[3] + p[4])
    uc = uc + (1./6)*(s[1] + 2*s[2] + 2*s[3] + s[4])
    ux = ux + (1./6)*(q[1] + 2*q[2] + 2*q[3] + q[4])
    
    limit <- limit - 1
    
    lplot [counter] = l
    bplot [counter] = b
    nplot [counter] = n
    ucplot[counter] = uc
    uxplot[counter] = ux
    counter = counter + 1
  }
  plot(bplot)
  plot(lplot)
  cat("Esperanza con resultados en Runge-Kutta: ", f1(lplot[4],nplot[4],ucplot[4],uxplot[4],h), "\n")
  cat("Varianza con resultados en Runge-Kutta: ", f2(lplot[4],nplot[4],bplot[4],ucplot[4],uxplot[4],h),"\n")
  cat("Auto-Lag-1 con resultados en Runge-Kutta: ", f3(lplot[4],nplot[4],bplot[4],ucplot[4],uxplot[4],h,1),"\n")
  cat("Auto-Lag-2 con resultados en Runge-Kutta: ", f3(lplot[4],nplot[4],bplot[4],ucplot[4],uxplot[4],h,2),"\n") 
  cat("Auto-Lag-3 con resultados en Runge-Kutta: ", f3(lplot[4],nplot[4],bplot[4],ucplot[4],uxplot[4],h,3),"\n") 
  return (list(v1 = lplot, v2 = nplot, v3 = bplot, v4 = ucplot, v5 = uxplot))
  
}

switcheroo <- function(newv, v4, v3, v2)
{
  vnew = c()
  vnew[1] = v2
  vnew[2] = v3
  vnew[3] = v4
  vnew[4] = newv
  return (vnew)
}
#Código Adam Bashford WIP.

adambashfordVector <- function(f1, f2, f3, h, v1, v2, v3, v4, v5, k, tf)
{
  #En bashford, recibimos vectores de tamaño 4 para cada variable.
  #Orden debe de ser, Posición 4 es el mas reciente o V0, Posición 1 el mas antiguo o V-3
  lplot <- c()
  nplot  <- c()
  bplot  <- c()
  ucplot <- c()
  uxplot <- c()
  counter = 1
  while(tf >= counter)
  {
    y4 <- c(v1[4], v2[4], v3[4], v4[4], v5[4])
    fy4 <- c(f1(v1[4], v2[4], v4[4], v5[4], h), f2(v1[4], v2[4], v3[4], v4[4], v5[4], h), f3(v1[4], v2[4], v3[4], v4[4], v5[4], h, 1), f3(v1[4], v2[4], v3[4], v4[4], v5[4], h, 2), f3(v1[4], v2[4], v3[4], v4[4], v5[4], h, 3))
    fy3 <- c(f1(v1[3], v2[3], v4[3], v5[3], h), f2(v1[3], v2[3], v3[3], v4[3], v5[3], h), f3(v1[3], v2[3], v3[3], v4[3], v5[3], h, 1), f3(v1[3], v2[3], v3[3], v4[3], v5[3], h, 2), f3(v1[3], v2[3], v3[3], v4[3], v5[3], h, 3))
    fy2 <- c(f1(v1[2], v2[2], v4[2], v5[2], h), f2(v1[2], v2[2], v3[2], v4[2], v5[2], h), f3(v1[2], v2[2], v3[2], v4[2], v5[2], h, 1), f3(v1[2], v2[2], v3[2], v4[2], v5[2], h, 2), f3(v1[2], v2[2], v3[2], v4[2], v5[2], h, 3))
    fy1 <- c(f1(v1[1], v2[1], v4[1], v5[1], h), f2(v1[1], v2[1], v3[1], v4[1], v5[1], h), f3(v1[1], v2[1], v3[1], v4[1], v5[1], h, 1), f3(v1[1], v2[1], v3[1], v4[1], v5[1], h, 2), f3(v1[1], v2[1], v3[1], v4[1], v5[1], h, 3))
    
    y5 <- y4 + (h/24) * (55 * fy4 - 59 * fy3 + 37 * fy2 - 9 * fy1)
    fy5 <- c(f1(y5[1],y5[2],y5[4],y5[5], h), f2(y5[1],y5[2], y5[3],y5[4],y5[5], h),f3(y5[1],y5[2], y5[3],y5[4],y5[5], h, 1),f3(y5[1],y5[2], y5[3],y5[4],y5[5], h, 2),f3(y5[1],y5[2], y5[3],y5[4],y5[5], h, 3))
    y5M <- y4 + (h/24) * (9 * fy5 + 19 * fy4 - 5 * fy3 + fy2)

    v1 <- switcheroo(y5M[1], v1[4], v1[3], v1[2])
    v2 <- switcheroo(y5M[2], v2[4], v2[3], v2[2])
    v3 <- switcheroo(y5M[3], v3[4], v3[3], v3[2])
    v4 <- switcheroo(y5M[4], v4[4], v4[3], v4[2])
    v4 <- switcheroo(y5M[5], v5[4], v5[3], v5[2])
    
  
    
    lplot[counter] <- y5M[1]
    nplot[counter] <- y5M[2]
    bplot[counter] <- y5M[3]
    ucplot[counter] <- y5M[4]
    uxplot[counter] <- y5M[5]

    counter = counter + 1
  }
  
  cat("Esperanza con resultados en Bashford: ", f1(lplot[1],nplot[1],ucplot[1],uxplot[1],h), "\n")
  cat("Varianza con resultados en Bashford: ", f2(lplot[1],nplot[1],bplot[1],ucplot[1],uxplot[1],h),"\n")
  cat("Auto-Lag-1 con resultados en Bashford: ", f3(lplot[1],nplot[1],bplot[1],ucplot[1],uxplot[1],h,1),"\n")
  cat("Auto-Lag-2 con resultados en Bashford: ", f3(lplot[1],nplot[1],bplot[1],ucplot[1],uxplot[1],h,2),"\n") 
  cat("Auto-Lag-3 con resultados en Bashford: ", f3(lplot[1],nplot[1],bplot[1],ucplot[1],uxplot[1],h,3),"\n")

}


#Primer Método de Resolución de Sistemas de Ecuaciones.
#Se usa Runge-Kutta Orden 4 como primer método.
lista = rk4(Mean,Variance,Covarianza,l,n,b,uc,ux,h,k,4)
lvec = lista[[1]]
nvec = lista[[2]]
bvec = lista[[3]]
ucvec = lista[[4]]
uxvec = lista[[5]]
#Adam Bashford? 
adambashfordVector(Mean, Variance, Covarianza,h, lvec, nvec, bvec, ucvec, uxvec,k,1)
