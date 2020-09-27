binario<-function(num){
  exp<- -1
  sum<- 0
  while(num > 0){
    digit<-num %% 2
    num<-floor(num/2)
    sum<-sum+digit*exp
    exp<-exp*10
  }
  return(sum)
}

decimal<-function(num){
  x=1
  y=0
  
  while(num>0){
    z=num %% 2
    num=(num/10)
    num<-as.integer(num)
    y=y+(z*x)
    x=x*2
  }
  return(y)
}


#a.Encuentre los primeros 15 bits en la representacion 
#  binaria de ??
p=as.integer(pi)
n=((pi*(10^4))-(p*(10^4)))
n<-as.integer(n)
a<-binario(p)
b<-binario(n)
cat("Pimeros 15 bits en binario de pi: ", a, ".",b)

# b.Convertir los siguientes n´umeros binarios a base 10: 1010101;
#   1011.101; 10111.010101...; 111.1111...
val<-decimal(10)
cat("Numero decimal de 10: ", val)

val<-decimal(1010101)
cat("Numero decimal de 1010101: ", val)

val<-decimal(1011)
valor<-decimal(101)
cat("Numero decimal de 1011.101: ", val, ".", valor)

val<-decimal(10111)
valor<-decimal(010101)
cat("Numero decimal de 10111.010101: ", val, ".", valor)

val<-decimal(111)
valor<-decimal(1111)
cat("Numero decimal de 111.1111: ", val, ".", valor)

#c.Convierta los siguientes n´umeros de 
#   base 10 a binaria: 11.25;2/3; 30.6; 99.9
n1<-as.integer(11)
n2<-as.integer(25)
x<-binario(n1)
y<-binario(n2)
cat("11.25 es expresado en binario como: ", x, ".", y)

n<-as.integer((2/3)*100)
x<-binario(n)
cat("2/3 es expresado en binario como: ", x)

n1<-as.integer(30)
n2<-as.integer(6)
x<-binario(n1)
y<-binario(n2)
cat("30.6 es expresado en binario como: ", x, ".", y)

n1<-as.integer(99)
n2<-as.integer(9)
x<-binario(n1)
y<-binario(n2)
cat("99.9 es expresado en binario como: ", x, ".", y)