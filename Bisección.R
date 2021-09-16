#Método Bisección
options(digits=16)

Biseccion <- function(Fcn,a,b,TOL,N){
  #Paso 1 Asigno valor al contador
  i = 1
  FA <- Fcn(a)
  
  #Paso 2 Bucle para iteración en la resolución
  while (i <= N) {
    
    #Paso 3
    p <- a + (b-a)/2   #Indicación de Burden para mejorar error
    FP <- Fcn(p)      
    #Paso 4
    
    if (FP == 0 | (b-a)/2 < TOL ) {
      return(paste("El valor p es = ",p,". Obtenido en la ",i," iteración"))
    }
    #Paso 5
    
    i <- i + 1
    
    #Paso 6
    if (FA * FP > 0) {
      a <- p
      FA <- FP
    }
    else {
      b <- p
    }
  }
  #Paso 7
  return (paste ("El metodo fallo despues de", N, "iteraciones"))
  
}

#a) cos(x)-sqrt(x)=0
FnB1<- function(x){
  return (cos(x)-sqrt(x))
}

resultados<-seq(from=0.6,to=0.7,by=0.001)
plot(resultados,Funcion1(resultados),axes= TRUE)
abline(0,0,col="red") #a=0.64, b=0.66

Biseccion(FnB1,0.64,0.66,10^-20,1000)
#Verificación
Funcion1(0.6417143708728826)

#b) x^3+4x^2-10
FnB2<- function(x) {
  return(x^3+4*x^2-10)
}
resultados<-seq(from=1.2,to=1.4,by=0.005)
plot(resultados,FnB2(resultados),axes=TRUE)
abline(0,0,col="red") #a=1.35, b=1.4

Biseccion(FnB2,1.35,1.4,10^-10,100)
#Verificación
FnB2(1.36523001333699)

#c) f(x)=2+cos(e^x-2)-e^x

FnB3<-function(x){
  return(2+2*cos(exp(x)-2)-exp(x))
}
resultados<-seq(from=1,to=2,by=0.001)
plot(resultados,FnB3(resultados),axes=TRUE)
abline(0,0,col="red") #a=1, b=1.2

Biseccion(FnB3,1,1.2,10^-15,100)
FnB3(1.10851856882243)

#d) x^3-7x^2+14x-6

FnB4<-function(x) {
  return(x^3-7*x^2+14*x-6)
}

resultados<-seq(from=0,to=5,by=0.001)
plot(resultados,FnB4(resultados),axes=TRUE)
abline(0,0,col="red") #a=0, b=1 & a=3, b=3.3 & 

#0<=x<=1
Biseccion(FnB4,0,1,10^-15,100)
FnB4(0.585786437626905)

#3<=x<=3.3
Biseccion(FnB4,3,3.3,10^-15,100)
FnB4(3)

#3.4<=x<=3.5
Biseccion(FnB4,3.4,3.5,10^-15,100)
FnB4(3.41421356237309)
