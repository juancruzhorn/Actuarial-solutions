#Método de Newton

options(digits=16)

#Algoritmo
Newton<-function(Fn,dFn,p0,TOL,N){
  i=1
  for(i in 1:N){
    p<-p0-Fn(p0)/dFn(p0)
    if(abs(p-p0)<TOL){
      return(paste("El valor p es = ",p,". Obtenido en la ",i," iteración"))
    }
    i=i+1
    p0=p
  }
  return(paste ("El metodo fallo despues de", N, "iteraciones"))
}

# a) exp(x)+2^(-x)+2*cos(x)-6

FnNR1<-function(x){
  return(exp(x)+2^(-x)+2*cos(x)-6)
}

DFnNR1<-function(x){
  return(exp(x)-(log(2)*2^(-x))-2*sin(x))
}

x<-seq(from=1,to=2,by=0.01)
plot(x,FnNR1(x),axes=TRUE)
abline(0,0, col="red")

#p0=1.8
Newton(FnNR1,DFnNR1,1.8,10^-8,100)

#b) ln(x-1)+cos(x-1)

FnNR2<-function(x){
  return(log(x-1)+cos(x-1))
}
DFnNR2<-function(x){
  return(1/(x-1)-sin(x-1))
}

x<-seq(from=1.3,to=2,by=0.01)
plot(x,FnNR2(x),axes=TRUE)
abline(0,0, col="blue")

#p0=1.4
Newton(FnNR2,DFnNR2,1.5,10^-8,100)

#c) 2xcos2x-(x-2)^2

FnNR3<-function(x){
  return((2*x*cos(2*x))-((x-2)^2))
}
DFnNR3<-function(x){
  return((2*x*sin(2*x)-cos(2*x)+x-2)*-2)
}

#2<=x<=3
x<-seq(from=2,to=3,by=0.001)
plot(x,FnNR3(x),axes=TRUE)
abline(0,0, col="blue")

#p0=2.6
Newton(FnNR3,DFnNR3,2.6,10^-10,100)
#Verificación
FnNR3(2.37068691766226)

#3<=x<=4
x<-seq(from=3,to=4,by=0.001)
plot(x,FnNR3(x),axes=TRUE)
abline(0,0, col="blue")

#p0=3.7
Newton(FnNR3,DFnNR3,3.7,10^-10,100)
#Verificación
FnNR3(3.72211277310179)

#d)(x-2)^2-lnx

FnNR4<-function(x){
  return(((x-2)^2)-log(x))
}

DFnNR4<-function(x){
  return(2*(x-2)-1/x)
}

#1<=x<=2
x<-seq(from=1,to=2,by=0.001)
plot(x,FnNR4(x),axes=TRUE)
abline(0,0, col="blue")

#p0=1.4
Newton(FnNR4,DFnNR4,1.4,10^-10,100)
FnNR4(1.41239117202388)

#e<=x<=4
x<-seq(from=exp(1),to=4,by=0.001)
plot(x,FnNR4(x),axes=TRUE)
abline(0,0, col="blue")

#p0=3
Newton(FnNR4,DFnNR4,3,10^-10,100)

#e) e^x-3x^2

FnNR5<-function(x){
  return(exp(x)-3*x^2)
}

DFnNR5<-function(x){
  return(exp(x)-6*x)
}

#0<=x<=1
x<-seq(from=0,to=1,by=0.001)
plot(x,FnNR5(x),axes=TRUE)
abline(0,0, col="blue")

#p0=0.9
Newton(FnNR5,DFnNR5,0.9,10^-10,100)
FnNR5(0.910007572488709)

#3<=x<=5
x<-seq(from=3,to=5,by=0.001)
plot(x,FnNR5(x),axes=TRUE)
abline(0,0, col="blue")

#p0=3.5
Newton(FnNR5,DFnNR5,3.5,10^-10,100)
FnNR5(3.73307902863281)

#f(x)=sinx-e^-x
FnNR6<-function(x){
  return(sin(x)-exp(-x))
}

DFnNR6<-function(x){
  return(cos(x)+exp(-x))
}

#0<=x<=1

x<-seq(from=0,to=1,by=0.001)
plot(x,FnNR6(x),axes=TRUE)
abline(0,0, col="blue") #p0=0.55

Newton(FnNR6,DFnNR6,0.55,10^-10,100)
FnNR6(0.588532743981861 )

#3<=x<=4
x<-seq(from=3,to=4,by=0.001)
plot(x,FnNR6(x),axes=TRUE)
abline(0,0, col="blue") #p0=3

Newton(FnNR6,DFnNR6,3,10^-10,100)
FnNR6(3.09636393241065)

#6<=x<=7
x<-seq(from=6,to=7,by=0.001)
plot(x,FnNR6(x),axes=TRUE)
abline(0,0, col="blue") #p0=6.2

Newton(FnNR6,DFnNR6,6.2,10^-10,100)
FnNR6(6.28504927338259)


