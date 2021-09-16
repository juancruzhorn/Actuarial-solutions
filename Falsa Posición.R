#Falsa posición

RegulaFalsi<-function(Fn,p0,p1,TOL,N){
  i=1
  q0=Fn(p0)
  q1=Fn(p1)
  while(i<=N){
    p=p1-(q1*(p1-p0))/(q1-q0)
    if(abs(p-p0)<TOL){
      return(paste("El valor p es = ",p,". Obtenido en la ",i," iteración"))
    }
    i=i+1
    q=Fn(p)
    if((q*q0)<0){
      p0=p1
      q0=q1
    }
    p1=p
    q1=q
  }
  return(paste ("El metodo fallo despues de", N, "iteraciones"))
}

# a) exp(x)+2^(-x)+2*cos(x)-6

#p0=1.7, p1=1.8
RegulaFalsi(FnNR1,1.7,1.8,10^-10,100)

#b) f(x) = ln(x-1)+cos(x-1) = 0

#p0=1.3, p1=1.4
RegulaFalsi(FnNR2,1.3,1.4,10^-10,100)

#c) f(x)=2xcos2x-(x-2)^2

#p0=2.6, p1=3
RegulaFalsi(FnNR3,2.6,3,10^-10,100)

#p0=3.7,p1=4
RegulaFalsi(FnNR3,3.7,4,10^-10,100)

#d) f(x) = (x-2)^2-lnx = 0

#p0=1.4, p1=1.45
RegulaFalsi(FnNR4,1.4,1.45,10^-10,100)

#p0=3,p1=3.1
RegulaFalsi(FnNR4,3,3.1,10^-10,100)

#e) f(x) = e^x-3x^2 = 0

#p0=0.9, p1=0.95
RegulaFalsi(FnNR5,0.9,0.95,10^-10,100)

#p0=3.5, p1=3.8
RegulaFalsi(FnNR5,3.5,3.8,10^-10,100)

#f(x )= sinx-e^-x = 0

#p0=0.55, p1=0.6
RegulaFalsi(FnNR6,0.55,0.6,10^-10,100)

#p0=3,p1=3.1
RegulaFalsi(FnNR6,3,3.1,10^-10,100)

#p0=6.2, p1=6.3
RegulaFalsi(FnNR6,6.2,6.3,10^-10,100)