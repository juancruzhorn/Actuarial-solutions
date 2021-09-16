rm(list=ls())

#NaturalCubicSpline

NaturalCubicSpline<-function(x,y){
  n = length(y)
  j = n - 1
  
  a = y
  b = c(rep(NA,n))
  c = c(rep(NA,n))
  d = c(rep(NA,n))
  
  A = c(rep(NA,n))
  h = c(rep(NA,n))
  l = c(rep(NA,n))
  u = c(rep(NA,n))
  z = c(rep(NA,n))
  
  #Paso 1
  for (i in 1:j) { 
    h[i] = x[i + 1] - x[i]
  }
  
  
  #Paso 2
  for (i in 1:j) { 
    if(i != 1){
      A[i] = (3 * (a[i + 1] - a[i])/(h[i])) - (3 * (a[i] - a[i - 1]) /h[i - 1])
    }
  }
  
  
  #Paso 3
  l[1] = 1
  u[1] = 0
  z[1] = 0
  
  #Paso 4
  for (i in 2:j) {
    l[i] = 2 * (x[i + 1] - x[i - 1]) - h[i - 1] * u[i - 1]
    u[i] = h[i]/l[i]
    z[i] = (A[i] - h[i - 1] * z[i - 1])/l[i]
  }
  
  #Paso 5
  l[n] = 1
  z[n] = 0
  c[n] = 0
  
  #Paso 6
  for (i in j:1) {
    c[i] = z[i] - u[i] * c[i + 1]
    b[i] = (a[i + 1] - a[i])/h[i] - h[i] * (c[i + 1] + 2*c[i])/3
    d[i] = (c[i + 1] - c[i])/(3*h[i])
  }
  
  #Paso 7
  results = matrix(rep(NA, 4*j), nrow = j, ncol = 4, byrow = F)
  for (k in 1:j) {
    results[k, 1] = a[k]
    results[k, 2] = b[k]
    results[k, 3] = c[k]
    results[k, 4] = d[k]
  }
  return(results)
}

x=c(1,2,3)
fx=c(2,3,5)

A=NaturalCubicSpline(x,fx)

InterpolacionNCS<-function(x0,x,Coef){
  interpolacion=0
  n=length(x)
  for(i in 1:(n-1)){
    if(x0 < x[i+1]){
      interpolacion=Coef[i,1] + (Coef[i,2]*(x0 - x[i])) + (Coef[i,3]*(x0 - x[i])^2) + (Coef[i,4]*(x0 - x[i])^3)
      return(interpolacion)
    }
  }
}

PolinomioInterpolanteNCS<-function(Coef,x){
  n=length(x)
  S=matrix("",nrow=(n-1),ncol=2)
  colnames(S)=c("S_i(x)","Intervalo")
  aux=""
  for(i in 1:(n-1)){
    aux=paste0(Coef[i,1],"+","(",Coef[i,2],")*(x-(",x[i],"))","+","(",Coef[i,3],")*(x-(",x[i],"))^2","+","(",Coef[i,4],")*(x-(",x[i],"))^3")
    S[i,1]=paste0("S_",i,"(x)=",aux)
  }

  for(i in 1:(n-1)){
    S[i,2]=paste0("[",x[i],",",x[i+1],"]")
  }
  return(S)
}

InterpolacionNCS(1.2,x,A)

#Ejercicio
x3_dado=c(-0.5,-0.25,0.25,0.5)
f3_dado=c(1.93750,1.33203,0.800781,0.687500)
B=NaturalCubicSpline(x3_dado,f3_dado)
InterpolacionNCS(0,x3_dado,B)

PolinomioInterpolanteNCS(B,x3_dado)
