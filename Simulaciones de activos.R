rm(list=ls())
graphics.off()

#Supuestos del MGB
#Parámetros
p0=45
mu=0.1
sigma=0.2

T<-0.5 #seis meses
n=182 #número de time steps
dt=T/n #time step

m<-1000 #cantidad de caminos
Pt<-matrix(NA,nrow=m,ncol=n+1)
Pt[,1]<-p0

for(i in 1:m){
  for(t in 2:(n+1)){
    Pt[i,t]<-Pt[i,t-1]*exp((mu-0.5*sigma^2)*dt+(sigma*sqrt(dt)*rnorm(1)))
  }
}

#Grafico
t<-rep(0:n,m)
t<-matrix(t,nrow=m,ncol=n+1,byrow=T)

plot(t[1,],Pt[1,],type="l",ylim=c(min(Pt),max(Pt)))
for(i in 2:m){
  lines(t[i,],Pt[i,],col=trunc(runif(1)*m))
}

#Vector de promedios
M<-matrix(NA,nrow=1,ncol=n+1)
for(i in 1:(n-1)){
  M[i]=mean(Pt[,i])
}

#Percentiles para intervalos de confianza
prob=.95
LS<-matrix(NA,nrow=1,ncol=n+1)
for(i in 1:(n+1)){
  LS[i]<-quantile(Pt[,i],prob)
}
LI<-matrix(NA,nrow=1,ncol=n+1)
for(i in 1:(n+1))

lines(t[1,],M,col='black',lwd=5)
lines(t[1,],LS,col='red',lwd=5)
lines(t[1,],LI,col='red',lwd=5)

#Simulación de precio final
m<-1000 #cantidad de simulaciones
e<-rnorm(m)
PT<-matrix(NA,nrow=m,ncol=1)
PT<-p0*exp((mu-0.5*sigma^2)*T+sigma*(sqrt(T)*e))
hist(PT)
quantile(PT,prob)
quantile(PT,1-prob)

#Simulación de precios incorrelaciondos
p0<-c(45,50,108)
mu<-c(0.1,0.15,0.08)
sigma<-c(0.2,0.25,0.3)
T<-0.5
m<-1000

z<-matrix(rnorm(3*m),nrow=m,ncol=3) #incorrelacionados

P<-matrix(NA,m,3)

for(i in 1:m){
  for (k in 1:3){
    P[i,k]=p0[k]*exp(mu[k]-0.5*sigma[k]^2*T+sigma[k]*sqrt(T)*z[i,k])
  }
}

Rho<-diag(3)
Rho[1,2]<-Rho[2,1]<-0.9
Rho[1,3]<-Rho[3,1]<-0.7
Rho[2,3]<-Rho[3,2]<-0.6

CH<-chol(Rho)
e<-z%*%CH
cor(e)

#Simulación de precios correlacionados
Pc<-matrix(NA,m,3)
for(i in 1:m){
  for (k in 1:3){
    Pc[i,k]=p0[k]*exp(mu[k]-0.5*sigma[k]^2*T+sigma[k]*sqrt(T)*e[i,k])
  }
}

P0.m=matrix(rep(p0,m),m,3,byrow=3)
RLc<-log(Pc/P0.m) #retorno logarítmico

cor(RLc)

par(mfrow=c(2,3))
plot(P[,1],P[,2])
plot(P[,1],P[,3])
plot(P[,3],P[,2])
plot(Pc[,1],Pc[,2])
plot(Pc[,1],Pc[,3])
plot(Pc[,3],Pc[,2])

#Simulación del retorno de una cartera
#Precios independientes

#Input
Q<-c(100,150,120)
V0<-Q*p0 #valor de cada activo
VI<-sum(V0) #valor inicial de la cartera

Q.m<-matrix(rep(Q,m),
            nrow=m,ncol=3,
            byrow=TRUE)
VT<-Q.m*P #matriz de valores en el horizonte T de activos
VF<-matrix(rowSums(VT),m,1) #matriz de los valores finales

#Rendimiento logarítmico
RL.V<-log(VF/VI)
hist(RL.V)

#Simulación del retorno de una cartera
#Precios correlacionados

VTc<-Q.m*Pc
VFc<-matrix(rowSums(VTc),m,1)

RVc<-log(VFc/VI)
hist(RVc)
