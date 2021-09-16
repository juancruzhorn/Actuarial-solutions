rm(list=ls())
#Semana 2

#Eliminación gaussiana

EliminacionGaussiana<-function(A,B){
  #Paso 1
  Aa<-cbind(A,B)
  n=nrow(Aa)
  #Paso 2
  for(i in 1:(n-1)){
    pp=0
    for(p in i:n){
      if(Aa[p,i]!=0){
        pp<-p
        break
      }
    }
  
  if(pp==0){
    return("No hay solución única para el sistema")
    break
  }
  
  #Paso 3
  if (pp!=i){
    aux = Aa[pp,]
    Aa[pp,] = Aa[i,]
    Aa[i,] = aux
  }
  
  #Paso 4
  for(j in (i+1):n){
    
    #Paso 5
    m<-Aa[j,i]/Aa[i,i]
    
    #Paso 6
    Aa[j,]<-Aa[j,]-Aa[i,]*m
  }
  
  #Paso 7
  if(Aa[n,n]==0){
    return("No existe solución única")
    break
  }
  }
  
  #Paso 8
  x = rep(NA, times = n)
  x[n]=Aa[n,n+1]/Aa[n,n]
  
  #Paso 9
  for (i in (n-1):1){
    aux<-0
    for (j in (i+1):n){
      aux<-aux+(Aa[i,j]*x[j])
    }
    x[i]<-(Aa[i,n+1]-aux)/Aa[i,i]
  }
  
  return (x)
}

MatrizA = matrix(c(1,-1,2,-1,
                   2,-2,3,-3,
                   1,1,1,0,
                   1,-1,4,3), nrow = 4, ncol = 4, byrow = T)
VectorB = matrix(c(-8,-20,-2,4), nrow = 4, ncol = 1)
C<-EliminacionGaussiana(MatrizA,VectorB)

MatrizA%*%C
MatrizA
VectorB
cbind(MatrizA,VectorB)
