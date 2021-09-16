rm(list=ls())

Cholesky<-function(A){
  
  n=nrow(A)
  L=matrix(data=0,nrow=n,ncol=n)
  
  #Paso 1
  L[1,1]=sqrt(A[1,1])
  
  #Paso 2
  j=2
  for(j in j:n){
    L[j,1]=A[j,1]/L[1,1]
  }
  
  #Paso 3
  i=2
  nn=n-1
  for(i in i:nn){
    
    #Paso 4
    aux=0
    k=1
    for(k in k:(i-1)){
      aux=aux+L[i,k]^2
    }
    L[i,i]=sqrt(A[i,i]-aux)
    
    #Paso 5
    aux=0
    for(j in (i+1):n){
      aux=0
      k=1
      for(k in k:(i-1)){
        aux=aux+L[j,k]*L[i,k]
      }
      L[j,i]=(A[j,i]-aux)/L[i,i]
    }
  }
  
  #Paso 6
  aux=0
  k=1
  for(k in k:(n-1)){
    aux=aux+L[n,k]^2
  }
  L[n,n]=sqrt(A[n,n]-aux)
  
  #Paso 7
  return (L)
}

A<-matrix(data= c(4,-1,1,
                  -1,4.25,2.75,
                  1,2.75,3.5),
          nrow=3,ncol=3,byrow=TRUE)
B<-Cholesky(A)
B%*%t(B)

