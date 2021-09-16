rm(list=ls())

FactorizacionLU<-function(A){
  i=1
  
  n=nrow(A)
  
  L=matrix(data=0,nrow=n,ncol=n,byrow=TRUE)
  U=matrix(data=0,nrow=n,ncol=n,byrow=TRUE)
  
  #Creo la matriz L=I(n) para luego completar con los m_ij
  for(i in 1:n){
    L[i,i]=1
  }
  
  #Paso 1
  if(A[1,1]==0){
    return("Mission failed")
  } else {
    U[1,1]=A[1,1]/L[1,1]
  }
  
  #Paso 2
  j=2
  for(j in j:n){
    U[1,j]=A[1,j]/L[1,1] #Primer renglón U
    L[j,1]=A[j,1]/U[1,1] #Primera columna L
  }
  
  #Paso 3
  i = 2
  nn=(n-1)
  while(i<=nn){
    #Paso 4
    aux=0
    k= 1
    l=i-1
    for(k in k:l){
      aux<-aux+L[i,k]*U[k,i]
      U[i,i]=A[i,i]-aux
    }
    if(U[i,i]==0){
      return("No way José")
      break
    }
  
  
  #Paso 5
  m=(i+1)
   for(j in m:n){
      auxU=0
      k=1
      for(k in k:(i-1)){
        auxU=auxU+L[i,k]*U[k,j]
      }
      auxL=0
      k=1
      for (k in k:(i-1)){
        auxL=auxL+L[j,k]*U[k,i]
      }
      U[i,j]=(1/L[i,i])*(A[i,j]-auxU)
      L[j,i]=(1/U[i,i])*(A[j,i]-auxL)
      }
    i=i+1
  }
  
  #Paso 6
  aux=0
  k=1
  nn=n-1
  for(k in k:nn){
    aux=aux+L[n,k]*U[k,n]
  }
  U[n,n]=A[n,n]-aux
  
  #Paso 7
  return(list("L"=L,"U"=U))
  
}

A<-matrix(data=c(1,1,0,3,
                 2,1,-1,1,
                 3,-1,-1,2,
                 -1,2,3,-1),
          nrow=4,ncol=4,byrow=TRUE)

B<-FactorizacionLU(A)
