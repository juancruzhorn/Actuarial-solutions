#Iteración de punto fijo

Iteracion_PF<-function(Fn,p0,TOL,N){
  #Paso 1
  i=1
  #condición g'(x)<=k<1
  #Paso 2
  while (i<N){
    
    #Paso 3
    p=Fn(p0)
    #Paso 4
    if (abs(p-p0)<TOL){
      return(paste("El valor p es = ",p,". Obtenido en la ",i," iteración"))
    } else {
      #Paso 5
      i=i+1
      #Paso 6
      p0=p
    }
  }
  #Paso 7
  return (paste ("El metodo fallo despues de", N, " iteraciones"))
}

#c) x=g3(x)=1/2*(10-x^3)^1/2

FuncionPF3<-function(x){
  return(1/2*(sqrt(10-x^3)))
}

resultados<-seq(from=0,to=10,by=0.001)
plot(resultados,FuncionPF3(resultados),axes= TRUE)
Iteracion_PF(FuncionPF3,1.3,10^-10,100)
#Verificación
FuncionPF3(1.36523001150343)

(FuncionPF3(1.500001)-FuncionPF3(1.5))/0.000001
