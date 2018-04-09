
####################################################################################################
# Use esta función para realizar el preprocesamiento del dataset iris.data
####################################################################################################
# Esta funcion lee N datapoints del dataset iris.data
# y devuelve una matriz numérica del dataset
#

pre_process <- function()
{  
  
  dataset.location <- "../data/iris.data"
  
  XY <- read.csv(dataset.location,header=TRUE,stringsAsFactors=FALSE)
  
  for(i in 1:nrow(XY)) {
    n <- XY[i, 5]
    
    if(n == "Iris-virginica") {
      
      XY[i, 5] <- as.numeric(1)
    } else {
      XY[i, 5] <- as.numeric(-1)
    }
  }
  
  return(XY)
}

####################################################################################################
# Use esta función para realizar el aprendizaje del discriminador
####################################################################################################
# recibe una matriz numérica con los datos preprocesados
# y devuelve una lista list(XEntrenamiento, YEntrenamiento, XTesteo, YTesteo, w)
# donde:
# XEntrenamiento: es la matriz X correspondiente a datos de entrenamiento
# YEntrenamiento: es la matriz Y correspondiente a datos de entrenamiento
# XTesteo: es la matriz X correspondiente a datos de testeo
# YTesteo: es la matriz Y correspondiente a datos de testeo
# w: es la matriz de pesos w obtenida en (4)
#

learn <- function(data,N)
{  
  
  N*0.7 -> finEntr;
  data[sample(nrow(data),N),] -> XYDesordenada
  XEntrenamiento <- NULL
  YEntrenamiento <- NULL
  XTesteo <- NULL
  YTesteo <- NULL
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  XEntrenamiento <- as.matrix(XYDesordenada[1:finEntr, 1:4])
  YEntrenamiento <- as.matrix(as.numeric(XYDesordenada[1:finEntr, 5]))
  XTesteo <- as.matrix(XYDesordenada[(finEntr + 1):N, 1:4])
  YTesteo <- as.matrix(as.numeric(XYDesordenada[(finEntr + 1):N, 5]))

  w <- solve(t(XEntrenamiento) %*% as.matrix(XEntrenamiento)) %*% t(XEntrenamiento) %*% YEntrenamiento
  
  ret <- list(XEntrenamiento=XEntrenamiento,
              YEntrenamiento=YEntrenamiento,
              XTesteo=XTesteo,
              YTesteo=YTesteo,
              w=w)
  return(ret)
}

####################################################################################################
# Use esta función para realizar la evaluacion del desempeño del discriminador
####################################################################################################
# recibe una matriz numérica con los datos preprocesados
# Evalúa el discriminador calculado su error
# Muestra por pantalla y guarda en el archivo outputs.raws los resultados (punto c)
# Descripción de campos:
# nombre de dataset
# N
# número de repetición
# error de clasificación
# iris.data,N,R,E
# 

evaluate <- function(data)
{ 

  # place your code here
  #N<-150;
  
  N <- c(50,100,150)
  
  salida <- c(1:30)
  
  for(n in N)
  {
    errores <- c(1:10)
    
    for(i in 1:10){
      
      exitos <- 0;
      
      l <- learn(data,n)
      
      #######
      #
      # ADD YOUR CODE HERE
      #
      ########
      errores[i] <- 0
      
      for(j in 1:nrow(l$XTesteo)) {
        YResultado <- l$XTesteo[j, 1:4] %*% l$w
        YResultado <- YResultado[1, 1]
        YTesteo <- l$YTesteo[j, 1]
        
        if((YResultado < 0 && YTesteo < 0) || (YResultado > 0 && YTesteo > 0)) {
          exitos <- exitos + 1
        } else {
          errores <- errores + 1
        }  
      }
      
      salida[n/50*10-10+i] <- (paste("iris.data,",n,",",i,",",(errores[i])));
      
    
    }
    
    #ErrorPromedio<-0;
    #for(i in 1:10){
    #  ErrorPromedio<-errores[i]+ErrorPromedio;
    #  salida[n/50*10-10+i]<-(paste("iris.data, ",n,", ",i,", ",(errores[i])));
    #}
    #N<-N-50;
  }
  
  ref<-c("# Descripción de campos:","# nombre de dataset","# N","# número de repetición","# error promedio de clasificación");
  print(append(ref,salida));
  write(append(ref,salida),file="output.raws");
}

####################################################################################################
# Use esta función muestra por pantalla los errores promedios para cada corrida
####################################################################################################
# procesa el archivo outputs.raws generado por evaluate()
# devuelve las tupas (N,u)
# N: numero de datos
# u: error promedio
# 
pos_process <- function() {  
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  res <- c("N","u")
  N.vector <- c(50,100,150)
  raw.data <- readLines("output.raws")
  data <- raw.data[!startsWith(raw.data, '#')]
  
  # format(round(mean(c(1,2,4)), 2), nsmall=2)
  
  for(i in 1:length(data)) {
    params <- strsplit(data[i], ", ")[[1]]
    N <- as.numeric(params[2])
    rep <- as.numeric(params[3])
    errores <- as.numeric(params[4])
    
    vectorIndex <- match(N, N.vector)
  }
  
  return(res)
}

####################################################################################################
# Use esta función correr el experimento completo
####################################################################################################
# 
# 
run.experiment<- function()
{
  set.seed(1)
  data <- pre_process()
  evaluate(data)
  pos_process()
}

