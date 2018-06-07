# load utils functions read.dataset() split.data() confusion.matrix()
source("utils.R") 

deps <- c("utils.R") 

dependencies.loader(deps)

# INSTALE LAS librerías nnet y e1071

library(nnet) # neural network library
library(e1071) # svm library


# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.positives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- cm[k, k]

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.negatives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- cm[-k, -k]

  sum(res)
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.positives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- cm[-k, k]

  sum(res)
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.negatives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- cm[k, -k]

  sum(res)
}


accuracy <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- (true.pos + true.neg) / (true.pos + true.neg + false.pos + false.neg)

  return(res)
}

precision <- function(true.pos, false.pos) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- true.pos / (true.pos + false.pos)

  return(res)
}

recall <- function(true.pos, false.neg) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  res <- true.pos / (true.pos + false.neg)

  return(res)
}

f.measure <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  p <- precision(true.pos, false.pos)
  r <- recall(true.pos, false.neg)
  
  res <- 2 * ((p * r) / (p + r))

  return(res)
}

run.annvssvm <- function()
{
  ## Read dataset
  data <- read.dataset("../data/faces.csv")
  
  ## Split for train and test sets
  splits <- split.data(data, 0.3)
  train.set <- splits$train
  test.set <- splits$test
  
  ann <- NULL # clasificador aprendido por la red neuronal usando la función nnet
  svm <- NULL # clasificado aprendido por SVM usando la función svm
  
  predicted.by.ann <- c() #resultados de la predicción usando el modelo ANN (método predict)
  predicted.by.svm <- c() #resultados de la predicción usando el modelo SVM (método predict)
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  # ANN
  print("=== ANN ===")
  predicted.by.ann <- nnet(formula = class ~ ., data = train.set, size = 3, MaxNWts = 3000)
  res <- predict(predicted.by.ann, test.set, type="class")
  
  #matriz de confusión para los resultados de ANN
  mat_ann <- confusion.matrix(test.set$class, res)
  
  print(mat_ann)
  
  levs <- colnames(mat_ann) #lista de labels/target o "niveles" de clasificación
  
  #iteramos para mostrar los resultados (accuracy,recall,precision,fmeasure,etc)
  #de la clasificación en cada clase
  #imprimimos al final los resultados
  for (k in 1:length(levs)){
    
    tp_ann <- true.positives(mat_ann, k)
    tn_ann <- true.negatives(mat_ann, k)
    fp_ann <- false.positives(mat_ann, k)
    fn_ann <- false.negatives(mat_ann, k)
    
    acc_ann <- accuracy(tp_ann, tn_ann, fp_ann, fn_ann)
    prec_ann <- precision(tp_ann,fp_ann)
    rec_ann <- recall(tp_ann, fn_ann)
    f_ann <- f.measure(tp_ann, tn_ann, fp_ann, fn_ann)
    
    cat("\nk = ",k, ", Class:", levs[k], " tp:",tp_ann," tn:",tn_ann," fp:",fp_ann,"  fn:", fn_ann,
        "\nAccuracy: ", acc_ann,
        "\nPrecision:", prec_ann,
        "\nRecall    ", rec_ann,
        "\nF-measure:", f_ann)
  }
  
  # CREE LA MATRIZ DE CONFUSIÓN PARA LOS RESULTADOS CON SVM
  # PROCEDA A MOSTRAR LAS MÉTRICAS
  # PUEDE BASARSE EN EL CÓDIGO ANTERIOR PARA ANN
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  # SVM
  cat("\n=== SVM ===")
  predicted.by.svm <- svm(formula = class ~ ., data = train.set)
  res <- predict(predicted.by.svm, test.set)
  
  #matriz de confusión para los resultados de ANN
  mat_svm <- confusion.matrix(test.set$class, res)
  
  print(mat_svm)
  
  levs <- colnames(mat_svm) #lista de labels/target o "niveles" de clasificación
  
  #iteramos para mostrar los resultados (accuracy,recall,precision,fmeasure,etc)
  #de la clasificación en cada clase
  #imprimimos al final los resultados
  for (k in 1:length(levs)){
    
    tp_svm <- true.positives(mat_svm, k)
    tn_svm <- true.negatives(mat_svm, k)
    fp_svm <- false.positives(mat_svm, k)
    fn_svm <- false.negatives(mat_svm, k)
    
    acc_svm <- accuracy(tp_svm, tn_svm, fp_svm, fn_svm)
    prec_svm <- precision(tp_svm,fp_svm)
    rec_svm <- recall(tp_svm, fn_svm)
    f_svm <- f.measure(tp_svm, tn_svm, fp_svm, fn_svm)
    
    cat("\nk = ",k, ", Class:", levs[k], " tp:",tp_svm," tn:",tn_svm," fp:",fp_svm,"  fn:", fn_svm,
        "\nAccuracy: ", acc_svm,
        "\nPrecision:", prec_svm,
        "\nRecall    ", rec_svm,
        "\nF-measure:", f_svm)
  }
  
  mat_svm
}

