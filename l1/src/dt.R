source("utils.R")

deps <- c("read-matrix.R", "tree.R")

dependencies.loader(deps)

VALUES <- list() # variable de acceso global

# Funcion auxiliar para el calculo entropia
# Parametros:
#   values: es un vector de valores de la "variable"

entropia <- function(values) {
  ent <- 0
  
  total <- sum(values)
  
  if (total == 0) {
    return (total)
  }
  
  for (i in 1:length(values)) {
    p <- values[i] / total
    
    if (p != 0) {
      ent <- ent + ((-p) * log2(p))
    }
  }
  
  return(ent)
}


# Elige el mejor atributo para clasificar los ejemplos en base a 'Information Gain'.
#
# Los parámetros son:
#
#     "examples" es el conjunto de ejemplos
#
#     "target" es un string con el nombre del atributo cuyo valor debe ser predecido por el árbol
# Return a string with the name of the best attribute to classify the examples.
# Use la funcion "entropia" para el cálculo de la misma

best.attribute <-
  function(examples,
           attributes,
           target,
           labels,
           splitInf = FALSE) {
    best.att <- "" #guardará nombre del mejor atributo
    
    #######
    #
    # ADD YOUR CODE HERE
    #
    ########
    targetValues <- examples[, target]
    cantTotal <- nrow(examples)
    cants <- matrix(data = 0,
                    nrow = length(labels),
                    ncol = 2)
    
    values <- c()
    
    
    for (i in 1:length(labels)) {
      values[i] <- sum(targetValues == labels[i])
    }
    
    entropia <- entropia(as.numeric(values))
    
    # Matriz con las ganancias de cada columna
    ganancias <- matrix(data = 0,
                        nrow = length(attributes),
                        ncol = 2)
    
    for (i in 1:length(attributes)) {
      data <- matrix(data = 0,
                     nrow  = cantTotal,
                     ncol = 2)
      data[, 1] <- examples[, attributes[i]]
      data[, 2] <- targetValues
      
      nodos <- unique(data[, 1])
      entropias <- c()
      splitValue <- 0
      
      for (j in 1:length(nodos)) {
        nodo <- nodos[j]
        
        nodoData <- matrix(data = 0,
                           nrow = sum(data[, 1] == nodo),
                           ncol = 2)
        nodoData[, 1] <- data[, 1][data[, 1] == nodo]
        nodoData[, 2] <- data[, 2][data[, 1] == nodo]
        
        nodoTotal <- nrow(nodoData) # valores con la etiqueta
        p <- c()
        
        for (k in 1:length(labels)) {
          if (nrow(nodoData) > 1) {
            pm <- sum(nodoData[, 2] == labels[k])
          } else {
            pm <- 1
          }
          
          # guarda en arreglo para entropía
          p <-  c(p, pm)
        }
        
        splitValue <- splitValue - (nodoTotal / cantTotal) * log2(nodoTotal / cantTotal)
        
        entropias <- c(entropias, (nodoTotal / cantTotal) * entropia(p))
      }
      
      ganancias[i, 1] <- attributes[i]
      ganancias[i, 2] <- entropia - sum(entropias)
      
      if(splitInf){
        ganancias[i,2] <- (entropia - sum(entropias)) / splitvalue
      }else{
        ganancias[i,2] <- entropia - sum(entropias)
      }
    }
    
    best.att <- ganancias[order(ganancias[, 2], decreasing = TRUE)][1]
    gan <- ganancias[order(ganancias[, 2], decreasing = TRUE), 2][1]
    
    return(best.att)
  }


# Choose the most common value from a set of examples
#
# The parameters of this function are:
#     "examples" are a set of example
#
#     "target" is a string with the name of attribute whose value is to be
#     predicted by the tree.
#
# Return a string with the name of the best attribute to classify the examples.
most.common.value <- function(examples, target) {
  value <- ""
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  labels <- unique(examples[, target])
  cants <- matrix(data = 0,
                  nrow = length(labels),
                  ncol = 2)
  
  for (i in 1:length(labels)) {
    cants[i, 1] <- labels[i]
    cants[i, 2] <- length(which(examples[, target] == labels[i]))
  }
  
  value <- cants[order(cants[, 2], decreasing = TRUE)[1], 1]
  
  # print(c('Valor mas comun de', target, value))
  
  return(value)
}

# Get the decision tree from a set of examples.
#
# The parameters of this function are:
#
# 	"examples" are the set of examples
#
# 	"target" is the attribute whose value is to be predicted by the tree
#
# 	"attributes" is a vector of other attributes that may be tested by the
# 	learned decision tree.
#
#   "labels" is a vector with the labels to classify the "examples"
#
#   "tree" is data structure to save the decision tree
#
# Esta función regresa una lista con dos componentes: 'root', que es la variable nodo
# obtenido de la iteración con id3, y 'tree' que es el árbol construido.
id3 <- function(examples, target, attributes, labels, tree) {
  examples <- matrix(
    examples,
    ncol = length(attributes) + 1,
    dimnames = list(
      rownames = NULL,
      colnames = c(attributes, target)
    )
  )
  
  #se crea una estructura vacía de nodo
  root <- NULL
  
  # ¿Tienen todos los ejemplos la misma etiqueta?
  for (i in 1:length(labels))
    if (all(examples[, target] == labels[i])) {
      class <- labels[i]
      root <- new.leaf(class)
      
      return(new.tree(root))
    }
  
  # ¿Se encuentra vacío el conjunto de los atributos?
  if (length(attributes) == 0) {
    class <- most.common.value(examples, target)
    root <- new.leaf(class)
    
    return(new.tree(root))
  }
  
  attribute <- best.attribute(examples, attributes, target, labels)
  
  root <- new.node(attribute, VALUES[[attribute]])
  
  if (is.null(tree))
    tree <- new.tree(root) # Raíz
  # cat("attribute selected: ", attribute, "\n")
  
  
  for (i in 1:length(VALUES[[attribute]])) {
    #Add a new tree branch below Root, corresponding to the test A = vi
    branchId <- root$branches[[i]]
    
    value <- as.character(VALUES[[attribute]][i])
    # print(paste("root is ", root$name, "value selected: ", value))
    
    #Subset of examples that have value vi for A
    Anumber <-
      which(attribute == colnames(examples)) #column number of attribute
    
    fila <- which(examples[, Anumber] == value)
    
    examplesi <- examples[fila, ]
    
    #examplesi as 1 row?
    if (length(examplesi) == (length(attributes) + 1)) {
      examplesi <-
        matrix(
          examplesi,
          ncol = (length(attributes) + 1),
          dimnames = list(
            rownames = NULL,
            colnames = c(attributes, target)
          )
        )
    }
    
    if (is.null(examplesi) | nrow(examplesi) == 0) {
      # Add a leaf node with label = most common value of target in examples
      
      #######
      #
      # ADD YOUR CODE HERE
      #
      ########
      class <- most.common.value(examples, target)
      leaf <- new.leaf(class)
      
      tree <- add.subtree(tree, leaf, branchId)
      
    } else {
      # <-- ES UNA MATRIX. Es un subconjunto de Examplesi (ver seudocódigo en Mitchel.)
      exam <- NULL
      
      #######
      #
      # ADD YOUR CODE HERE
      #
      ########
      exam <- examplesi[, -Anumber]
      
      subtree <-
        id3(exam, target, attributes[-Anumber], labels, NULL)
      
      tree <- add.subtree(tree, subtree, branchId)
    }
    
  }
  
  return(tree)
}

# Classify an example from the tree model
#
# The parameters are
#
#	"tree" the tree model obtained with learn.tree()
#	"example" one example to be classify
#
# Return the label for "example"
classify.example <- function(tree = NULL, example = NULL)
{
  label <- NULL
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  parent <- tree$nodes[[1]]$parentId
  branches <- tree$nodes[[1]]$branches
  
  if (is.null(parent)) {
    #Es la raíz?
    
    varExam <- NULL
    
    # Busca raíz
    for (i in 1:length(example)) {
      #Si alguno de los valores del ejemplo está presente en alguna de las ramas del arbol
      if (any(example[i] == names(branches))) {
        #Guardo la variable del ejemplo que coincide con la raiz
        varExam <-
          example[which(any(example[i] == names(branches)))]
        break
      }
    }
    
    #Guardo el parentId de la raiz
    parent <-
      branches[which(names(branches) == varExam)]
  }
  
  # Si no es la raiz tengo que recorrer el resto del arbol
  # Itero sobre los nodos del arbol
  for (v in 2:tree$nodesCount) {
    # Es la raíz del subarbol?
    if (tree$nodes[[v]]$parentId == parent) {
      # Es una hoja?
      if (!is.null(tree$nodes[[v]]$label)) {
        # print(paste("Resultado: ", tree$nodes[[f]]$label))
        label <- tree$nodes[[v]]$label
        break
      }
      
      # No es una hoja, es un branch
      # Determino por cual nodo debo seguir recorriendo
      
      for (i in 1:length(example)) {
        # Verifico si algun valor del ejemplo se corresponde con algun branch para seguir recorriendo
        if (any(names(tree$nodes[[v]]$branches) == example[i])) {
          #Guardo el parentId
          parent <-
            tree$nodes[[v]]$branches[which(example[i] == names(tree$nodes[[v]]$branches))]
          break
        }
      }
    }
  }
  
  return(label)
}

# Realice todo el pre-procesamiento necesario de los datos en esta función.
# En la función se encuentra el pre-procesamiento del dataset PlayTennis para que
# se obtengan estructuras necesarias para trabajar con ID3.
# Modifique esta función para poder manipular distintos tipos de dataset
# (spam, restaurant,tennis)
# De esta manera se le facilitará la carga de datos
#
# REGRESA:
# target.attribute: la etiqueta del atributo objetivo target
# labels: los valores posibles de clasificación
# examples: matriz conjunto de ejemplos que serán utilizados para clasificar el árbol
# attributes: vector listado de nombres de atributos

load.data <- function(path.data = "../data/", name = 'tennis.csv') {
  #variables que debe completar, luego de cargar cada dataset
  target <- NULL
  labels <- NULL
  examples <- NULL
  attributes <- NULL
  
  if (startsWith(name, "tennis")) {
    path <- paste(path.data, name, sep = "")
    examples <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
    examples <- as.matrix(examples)
    
    cols <- colnames(examples)
    cols.length <- ncol(examples)
    # Todas las columnas excepto la última
    attributes <- cols[-cols.length]
    etiquetas <- unique(examples[, cols.length]) # Yes, no
    # Nombre de última columna
    target <- cols[length(cols)]
    
    ## las siguientes líneas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[, attributes[i]])
    
  } else if (startsWith(name, "restaurant")) {
    #######
    #
    # ADD YOUR CODE HERE
    #
    ########
    path <- paste(path.data, name, sep = "")
    examples <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
    examples <- as.matrix(examples)
    
    cols <- colnames(examples)
    cols.length <- ncol(examples)
    attributes <- cols[-cols.length]
    etiquetas <- unique(examples[, cols.length])
    target <- cols[length(cols)]
    
    ## las siguientes líneas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[, attributes[i]])
    
  } else if (startsWith(name, "web_sample")) {
    #######
    #
    # ADD YOUR CODE HERE
    #
    ########
    path <- paste(path.data, name, sep = "")
    examples <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
    examples <- as.matrix(examples)
    
    cols <- colnames(examples)
    cols.length <- ncol(examples)
    attributes <- cols[-cols.length]
    etiquetas <- unique(examples[, cols.length])
    target <- cols[length(cols)]
    
    ## las siguientes líneas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[, attributes[i]])
  } else if (startsWith(name, "MATRIX")) {
    source("read-matrix.R")
    path <- paste(path.data, name, sep = "")
    
    m.train <-
      read_matrix(filename = path,
                  ocurrence = FALSE,
                  sparse = FALSE)
    
    examples <- as.matrix(m.train$matrix)
    attributes <- m.train$tokens
    # attributes <- as.vector(dimnames(examples)[[2]])
    # attributes <- attributes[-ncol(examples)]
    etiquetas <- unique(examples[, ncol(examples)])
    target <- (colnames(examples))[length(colnames(examples))]
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[, i])
  } else
    stop("ERROR Debe brindar un dataset. Verifique argumentos path.data y name")
  
  return (
    list(
      target.attribute = target,
      labels = etiquetas,
      examples = examples,
      attributes = attributes
    )
  )
}

run.tree.experiment <- function() {
  # 1- CARGA DE DATOS
  # path.data : donde se encuentra el directorio data de este laboratorio 1
  # name: nombre del dataset incluida su extensión de archivo, ej: restaurant.csv
  #
  # dataset <- load.data(path.data = "../data/", name = "tennis.csv")
  dataset <- load.data(path.data = "../data/", name = "MATRIX.TRAIN.50")
  ## Para ver los elementos de dataset,
  ## descomente las siguientes líneas antes de ejecutar
  # print(dataset$target)
  # print(dataset$labels)
  # print(dataset$attributes)
  
  # 2- CONSTRUCCIÓN DEL ÁRBOL USANDO ID3
  result <- id3(
    dataset$examples,
    dataset$target.attribute,
    dataset$attributes,
    dataset$labels,
    NULL
  )
  
  # La función plot.tree permite ver el árbol graficado
  plot.tree(result)
  
  # 3- CLASIFICAR un nuevo ejemplo
  # Complete el argumento example con el nuevo ejemplo a clasificar
  # (e.g example=c("v1","v2","valor3"))
  # El ejemplo dependerá del dataset con el que esté trabajando
  # Muestre en consola el ejemplo a clasificar y el resultado.
  example <- c("Overcast", "Mild", "Normal", "Strong")
  classify.example(tree = result, example = example)
}