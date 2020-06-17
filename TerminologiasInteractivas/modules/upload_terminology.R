
upload_terminology <- function(statsPOSSplit, currentCorpus, termName){
  
  if("keyword" %in% colnames(statsPOSSplit)){
    statsPOSSplit$keyword <- as.character(statsPOSSplit$keyword)
    
    if(!("ngram" %in% colnames(statsPOSSplit))){
      ngram <- c()
      for(word in statsPOSSplit$keyword){
        append(ngram, length(strsplit(word, " ")[[1]])) -> ngram
      }
      cbind(statsPOSSplit, ngram) -> statsPOSSplit
    }
    
    print("La terminología tiene keywords")
    if("Frecuencia" %in% colnames(statsPOSSplit)){
      #statsPOSSplit$Frecuencia <- as.numeric(statsPOSSplit$Frecuencia)
      print("Se pueden obtener las puntuaciones de cada keyword")
      if("doc_id" %in% colnames(statsPOSSplit)){
        statsPOSSplit <- data.frame("doc_id" = as.character(statsPOSSplit$doc_id), "keyword" = as.character(statsPOSSplit$keyword), "ngram" = as.numeric(statsPOSSplit$ngram), "Frecuencia" = as.numeric(statsPOSSplit$Frecuencia))
        statsPOSSplit$keyword <- as.character(statsPOSSplit$keyword)
        print("Se puede obtener la puntuación tf_idf, RAKE y c-value")
        #Calcular puntuación tf_idf--------------------------------------------------------------------------------------------------------------------------------------------------
        print("Puntuación tf_idf: ")
        statsPOSSplit <- statsPOSSplit %>%
          bind_tf_idf(keyword, doc_id, Frecuencia)
        #doc_id - keyword - ngram - freq - tf - idf - tf_idf
        
        #Agrupamos la terminologia en funcion del documento, generando una terminología completa del corpus entero.
        statsPOSSplit <- select(statsPOSSplit, -c(doc_id, tf, idf))
        statsPOSSplit <- aggregate(list(Frecuencia=statsPOSSplit$Frecuencia, tf_idf=statsPOSSplit$tf_idf), by=list(keyword=statsPOSSplit$keyword, ngram=statsPOSSplit$ngram), FUN=sum)
        #Calcular puntuación RAKE-----------------------------------------------------------------------------------------------------------------------------------------------------
        print("Puntuación RAKE: ")
        terminosPorSeparado <- strsplit(statsPOSSplit$keyword, split = " ")
        data.frame(splited = unlist(terminosPorSeparado)) -> terminosPorSeparado
        table(terminosPorSeparado) -> tablaPOS
        tablaPOS <- data.frame(tablaPOS)
        
        score = c()
        pb = txtProgressBar(min = 0, max = nrow(statsPOSSplit), initial = 0)#Prescindible
        for (keyIndex in 1:nrow(statsPOSSplit)) {
          
          if(statsPOSSplit[keyIndex, "ngram"] == 1){
            score <- append(score, 0)
          } else {
            
            keySplt <- strsplit(statsPOSSplit[keyIndex, "keyword"], " ")
            keyDegree <<- statsPOSSplit[keyIndex, "ngram"] - 1
            
            tempScore <- c()
            for(word in keySplt){
              
              tempScore <- append(tempScore, keyDegree / tablaPOS[tablaPOS$terminosPorSeparado %in% word, ]$Freq)
              
            }
            score <- append(score, sum(tempScore))
          }
          setTxtProgressBar(pb,keyIndex)#Prescindible
        }
        
        cbind(statsPOSSplit, score) -> statsPOSSplit
        colnames(statsPOSSplit)[5] <- "RAKE"
        
        #Calcular puntuación c-value--------------------------------------------------------------------------------------------------------------------------------------------------
        print("Puntuación c-value")
        pb = txtProgressBar(min = 0, max = nrow(statsPOSSplit), initial = 0) #Prescindible
        
        cValue <- c()
        for (keyIndex in 1:nrow(statsPOSSplit)) {
          #Extraemos el termino candidato
          candidate <- statsPOSSplit[keyIndex, "keyword"]
          #Extraemos la frecuencia del candidato en el corpùs
          freqCandidate <- statsPOSSplit[keyIndex, "Frecuencia"]
          
          #Buscamos terminos que contengan a nuestro candidato. 
          coincidencias <- statsPOSSplit[grepl(candidate ,statsPOSSplit$keyword, fixed = TRUE), ]
          
          #Numero de coincidencias
          ncoincidencias <- nrow(coincidencias)
          if(ncoincidencias == 1){
            #El candidato no esta contenido en otro termino
            #Calculamos c-value
            res <- log2(length(strsplit(candidate, " ")[[1]])) * freqCandidate
            #Almacenamos el resultado
            append(cValue, res) -> cValue 
          } else {
            sumatorio <- sum(coincidencias$Frecuencia)
            res <- log2(length(strsplit(candidate, " ")[[1]])) * freqCandidate - (1 / ncoincidencias) * sumatorio
            #Almacenamos el resultado
            append(cValue, res) -> cValue 
          }
          setTxtProgressBar(pb,keyIndex)#Prescindible
        }
        
        cbind(statsPOSSplit, cValue) -> statsPOSSplit
        colnames(statsPOSSplit)[6] <- "cvalue"
        #construir terminología final, con autor y fecha
        terminology <- data.frame(Terminos = subset(statsPOSSplit, select=c("keyword")),
                                  ngram = subset(statsPOSSplit, select=c("ngram")),
                                  Autor = c(rep("Orignial", nrow(statsPOSSplit))),
                                  Fecha = c(rep(Sys.Date(), nrow(statsPOSSplit))),
                                  Frecuencia = subset(statsPOSSplit, select=c("Frecuencia")),
                                  tf_idf = subset(statsPOSSplit, select=c("tf_idf")),
                                  RAKE = subset(statsPOSSplit, select=c("RAKE")),
                                  cValue = subset(statsPOSSplit, select=c("cvalue"))
        )
        
        terminology$keyword <- as.character(terminology$keyword)
        terminology$ngram <- as.integer(terminology$ngram)
        terminology$Frecuencia <- as.character(terminology$Frecuencia)
        terminology$tf_idf <- as.character(terminology$tf_idf)
        terminology$RAKE <- as.character(terminology$RAKE)
        terminology$cvalue <- as.character(terminology$cvalue)
        
      } else {
        statsPOSSplit <- data.frame("keyword" = as.character(statsPOSSplit$keyword), "ngram" = as.numeric(statsPOSSplit$ngram), "Frecuencia" = as.numeric(statsPOSSplit$Frecuencia))
        statsPOSSplit$keyword <- as.character(statsPOSSplit$keyword)
        print("Si no se tiene la información del documento del que se ha obtenido cada keyword, en una columna que se llame doc_id, no se podra obtener la puntuación tf_idf, solo se obtendrá RAKE y c-value")
        #Calcular puntuación RAKE-----------------------------------------------------------------------------------------------------------------------------------------------------
        print("Puntuación RAKE: ")
        terminosPorSeparado <- strsplit(statsPOSSplit$keyword, split = " ")
        data.frame(splited = unlist(terminosPorSeparado)) -> terminosPorSeparado
        table(terminosPorSeparado) -> tablaPOS
        tablaPOS <- data.frame(tablaPOS)
        
        score = c()
        pb = txtProgressBar(min = 0, max = nrow(statsPOSSplit), initial = 0)#Prescindible
        for (keyIndex in 1:nrow(statsPOSSplit)) {
          
          if(statsPOSSplit[keyIndex, "ngram"] == 1){
            score <- append(score, 0)
          } else {
            
            keySplt <- strsplit(statsPOSSplit[keyIndex, "keyword"], " ")
            keyDegree <<- statsPOSSplit[keyIndex, "ngram"] - 1
            
            tempScore <- c()
            for(word in keySplt){
              
              tempScore <- append(tempScore, keyDegree / tablaPOS[tablaPOS$terminosPorSeparado %in% word, ]$Freq)
              
            }
            score <- append(score, sum(tempScore))
          }
          setTxtProgressBar(pb,keyIndex)#Prescindible
        }
        
        cbind(statsPOSSplit, score) -> statsPOSSplit
        colnames(statsPOSSplit)[4] <- "RAKE"
        
        #Calcular puntuación c-value--------------------------------------------------------------------------------------------------------------------------------------------------
        print("Puntuación c-value")
        pb = txtProgressBar(min = 0, max = nrow(statsPOSSplit), initial = 0) #Prescindible
        
        cValue <- c()
        for (keyIndex in 1:nrow(statsPOSSplit)) {
          #Extraemos el termino candidato
          candidate <- statsPOSSplit[keyIndex, "keyword"]
          #Extraemos la frecuencia del candidato en el corpùs
          freqCandidate <- statsPOSSplit[keyIndex, "Frecuencia"]
          
          #Buscamos terminos que contengan a nuestro candidato. 
          coincidencias <- statsPOSSplit[grepl(candidate ,statsPOSSplit$keyword, fixed = TRUE), ]
          
          #Numero de coincidencias
          ncoincidencias <- nrow(coincidencias)
          if(ncoincidencias == 1){
            #El candidato no esta contenido en otro termino
            #Calculamos c-value
            res <- log2(length(strsplit(candidate, " ")[[1]])) * freqCandidate
            #Almacenamos el resultado
            append(cValue, res) -> cValue 
          } else {
            sumatorio <- sum(coincidencias$Frecuencia)
            res <- log2(length(strsplit(candidate, " ")[[1]])) * freqCandidate - (1 / ncoincidencias) * sumatorio
            #Almacenamos el resultado
            append(cValue, res) -> cValue 
          }
          setTxtProgressBar(pb,keyIndex)#Prescindible
        }
        
        cbind(statsPOSSplit, cValue) -> statsPOSSplit
        colnames(statsPOSSplit)[5] <- "cvalue"
        #construir terminología final, con autor y fecha
        terminology <- data.frame(Terminos = subset(statsPOSSplit, select=c("keyword")),
                                  ngram = subset(statsPOSSplit, select=c("ngram")),
                                  Autor = c(rep("Orignial", nrow(statsPOSSplit))),
                                  Fecha = c(rep(Sys.Date(), nrow(statsPOSSplit))),
                                  Frecuencia = subset(statsPOSSplit, select=c("Frecuencia")),
                                  tf_idf = c(rep("NA", nrow(statsPOSSplit))),
                                  RAKE = subset(statsPOSSplit, select=c("RAKE")),
                                  cvalue = subset(statsPOSSplit, select=c("cvalue"))
        )
        
        terminology$keyword <- as.character(terminology$keyword)
        terminology$ngram <- as.integer(terminology$ngram)
        terminology$Frecuencia <- as.character(terminology$Frecuencia)
        terminology$tf_idf <- as.character(terminology$tf_idf)
        terminology$RAKE <- as.character(terminology$RAKE)
        terminology$cvalue <- as.character(terminology$cvalue)
        
      }
    } else {
      print("no se puede obtener ninguna puntuación de la temrinología que se ha introducido")
      #Rellenar las columnas de puntuación con NA, y autor y fecha
      #construir terminología final, con autor y fecha
      terminology <- data.frame(Terminos = subset(statsPOSSplit, select=c("keyword")),
                                ngram = subset(statsPOSSplit, select=c("ngram")),
                                Autor = c(rep("Orignial", nrow(statsPOSSplit))),
                                Fecha = c(rep(Sys.Date(), nrow(statsPOSSplit))),
                                Frecuencia = subset(statsPOSSplit, select=c("Frecuencia")),
                                tf_idf = c(rep("NA", nrow(statsPOSSplit))),
                                RAKE = c(rep("NA", nrow(statsPOSSplit))),
                                cValue = c(rep("NA", nrow(statsPOSSplit)))
      )
      
      terminology$keyword <- as.character(terminology$keyword)
      terminology$ngram <- as.integer(terminology$ngram)
      terminology$Frecuencia <- as.character(terminology$Frecuencia)
      terminology$tf_idf <- as.character(terminology$tf_idf)
      terminology$RAKE <- as.character(terminology$RAKE)
      terminology$cValue <- as.character(terminology$cValue)
      
    }
  } else {
    print("La terminología tiene que tener al menos una columna nombrada keywords")
    terminology = data.frame()
  }
  
  #Guardado de datos
  saveRDS(terminology, paste0(getwd(), "/data/corpus_data/", currentCorpus, "/processed/terminology/",termName,"/terminology.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", currentCorpus, "/processed/terminology/",termName,"/terminologyFull.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", currentCorpus, "/processed/terminology/",termName,"/terminologyChanges.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", currentCorpus, "/processed/terminology/",termName,"/terminologyExtracted.rds"))

}