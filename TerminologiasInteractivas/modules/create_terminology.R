#-------------------------------------------------------------
# Function: createCorpus
#
# Generates a new directory structure to allocate a corpus.
# Also generates required docs to being used by the system.
# ------------------------------------------------------------
# inputs:
#   - path_of_data_raw: Path to raw docs from wich generate the
#                       new corpus.
#   - nameCorpus: Name of the new corpus. A new folder will be created under <repo_dir>/data/corpus_data/<nameCorpus>.
#                                         The directory <repo_dir>/data/corpus_data MUST exist!
#   - NThreads: Number of threads to use in the process.
#
# outputs:
#   - Folder containing all required files to use the corpus in
#     the system.
#-------------------------------------------------------------

createTerminology <- function(tDocs, nameCorpus, nameTerm, nThreads, patr, paternType, idioma){
  tic()
  #Creacion de los directorios
  print("Creando Directorios: ")
  tic()
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/terminology/", nameTerm))
  toc()
  
  #Variables
  hilos = nThreads

  #Creacion de corpus quanteda
  # print("Extracción de textos del corpus: ")
  # tic()
  # tDocs <- texts(quancorpusDocs) #No tarda nada. 
  # toc()
  
            print(paste0(getwd(),"/data/corpus_data/",nameCorpus,"/processed/terminology/terminologyFull",idioma,".rds"))
  if(!file.exists(paste0(getwd(),"/data/corpus_data/",nameCorpus,"/processed/terminology/terminologyFull",idioma,".rds"))){
    #Descarga de modelo selecionado para la extraccion de terminos
    print("Descargando modelo: ")
    model <- udpipe_download_model(language = idioma) #Siempre la ultima version
    path <- model$file_model
    
    #Extracción de terminos
    print("Extrayendo Terminos: ")
    tic()
    x <- udpipe(tDocs, path, parallel.cores = hilos)
    toc()
    
    x$phrase_tag <- as_phrasemachine(x$upos, 
                                     type = "upos" #Puede ser tambiÃ©n "penn-treebank"
    )
    saveRDS(x, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminologyFull",idioma,".rds"))
  } else {
    x <<- readRDS(paste0(getwd(), "/data/corpus_data/",nameCorpus,"/processed/terminology/terminologyFull",idioma,".rds"))
  }
  #Extraccion de terminología segun patron
  print("Extrayendo Terminologia: ")
  tic()
  if(paternType == "upos"){
    print("Extraccion de terminos por documento empleando UPOS: ")
    tic()
    #Extraemos la terminología por documento--------------------------------------
    statsPOS2 <- data.frame()
    stats <- data.frame()
    split(x, x$doc_id) -> xSplit
    
    for (doc_id in xSplit) {
      statstTemp <- keywords_phrases(x = doc_id$upos, 
                                     term = tolower(doc_id$token), 
                                     pattern = patr,
                                     is_regex = TRUE, 
                                     detailed = FALSE 
                                     
      )
      
      cbind(rep(doc_id$doc_id[1], nrow(statstTemp)), statstTemp) -> statsPOS2
      
      colnames(statsPOS2)[1] <- "doc_id"
      
      rbind(statsPOS2, stats) -> stats
    }
    print("Extraido")
    #doc_id - keyword - ngram - freq
    
    #Calculamos la puntuación TF_IDF-------------------------------------------------
    #Esta función añade 3 columnas: tf, idf, tf_idf
    print("Puntuación tf_idf: ")
    stats <- stats %>%
      bind_tf_idf(keyword, doc_id, freq)
    
    #doc_id - keyword - ngram - freq - tf - idf - tf_idf
    
    #Agrupamos la terminologia en funcion del documento, generando una terminología completa del corpus entero.
    stats <- select(stats, -c(doc_id, tf, idf))
    stats <- aggregate(list(Frecuencia=stats$freq, tf_idf=stats$tf_idf), by=list(keyword=stats$keyword, ngram=stats$ngram), FUN=sum)
    toc()
    #keyword - ngram - freq - tf_idf
    #Calculamos la puntuación RAKE---------------------------------------------------
    print("Puntuación RAKE: ")
    tic()
    terminosPorSeparado <- strsplit(stats$keyword, split = " ")
    data.frame(splited = unlist(terminosPorSeparado)) -> terminosPorSeparado
    table(terminosPorSeparado) -> tablaPOS
    tablaPOS <- data.frame(tablaPOS)
    
    score = c()
    pb = txtProgressBar(min = 0, max = nrow(stats), initial = 0)#Prescindible
    for (keyIndex in 1:nrow(stats)) {
      
      if(stats[keyIndex, "ngram"] == 1){
        score <- append(score, 0)
      } else {
        
        keySplt <- strsplit(stats[keyIndex, "keyword"], " ")
        keyDegree <<- stats[keyIndex, "ngram"] - 1
        
        tempScore <- c()
        for(word in keySplt){
          
          tempScore <- append(tempScore, keyDegree / tablaPOS[tablaPOS$terminosPorSeparado %in% word, ]$Freq)
          
        }
        score <- append(score, sum(tempScore))
      }
      setTxtProgressBar(pb,keyIndex)#Prescindible
    }
    
    cbind(stats, score) -> stats
    colnames(stats)[5] <- "RAKE"
    toc()
    #Calculamos la puntuación c-value------------------------------------------------
    print("Puntuación c-value: ")
    tic()
    pb = txtProgressBar(min = 0, max = nrow(stats), initial = 0) #Prescindible
    
    cValue <- c()
    for (keyIndex in 1:nrow(stats)) {
      #Extraemos el termino candidato
      candidate <- stats[keyIndex, "keyword"]
      #Extraemos la frecuencia del candidato en el corpùs
      freqCandidate <- stats[keyIndex, "Frecuencia"]
      
      #Buscamos terminos que contengan a nuestro candidato. 
      coincidencias <- stats[grepl(candidate ,stats$keyword, fixed = TRUE), ]
      
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
    
    cbind(stats, cValue) -> stats
    colnames(stats)[6] <- "cvalue"
    
    toc()
    #El resultado tiene que ser una tabla con el termino, el autor, la fecha, ngram, la frecuencia, TF_IDF, RAKE y C-VALUE
    terminology <- data.frame(Terminos = subset(stats, select=c("keyword")),
                              ngram = subset(stats, select=c("ngram")),
                              Autor = c(rep("Orignial", nrow(stats))),
                              Fecha = c(rep(Sys.Date(), nrow(stats))),
                              Frecuencia = subset(stats, select=c("Frecuencia")),
                              tf_idf = subset(stats, select=c("tf_idf")),
                              RAKE = subset(stats, select=c("RAKE")),
                              cValue = subset(stats, select=c("cvalue"))
    )
  } else if(paternType == "pos"){
    print("Extraccion de terminos por documento empleando POS: ")
    tic()
    #Extraemos la terminología por documento--------------------------------------
    statsPOS2 <- data.frame()
    stats <- data.frame()
    split(x, x$doc_id) -> xSplit
    
    for (doc_id in xSplit) {
      statstTemp <- keywords_phrases(x = doc_id$phrase_tag, 
                                     term = tolower(doc_id$token), 
                                     pattern = patr,
                                     is_regex = TRUE, 
                                     detailed = FALSE 
                                     
      )
      
      cbind(rep(doc_id$doc_id[1], nrow(statstTemp)), statstTemp) -> statsPOS2
      
      colnames(statsPOS2)[1] <- "doc_id"
      
      rbind(statsPOS2, stats) -> stats
    }
    print("Extraido")
    toc()
    #doc_id - keyword - ngram - freq
    
    #Calculamos la puntuación TF_IDF-------------------------------------------------
    #Esta función añade 3 columnas: tf, idf, tf_idf
    print("Puntuación tf_idf: ")
    stats <- stats %>%
      bind_tf_idf(keyword, doc_id, freq)
    
    #doc_id - keyword - ngram - freq - tf - idf - tf_idf
    
    #Agrupamos la terminologia en funcion del documento, generando una terminología completa del corpus entero.
    stats <- select(stats, -c(doc_id, tf, idf))
    stats <- aggregate(list(Frecuencia=stats$freq, tf_idf=stats$tf_idf), by=list(keyword=stats$keyword, ngram=stats$ngram), FUN=sum)
    toc()
    #Calculamos la puntuación RAKE---------------------------------------------------
    print("Puntuación RAKE: ")
    tic()
    terminosPorSeparado <- strsplit(stats$keyword, split = " ")
    data.frame(splited = unlist(terminosPorSeparado)) -> terminosPorSeparado
    table(terminosPorSeparado) -> tablaPOS
    tablaPOS <- data.frame(tablaPOS)
    
    score = c()
    pb = txtProgressBar(min = 0, max = nrow(stats), initial = 0)#Prescindible
    for (keyIndex in 1:nrow(stats)) {
      
      if(stats[keyIndex, "ngram"] == 1){
        score <- append(score, 0)
      } else {
        
        keySplt <- strsplit(stats[keyIndex, "keyword"], " ")
        keyDegree <<- stats[keyIndex, "ngram"] - 1
        
        tempScore <- c()
        for(word in keySplt){
          
          tempScore <- append(tempScore, keyDegree / tablaPOS[tablaPOS$terminosPorSeparado %in% word, ]$Freq)
          
        }
        score <- append(score, sum(tempScore))
      }
      setTxtProgressBar(pb,keyIndex)#Prescindible
    }
    
    cbind(stats, score) -> stats
    colnames(stats)[5] <- "RAKE"
    toc()
    #Calculamos la puntuación c-value------------------------------------------------
    print("Puntuación c-value: ")
    tic()
    pb = txtProgressBar(min = 0, max = nrow(stats), initial = 0) #Prescindible
    
    cValue <- c()
    for (keyIndex in 1:nrow(stats)) {
      #Extraemos el termino candidato
      candidate <- stats[keyIndex, "keyword"]
      #Extraemos la frecuencia del candidato en el corpùs
      freqCandidate <- stats[keyIndex, "Frecuencia"]
      
      #Buscamos terminos que contengan a nuestro candidato. 
      coincidencias <- stats[grepl(candidate ,stats$keyword, fixed = TRUE), ]
      
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
    
    cbind(stats, cValue) -> stats
    colnames(stats)[6] <- "cvalue"
    
    toc()
    #El resultado tiene que ser una tabla con el termino, el autor, la fecha, ngram, la frecuencia, TF_IDF, RAKE y C-VALUE
    terminology <- data.frame(Terminos = subset(stats, select=c("keyword")),
                              ngram = subset(stats, select=c("ngram")),
                              Autor = c(rep("Orignial", nrow(stats))),
                              Fecha = c(rep(Sys.Date(), nrow(stats))),
                              Frecuencia = subset(stats, select=c("Frecuencia")),
                              tf_idf = subset(stats, select=c("tf_idf")),
                              RAKE = subset(stats, select=c("RAKE")),
                              cValue = subset(stats, select=c("cvalue"))
                              )
    
  } else {
    
    print("Extraccion de terminos por documento: ")
    tic()
    stats <<- keywords_rake(x = x, 
                            term = "lemma", 
                            group = "doc_id", 
                            relevant = x$upos %in% c("NOUN", "ADJ")
    )
    
    #keyword - ngram - freq - rake
    toc()
    tic()
    #Calculamos la puntuación c-value------------------------------------------------
    print("Puntuación c-value: ")
    tic()
    pb = txtProgressBar(min = 0, max = nrow(stats), initial = 0) #Prescindible
    
    cValue <- c()
    for (keyIndex in 1:nrow(stats)) {
      #Extraemos el termino candidato
      candidate <- stats[keyIndex, "keyword"]
      #Extraemos la frecuencia del candidato en el corpùs
      freqCandidate <- stats[keyIndex, "freq"]
      
      #Buscamos terminos que contengan a nuestro candidato. 
      coincidencias <- stats[grepl(candidate ,stats$keyword, fixed = TRUE), ]
      
      #Numero de coincidencias
      ncoincidencias <- nrow(coincidencias)
      if(ncoincidencias == 1){
        #El candidato no esta contenido en otro termino
        #Calculamos c-value
        res <- log2(length(strsplit(candidate, " ")[[1]])) * freqCandidate
        #Almacenamos el resultado
        append(cValue, res) -> cValue 
      } else {
        sumatorio <- sum(coincidencias$freq)
        res <- log2(length(strsplit(candidate, " ")[[1]])) * freqCandidate - (1 / ncoincidencias) * sumatorio
        #Almacenamos el resultado
        append(cValue, res) -> cValue 
      }
      setTxtProgressBar(pb,keyIndex)#Prescindible
    }
    
    cbind(stats, cValue) -> stats
    colnames(stats)[5] <- "cvalue"
    
    toc()
    #El resultado tiene que ser una tabla con el termino, el autor, la fecha, ngram, la frecuencia, TF_IDF, RAKE y C-VALUE
    terminology <- data.frame(Terminos = subset(stats, select=c("keyword")),
                              ngram = subset(stats, select=c("ngram")),
                              Autor = c(rep("Orignial", nrow(stats))),
                              Fecha = c(rep(Sys.Date(), nrow(stats))),
                              Frecuencia = subset(stats, select=c("freq")),
                              tf_idf = c(rep("NA", nrow(stats))),
                              RAKE = subset(stats, select=c("rake")),
                              cValue = subset(stats, select=c("cvalue"))
    )
  }
  toc()
  
  #Guardado de datos
  saveRDS(terminology, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminology.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminologyChanges.rds"))
  saveRDS(stats, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminologyExtracted.rds"))
  
  print("¡Terminología creada con exito!")
  toc()
}