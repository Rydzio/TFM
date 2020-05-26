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

createTerminology <- function(quancorpusDocs, nameCorpus, nameTerm, nThreads, patr, paternType, idioma){
  
  print(nameCorpus)
  print(nameTerm)
  print(nThreads)
  print(patr)
  print(paternType)
  print(idioma)
  
  #Creacion de los directorios
  print("Creando Directorios: ")
  tic()
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/terminology/", nameTerm))
  toc()
  
  #Variables
  hilos = nThreads

  #Creacion de corpus quanteda
  print("Extracción de textos del corpus: ")
  tic()
  tDocs <- texts(quancorpusDocs) #No tarda nada. 
  toc()
  
  #Descarga de modelo selecionado para la extraccion de terminos
  print("Descargando modelo: ")
  model <- udpipe_download_model(language = idioma)
  path <- model$file_model
  
  #Extracción de terminos
  print("Extrayendo Terminos: ")
  tic()
  x <- udpipe(tDocs, path, parallel.cores = hilos)
  toc()
  
  x$phrase_tag <- as_phrasemachine(x$upos, 
                                   type = "upos" #Puede ser tambiÃ©n "penn-treebank"
  )
  
  #Extraccion de terminología segun patron
  print("Extrayendo Terminologia: ")
  tic()
  if(paternType == "upos"){
    stats <<- keywords_phrases(x = x$upos, 
                               term = tolower(x$token), 
                               pattern = patr,
                               is_regex = TRUE, 
                               detailed = TRUE #logical indicating to return the exact positions where the phrase was found (set to TRUE) or just how many times each phrase is occurring (set to FALSE). Defaults to TRUE.
    )
    terminology <- data.frame(Terminos = subset(stats, select=c("keyword")), Autor = c(rep("Orignial", nrow(stats))), Fecha = c(rep(Sys.Date(), nrow(stats))))
    terminology <- ddply(terminology, .(keyword, Autor, Fecha), nrow)
    colnames(terminology)[4] <- "Frecuencia"
  } else if(paternType == "pos"){
    stats <<- keywords_phrases(x = x$phrase_tag, 
                               term = tolower(x$token), 
                               pattern = patr,
                               is_regex = TRUE,
                               detailed = TRUE #logical indicating to return the exact positions where the phrase was found (set to TRUE) or just how many times each phrase is occurring (set to FALSE). Defaults to TRUE.
    )
    terminology <- data.frame(Terminos = subset(stats, select=c("keyword")), Autor = c(rep("Orignial", nrow(stats))), Fecha = c(rep(Sys.Date(), nrow(stats))))
    terminology <- ddply(terminology, .(keyword, Autor, Fecha), nrow)
    colnames(terminology)[4] <- "Frecuencia"
  } else {
    stats <<- keywords_rake(x = x, 
                            term = "lemma", 
                            group = "doc_id", 
                            relevant = x$upos %in% c("NOUN", "ADJ"))
    terminology <- data.frame(Terminos = subset(stats, select=c("keyword", "freq")), Autor = c(rep("Orignial", nrow(stats))), Fecha = c(rep(Sys.Date(), nrow(stats))))
    terminology <- terminology[, c(1, 3, 4, 2)]
    colnames(terminology)[1] <- "keyword"
    colnames(terminology)[4] <- "Frecuencia"
  }
  
  toc()
  
  #Guardado de datos
  saveRDS(terminology, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminology.rds"))
  saveRDS(x, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminologyFull.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminologyChanges.rds"))
  saveRDS(stats, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/",nameTerm,"/terminologyExtracted.rds"))
  
  print("¡Terminología creada con exito!")
}