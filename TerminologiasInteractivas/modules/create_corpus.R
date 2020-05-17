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

createCorpus <- function(ruta, nameCorpus, nThreads, patr, paternType, idioma, imagesFlag = TRUE){
  
  print("Creando Directorios: ")
  
  tic()
  #Creacion de directorios para el corpus
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/raw"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/corpus"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/terminology"))
  toc()
  
  #Variables
  hilos = nThreads
  
  print("Extrayendo metadatos: ")
  print(ruta)
  
  tic()
  files <- list.files(ruta, full.names = TRUE, recursive = TRUE)
  sizes <- file.info(files)
  sizes <- sizes %>% select(size)
  
  sizes[] <- lapply(sizes, function(x){
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  sapply(sizes, class)
  
  sizes$size / 1000000 -> sizes$size
  
  metadata <- data.frame()
  
  for (x in files){
    #Obtenemos el nombre del documento
    filepath <- strsplit(x, "/")
    fname <- filepath[[1]][length(filepath[[1]])]
    cat(fname, "\n")
    #Leemos el documento (Lista de paginas (identificador), y su texto asociado)
    if(file_ext(fname) == "pdf" || file_ext(fname) == "PDF"){
      pdf_info(paste(x,sep = "")) -> info
      
      info$pages -> pages
      info$created -> created
      info$modified -> modified
      info$key -> key
      
    }else{
      pages = "NA"
      created = "NA"
      modified = "NA"
      key = "NA"
    }
    
    documentInfo <- data.frame("Nombre" = fname, "Pag" = toString(pages) , "Creacion" = as.character(created), "Modif" = as.character(modified) , "Datos" = toString(unlist(key)), stringsAsFactors = FALSE)
    
    metadata <- rbind(metadata,documentInfo) 
    
  }
  
  cbind(sizes$size, metadata) -> metadata
  
  colnames(metadata)[1] <- "TamañoMB"
  
  saveRDS(metadata, paste0(getwd(),"/data/corpus_data/" ,nameCorpus,"/processed/corpus/metadata.rds"))
  toc()
  
  print("Leyendo Documentos: ")
  tic()
  docs <- readtext(paste0(ruta, "*"), #Leo todo lo que tenga ese path
                   #docvarsfrom = "filenames", 
                   #docvarnames = c("document", "language"),
                   #dvsep = "_", 
                   #encoding = "UTF-8-BOM", #"ISO-8859-1", #Casi mejor no pongo nada porque no sÃ© el encoding
                   verbosity = 0) 
  toc()
  
  print("Creando Corpus: ")
  # create quanteda corpus
  tic()
  quanteda_options(threads = hilos)
  quancorpusDocs <- corpus(docs)
  toc()
  
  #Puedo sacar los textos 
  tDocs <- texts(quancorpusDocs) #No tarda nada. 
  
  model <- udpipe_download_model(language = idioma)
  #udmodel_spanish_gsd <- udpipe_load_model(file = 'spanish-gsd-ud-2.4-190531.udpipe')
  
  path <- model$file_model
  
  print("Extrayendo Terminos: ")
  tic()
  x <- udpipe(tDocs, path, parallel.cores = hilos)
  toc()
  
  x$phrase_tag <- as_phrasemachine(x$upos, 
                                   type = "upos" #Puede ser tambiÃ©n "penn-treebank"
  )
  
  #terminology <- data.frame(Terminos = subset(x, select=c("token")), Autor = c(rep("Orignial", nrow(x))), Fecha = c(rep(Sys.Date(), nrow(x))))
  #terminology <- unique(terminology)
  
  print("Extrayendo Terminología: ")
  tic()
  if(paternType == "upos"){
    stats <<- keywords_phrases(x = x$upos, 
                               term = tolower(x$token), 
                               pattern = patr,
                               is_regex = TRUE, 
                               detailed = TRUE #logical indicating to return the exact positions where the phrase was found (set to TRUE) or just how many times each phrase is occurring (set to FALSE). Defaults to TRUE.
    )
  } else if(paternType == "pos"){
    stats <<- keywords_phrases(x = x$phrase_tag, 
                               term = tolower(x$token), 
                               pattern = patr,
                               is_regex = TRUE,
                               detailed = TRUE #logical indicating to return the exact positions where the phrase was found (set to TRUE) or just how many times each phrase is occurring (set to FALSE). Defaults to TRUE.
    )
  } else {
    stats <<- keywords_rake(x = x, 
                           term = "lemma", 
                           group = "doc_id", 
                           relevant = x$upos %in% c("NOUN", "ADJ"))
  }
  
  terminology <- data.frame(Terminos = subset(stats, select=c("keyword")), Autor = c(rep("Orignial", nrow(stats))), Fecha = c(rep(Sys.Date(), nrow(stats))))
  
  terminology <- ddply(terminology, .(keyword, Autor, Fecha), nrow)
  colnames(terminology)[4] <- "Frecuencia"
  toc()

  saveRDS(quancorpusDocs, paste0(getwd(),"/data/corpus_data/" ,nameCorpus,"/processed/corpus/corpus.rds"))
  saveRDS(terminology, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminology.rds"))
  saveRDS(x, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminologyFull.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminologyChanges.rds"))
  saveRDS(stats, paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminologyExtracted.rds"))
  
  print("FIN")
  
}