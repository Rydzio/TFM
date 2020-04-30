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

createCorpus <- function(path_of_data_raw, nameCorpus, nThreads, imagesFlag = TRUE){
  
  #Creacion de directorios para el corpus
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/raw"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/corpus"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/doc_images"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/terminology"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminology.rds"))
  saveRDS(data.frame(), paste0(getwd(), "/data/corpus_data/", nameCorpus, "/processed/terminology/terminologyChanges.rds"))
  
  #Variables
  corpus_raw = data.frame()
  hilosDeProcesamiento = nThreads
  
  # Listamos todos los documentos de la ruta de los datos
  files <- list.files(path=path_of_data_raw, 
                      pattern = glob2rx("*.pdf"), #Solo pdfs. En el directorio puede haber otras cosas ;-)
                      full.names = TRUE, 
                      recursive = TRUE)
  
  #Generamos los metadatos de los documentos para almacenarlos en las estadisticas (unos 2 segundos)
  print("getting pdf metadata... ")
  file.info(files) -> sizes
  sizes %>% select(size) -> sizes
  sizes[] <- lapply(sizes, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  sapply(sizes, class)
  
  sizes %>%  rename(
    TamañoMB = size,
  ) -> sizes
  
  sizes$TamañoMB / 1000000 -> sizes$TamañoMB
  metadata <- data.frame()
  
  posindex = 1
  print("Reading documents")
  #Bucle por cada documentos
  for (x in files){
    #Obtenemos el nombre del documento
    filepath <- strsplit(x, "/")
    fname <- filepath[[1]][length(filepath[[1]])]
    cat(fname, "\n")
    #Leemos el documento (Lista de paginas (identificador), y su texto asociado)
    pdf_text(paste(x,sep = "")) -> document_text
    
    
    
    pageindex = 1
    #Por cada pagina del documento
    for (y in document_text){
      #añadimos los identificadores de cada pagina de un documento, y su texto asocidado en un string
      document <- data.frame("title" = fname, "page" = paste0("Page", pageindex), "pos" = paste0("Pos", posindex), "text" = toString(y), stringsAsFactors = FALSE)
      pageindex <- pageindex + 1
      posindex <- posindex + 1
      
      colnames(document) <- c("title", "page", "pos", "text")
      #Añadimos la información al corpus que contiene todos los datos leidos hasta ahora
      corpus_raw <- rbind(corpus_raw,document) 
      
    }
    #mETADATOS E INFORMACIÓN DE LOS PDF
    pdf_info(paste(x,sep = "")) -> info
    unlist(info$key) -> unlisted
    documentInfo <- data.frame("Nombre" = fname, "Pag" = info$pages , "Creacion" = info$created, "Modif" = info$modified , "Datos" = toString(unlist(info$key)), stringsAsFactors = FALSE)
    metadata <<- rbind(metadata,documentInfo) 
    
    cat("Tamaño del corpus: ",object.size(corpus_raw)/1000000," MB \n")
  }
  print("Building teh metadata from corpus")
  cbind(sizes$TamañoMB, metadata) -> metadata
  metadata %>%  rename(
    TamañoMB = 'sizes$TamañoMB',
  ) -> metadata
  
  saveRDS(metadata, paste0(getwd(),"/data/corpus_data" ,path_of_data,"/processed/corpus/metadata.rds"))
  
  
  print("Making corpus from data using quanteda...")
  quanteda_options(threads = hilosDeProcesamiento)
  corpus <- corpus(corpus_raw)
  doc_id <- paste(corpus_raw$title, corpus_raw$page, corpus_raw$pos, sep = "@")
  docnames(corpus) <- doc_id
  
  corpusTokens <- tokens(corpus)
  corpusPagesTokens <- doc_id
  
  saveRDS(corpus, paste0(getwd(),"/data/corpus_data/" , nameCorpus, "/processed/corpus/corpus.rds"))
  saveRDS(corpusTokens, paste0(getwd(),"/data/corpus_data/" , nameCorpus, "/processed/corpus/corpusTokens.rds"))
  saveRDS(corpusPagesTokens, paste0(getwd(),"/data/corpus_data/" , nameCorpus, "/processed/corpus/corpusPagesTokens.rds"))
  
  if(imagesFlag == TRUE) {
    #PARTE 2: GENERA IMAGENES Y METADATOS DE CADA PDF
    cat("Generando metadatos e imagenes \n")
    
    posindex = 1
    for (x in files){
      
      info <- pdf_info(x)
      numPags <- info$pages
      
      txtboxes <- pdf_data(x)
      
      filepath <- strsplit(x, "/")
      fname <- filepath[[1]][length(filepath[[1]])]
      print(fname)
      fname <- substring(fname, 1, nchar(fname)-4)
      
      dir.create(paste0(getwd(),"/data/corpus_data/" , nameCorpus, "/processed/doc_images/", fname))
      saveRDS(txtboxes, paste0(getwd(),"/data/corpus_data/" , nameCorpus, "/processed/doc_images/", fname, "/", fname, ".rds"))
      
      counterPage <- 1
      while(counterPage <= numPags) {
        bitmap <- pdf_render_page(x,
                                  dpi = 90,
                                  page = counterPage)
        png::writePNG(bitmap,
                      paste0(getwd(),"/data/corpus_data/" , nameCorpus, "/processed/doc_images/", fname, "/page", as.character(counterPage), ".png"))
        counterPage <- counterPage + 1
      }
    }
  }
}