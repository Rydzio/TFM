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

createCorpus <- function(ruta, nameCorpus, nThreads, encoding){
  
  #Creacion de los directorios
  print("Creando Directorios: ")
  tic()
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/corpus"))
  dir.create(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed", "/terminology"))
  toc()
  
  #Variables
  hilos = nThreads
  print(ruta)
  
  #Extracción de metadatos
  print("Extrayendo metadatos: ")
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
    filepath <- strsplit(x, "/")
    fname <- filepath[[1]][length(filepath[[1]])]
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
    documentInfo <- data.frame("Nombre" = gsub("_", " ", fname), "Pag" = toString(pages) , "Creacion" = as.character(created), "Modif" = as.character(modified) , "Datos" = toString(unlist(key)), stringsAsFactors = FALSE)
    metadata <- rbind(metadata,documentInfo) 
  }
  cbind(sizes$size, metadata) -> metadata
  colnames(metadata)[1] <- "TamañoMB"
  toc()
  
  #Lectura de documentos
  print("Leyendo Documentos: ")
  print(ruta)
  tic()
  if(encoding == "Default"){
    docs <- readtext(paste0(ruta, "*"), #Leo todo lo que tenga ese path
                     #docvarsfrom = "filenames", 
                     #docvarnames = c("document", "language"),
                     #dvsep = "_", 
                     #encoding = "UTF-8-BOM", #"ISO-8859-1", #Casi mejor no pongo nada porque no sÃ© el encoding
                     verbosity = 0)
  } else {
    print(encoding)
    docs <- readtext(paste0(ruta, "*"), #Leo todo lo que tenga ese path
                     #docvarsfrom = "filenames", 
                     #docvarnames = c("document", "language"),
                     #dvsep = "_", 
                     encoding = encoding,
                     verbosity = 0)
  }
  toc()
  
  #Creacion de corpus quanteda
  print("Creando Corpus: ")
  tic()
  quanteda_options(threads = hilos)
  quancorpusDocs <- corpus(docs)
  toc()

  #Guardado de datos
  saveRDS(metadata, paste0(getwd(),"/data/corpus_data/" ,nameCorpus,"/processed/corpus/metadata.rds"))
  saveRDS(quancorpusDocs, paste0(getwd(),"/data/corpus_data/" ,nameCorpus,"/processed/corpus/corpus.rds"))
  
  print("¡corpus creado con exito!")
}