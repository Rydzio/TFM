#-------------------------------------------------------------
# Function: addDocuments
#
# Add documents to a corpus.
# ------------------------------------------------------------
# inputs:
#   - nameCorpus: name of the corpus where to enter docs.
#   - pathFiles: path where the documents are located.
#
# outputs:
#   - updated corpus
#-------------------------------------------------------------

addDocuments <- function(nameCorpus, pathFiles) {
  corpus <- readRDS(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed/corpus/corpus.rds"))
  corpusTokens <- readRDS(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed/corpus/corpusTokens.rds"))
  corpusPagesTokens <- readRDS(paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed/corpus/corpusPagesTokens.rds"))
  
  lastPosCorpus <- as.integer(length(corpusTokens)) + 1
  lastPosCorpusPages <- as.integer(summary(corpusPagesTokens)[1]) + 1
  
  corpus_raw = data.frame()
  files <- list.files(path=pathFiles, full.names = TRUE, recursive = TRUE)
  
  posindex = lastPosCorpus
  lapply(files, function(x){
    
    filepath <- strsplit(x, "/")
    fname <- filepath[[1]][length(filepath[[1]])]
    cat(fname, "\n")
    
    pdf_text(paste(x,sep = "")) -> document_text
    
    pageindex = 1
    
    lapply(document_text, function(y){
      
      document <- data.frame("title" = fname, "page" = paste0("Page", pageindex), "pos" = paste0("Pos", posindex), "text" = toString(y), stringsAsFactors = FALSE)
      pageindex <<- pageindex + 1
      posindex <<- posindex + 1
      
      colnames(document) <- c("title", "page", "pos", "text")
      corpus_raw <<- rbind(corpus_raw,document) 
    })
  })
  
  quanteda_options(threads = 4)
  corpusAdd <- corpus(corpus_raw)
  doc_id <- paste(corpus_raw$title, corpus_raw$page, corpus_raw$pos, sep = "@")
  docnames(corpusAdd) <- doc_id
  
  newCorpus <- corpus + corpusAdd
  
  toToken <- ""
  
  for(pageName in doc_id) {
    pageName <- gsub(" ", "", pageName)
    pageName <- gsub(".pdf", "", pageName, ignore.case = TRUE)
    toToken <<- paste(toToken, pageName, separator = " ")
  }
  
  corpusPagesAdd <- corpus(toToken)
  doc_id2 <- paste("Corpus")
  docnames(corpusPagesAdd) <- doc_id2
  
  newCorpusTokens <- tokens(newCorpus)
  # newCorpusPagesTokens <- tokens(newCorpusPages)
  newCorpusPagesTokens <- c(corpusPagesTokens, doc_id)
  
  print(as.integer(length(newCorpusTokens)))
  print(as.integer(summary(newCorpusPagesTokens)[1]))
  
  saveRDS(corpus, paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed/corpus/corpus.rds"))
  saveRDS(corpusTokens, paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed/corpus/corpusTokens.rds"))
  saveRDS(corpusPagesTokens, paste0(getwd(),"/data/corpus_data/", nameCorpus, "/processed/corpus/corpusPagesTokens.rds"))
}