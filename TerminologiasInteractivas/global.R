library(shiny)
library(readtext)
library(tidyverse)
library(quanteda)
library(DT)
library(devtools)
library(profvis)
library(rlist)
library(png)
library(magick)
library(pdftools)
library(extrafont)
library(tictoc)
library(data.table)
library(zoo)
library(shinyWidgets)
library(shinyjs)
library(tools)
library(sos)
library(dplyr)
library(plyr)
library(shinybusy)
library(tidytext)
library(stringr)
library(doParallel)
library(pbapply)

library(shi18ny)


library(udpipe)


setwd("~/GitHub/TFM/TerminologiasInteractivas")

source(paste0(getwd(), "/modules/get_document_path.R"))
source(paste0(getwd(), "/modules/create_corpus.R"))
source(paste0(getwd(), "/modules/create_terminology.R"))
source(paste0(getwd(), "/modules/add_documents.R"))
source(paste0(getwd(), "/modules/upload_terminology.R"))

hilos = detectCores()
emptyCorpus <- FALSE

# Read config file
config_file <- readLines("config_file.txt") 
mode <- substr(config_file[3], 7, nchar(config_file[3]))

currentCorpus <- substr(config_file[5], 9, nchar(config_file[5]))
path <<- paste0(getwd(), "/data/corpus_data/")
corpusPath <- paste0(getwd(), "/data/corpus_data/", currentCorpus)

corpusList <- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/"), recursive = FALSE))
termList <- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/",currentCorpus,"/processed/terminology/"), recursive = FALSE))
currentTerm <- termList[1]

statistics <- list()


tableTerms <<- readRDS(paste0(corpusPath, "/processed/terminology/",currentTerm ,"/terminology.rds"))
listChangesTerms <<- readRDS(paste0(corpusPath, "/processed/terminology/",currentTerm ,"/terminologyChanges.rds"))
dtMetadata <<- readRDS(paste0(corpusPath, "/processed/corpus/metadata.rds"))
corp <<- readRDS(paste0(corpusPath, "/processed/corpus/corpus.rds"))

corpSize <- object.size(corp)
#tokens <- tokens(corp)
ntokens <- ntoken(corp)

termsList <<- tableTerms$Terminos

reactiveTerm <<- reactiveValues(data = tableTerms)
reactiveListTerm <<- reactiveValues(data = termsList)
reactiveCorpusList <<- reactiveValues(data = corpusList)
reactiveTermList <<- reactiveValues(data = termList)
reactiveCurrentCorpus <<- reactiveValues(data = currentCorpus)
reactiveCurrentTerm <<- reactiveValues(data = currentTerm)

# reactivePaternChoices <<- reactiveValues(data = c())

#quantokSession <- corpusTokens
#quantokPagesSession <- corpusPagesTokens
statisticsSession <- statistics
corpusPathSession <- corpusPath
