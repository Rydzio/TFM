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

library(udpipe)


setwd("~/TFM/TerminologiasInteractivas")

source(paste0(getwd(), "/modules/get_document_path.R"))
source(paste0(getwd(), "/modules/create_corpus.R"))
source(paste0(getwd(), "/modules/add_documents.R"))

hilos = 8

# Read config file
config_file <- readLines("config_file.txt") 
mode <- substr(config_file[3], 7, nchar(config_file[3]))

corpusList <- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/"), recursive = FALSE))

currentCorpus <- substr(config_file[5], 9, nchar(config_file[5]))
corpusPath <- paste0(getwd(), "/data/corpus_data/", currentCorpus)

statistics <- list()
#corpus <- readRDS(paste0(corpusPath, "/processed/corpus/corpus.rds"))
#corpusTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusTokens.rds"))
#corpusPagesTokens <- readRDS(paste0(corpusPath, "/processed/corpus/corpusPagesTokens.rds"))
tableTerms <<- readRDS(paste0(corpusPath, "/processed/terminology/terminology.rds"))
listChangesTerms <<- readRDS(paste0(corpusPath, "/processed/terminology/terminologyChanges.rds"))
dtMetadata <<- readRDS(paste0(corpusPath, "/processed/corpus/metadata.rds"))
corp <<- readRDS(paste0(corpusPath, "/processed/corpus/corpus.rds"))

#documents <- corpusPagesTokens
# indexPage <- regexpr(pattern = '@Page', documents, fixed = TRUE)
# documents <- substring(documents, 1, indexPage)
# documents <- substring(documents, 1, nchar(documents)-1)
# uniqueDocuments <- unique(documents)
# 
# #tokens <- as.list(corpusTokens)
# #numberTokens <- sum(unlist(lapply(tokens, length)))
# 
# statistics["NumberDocuments"] <- as.character(length(uniqueDocuments))

#statistics["NumberPages"] <- summary(corpusPagesTokens)[1]

#statistics["NumberTokens"] <- as.character(numberTokens)

termsList <<- tableTerms$Terminos

reactiveTerm <<- reactiveValues(data = tableTerms)
reactiveListTerm <<- reactiveValues(data = termsList)
reactiveCorpusList <<- reactiveValues(data = corpusList)
reactiveCurrentCorpus <<- reactiveValues(data = currentCorpus)

#quantokSession <- corpusTokens
#quantokPagesSession <- corpusPagesTokens
statisticsSession <- statistics
corpusPathSession <- corpusPath
