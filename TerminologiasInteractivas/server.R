library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(waiter)
library(rsvg)
library(svgPanZoom)

function(input, output, session) {

  result <- NULL
  currentDoc <- NULL
  currentPage <- NULL
  
  allowContains <- FALSE
  
  w <- Waiter$new(
    id = c("pdf", "noResultIm"),
    html = spin_3k(), 
    color = "transparent"
  )
  
  shinyDirChoose(input, 'dir', roots = getVolumes()())#c(wd = '../../'))#
  
  observeEvent(input$allowContains, {
    allowContains <<- input$allowContains
  })
  
  #Crear Corpus
  observeEvent(input$dirCreate, {
    show_modal_spinner(text = "Creando Corpus...")
    if(input$nameCorp == "") {
      nameCorpus <- paste0("Corpus", as.character(sample(1:100000, 1)))
    } 
    else {
      nameCorpus <- input$nameCorp
    }
    directorio <- input$dir
    pathDirectorio <- as.character(input$dir)
    if(pathDirectorio != "1") {
      listaDirectorios <- directorio$path
      listaDirectorios <- listaDirectorios[2:length(listaDirectorios)]
      path <- directorio$root
      print(path)
      path <- substring(path, regexpr(pattern = '(', path, fixed = TRUE)+1, regexpr(pattern = ')', path, fixed = TRUE)-1)
      
      for(directory in listaDirectorios) {
        path <- paste0(path, "/", directory)
      }
      createCorpus(path, nameCorpus, hilos, input$encoding)
      corpusList <<- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/"), recursive = FALSE))
      reactiveCorpusList$data <<- corpusList
      
      updateSelectInput(session, "corpusForTerm",
                        label = NULL,
                        choices = corpusList
      )
      
    }
    remove_modal_spinner()
  })
  
  #Crear Terminología---------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$dirCreateTerm, {
    show_modal_spinner(text = "Creando Terminología...")
    if(input$nameTerm == "") {
      nameTerm <- paste0("Term", as.character(sample(1:100000, 1)))
    } 
    else {
      nameTerm <- input$nameTerm
    }
    
    print(nameTerm)
    
    corpForExtractingTerm <<- readRDS(paste0(path, input$corpusForTerm,"/processed/corpus/corpus.rds"))
    createTerminology(corpForExtractingTerm, input$corpusForTerm, nameTerm, hilos, input$patern, input$paternType, input$idioma)
    
    if(input$corpusOpt == input$corpusForTerm){
      termList <<- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/",input$corpusForTerm,"/processed/terminology"), recursive = FALSE))
      
      updateRadioGroupButtons(session, 
                              "termOpt", 
                              label = NULL, 
                              choices = termList
      )
      
      updateSelectInput(session, "TermForDownload",
                        choices = termList)
      }
    #termList <<- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/",input$corpusOpt,"/processed/terminology"), recursive = FALSE))
    
    if(length(termList) == 1){
      print("bingo!")
      
      currentTerm <<- nameTerm
      
      tableTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      listChangesTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyChanges.rds"))
      
      dtTermFull <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyFull.rds"))
      dtTermExtracted <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyExtracted.rds"))
      
      emptyCorpus <<- FALSE
    }
    
    remove_modal_spinner()
  })
  
  #Borrar corpus---------------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$corpusDelBut, {
    if(input$corpusDelText == "") {
      showModal(
        modalDialog(
          renderText({
            paste0("No ha escrito el nombre de ningún corpus.")
          })
        ))
    } 
    else {
      if(input$corpusDelText == isolate(reactiveCurrentCorpus$data)) {
        showModal(
          modalDialog(
            renderText({
              paste0("No puede eliminar el corpus que tiene en uso actualmente, primero cambie de corpus.")
            })
          ))
      }
      else {
        if(input$corpusDelText %in% corpusList) {
          unlink(paste0(getwd(), "/data/corpus_data/", input$corpusDelText), recursive = TRUE)
          corpusList <<- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/"), recursive = FALSE))
          reactiveCorpusList$data <<- corpusList
        }
        else {
          showModal(
            modalDialog(
              renderText({
                paste0("El corpus que ha escrito no existe.")
              })
            ))
        }
      }
    }
  })
  
  #corpus -------------------------------------------------------------------------------------------------------------------------
  observe( {
    newCorpusList <- reactiveCorpusList$data
    updateRadioGroupButtons(session, 'corpusOpt', choices = newCorpusList, selected = isolate(reactiveCurrentCorpus$data))
  }
  )
  
  observe( {
    newCorpusList <- isolate(reactiveCorpusList$data)
    updateRadioGroupButtons(session, 'corpusOpt', choices = newCorpusList, selected = reactiveCurrentCorpus$data)
  }
  )
  
  #Borrar Terminología---------------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$termDelBut, {
    print(input$termDelText)
    print(termList)
    
    if(input$termDelText == "") {
      showModal(
        modalDialog(
          renderText({
            paste0("No ha escrito el nombre de ninguna terminología.")
          })
        ))
    } 
    else {
      if(input$termDelText == isolate(reactiveCurrentTerm$data)) {
        showModal(
          modalDialog(
            renderText({
              paste0("No puede eliminar la terminología que tiene en uso actualmente, primero cambie de Terminología.")
            })
          ))
      }
      else {
        if(input$termDelText %in% termList) {
          print(paste0(getwd(), "/data/corpus_data/",currentCorpus,"/processed/terminology/",input$termDelText))
          unlink(paste0(getwd(), "/data/corpus_data/",currentCorpus,"/processed/terminology/",input$termDelText), recursive = TRUE)
          termList <<- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/", currentCorpus,"/processed/terminology/"), recursive = FALSE))
          reactiveTermList$data <<- termList
          
          print(termList)
          
          updateRadioGroupButtons(session, 'termOpt', label = NULL, choices = termList, selected = termList[1])
          updateSelectInput(session, "TermForDownload",
                            choices = termList,
                            selected = termList[1])
        }
        else {
          showModal(
            modalDialog(
              renderText({
                paste0("La terminología que ha escrito no existe.")
              })
            ))
        }
      }
    }
  })
  # 
  # #tereminologia -------------------------------------------------------------------------------------------------------------------------
  # observe( {
  #   newTermList <- reactiveTermList$data
  #   updateRadioGroupButtons(session, 'termOpt', choices = newTermList, selected = isolate(reactiveTermList$data))
  # }
  # )
  # 
  # observe( {
  #   newTermList <- isolate(reactiveTermList$data)
  #   updateRadioGroupButtons(session, 'termOpt', choices = newTermList, selected = reactiveTermList$data)
  # }
  # )
  # 
  
  #Descargar corpus -----------------------------------------------------------------------------------------------------------------------
  output$downloadDataCorpus <- downloadHandler(

    filename = function() {
      paste(input$CorpusForDownload, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data <- readRDS(paste0(path, input$CorpusForDownload,"/processed/corpus/corpus.rds"))
      data <- texts(data)
      write.csv(data, file)
    }
  )
  
  #Descargar terminología -----------------------------------------------------------------------------------------------------------------------
  output$downloadDataTerm <- downloadHandler(
    
    filename = function() {
      paste(currentCorpus,"-", input$TermForDownload, "-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      print(input$TermForDownload)
      data <- readRDS(paste0(corpusPathSession, "/processed/terminology/",input$TermForDownload,"/terminology.rds"))
      write.csv(data, file)
    }
  )

  #Cambiar de terminología -------------------------------------------------------------------------------------------------------
  observeEvent(input$termOpt, {
    show_modal_spinner(text = "Cargando Terminología...")
    print("Cambiar Terminología")
   
    if(!emptyCorpus){
      print("Guardamos CaT")
      print(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      saveRDS(tableTerms, paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      saveRDS(listChangesTerms, paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyChanges.rds"))
    }
    
    oldTerm <- currentTerm
    currentTerm <<- input$termOpt
    
    print(oldTerm)
    print(input$termOpt)
    
    if(emptyCorpus){
      print("lol")
      
      tableTerms <<- data.frame()
      listChangesTerms <<- data.frame()
      
      dtTermFull <<- data.frame()
      dtTermExtracted <<- data.frame()
      
    } else {
      if(!(oldTerm == currentTerm)){
        print("LA EXCEPCIÓN")
        print("leemos CaT")
        print(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
        tableTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
        listChangesTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyChanges.rds"))
        
        dtTermFull <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyFull.rds"))
        dtTermExtracted <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyExtracted.rds"))
      }
    }
    
    termsList <<- tableTerms$keyword
    
    reactiveTerm$data <<- tableTerms
    reactiveListTerm$data <<- termsList
    reactiveCurrentTerm$data <<- currentTerm
    
    #Contextualizar Terminologías
    output$dtTermsRaw = DT::renderDataTable(
      tableTerms %>% select(1,4),selection = 'single'
    )
    
    observeEvent(input$dtTermsRaw_rows_selected, {
      output$dtTerms = DT::renderDataTable({
        kwic(corp, pattern = phrase(tableTerms[input$dtTermsRaw_rows_selected,1])) %>% select(1,4,5,6)
      })
    })
    
    remove_modal_spinner()
  })
  
  #Cambiar de corpus ------------------------------------------------------------------------------------------------------------
  observeEvent(input$corpusOpt, {
    show_modal_spinner(text = "Cargando corpus...")
    print("Cambiar Corpus")
    
    print(currentTerm)
    
    if(!emptyCorpus){
      print("Guardamos CC")
      print(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      saveRDS(tableTerms, paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      saveRDS(listChangesTerms, paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyChanges.rds"))
    }
    
    
    currentCorpus <<- input$corpusOpt
    corpusPathSession <<- paste0(getwd(), "/data/corpus_data/", input$corpusOpt)
    
    statisticsSession <<- list()
    
    dtMetadata <<- readRDS(paste0(corpusPathSession, "/processed/corpus/metadata.rds"))
    corp <<- readRDS(paste0(corpusPathSession, "/processed/corpus/corpus.rds"))

    termList <- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/",currentCorpus,"/processed/terminology/"), recursive = FALSE))

    if(length(termList) == 0){
      print("mal")
      currentTerm <<- c()
      # tableTerms <<- data.frame()
      # dtTermFull <<- data.frame()
      # dtTermExtracted <<- data.frame()

      termList <- c("Todavia no has creado ninguna terminología")
      emptyCorpus <<- TRUE
      
      termsList <<- c()

      reactiveCurrentTerm$data <<- c()
    } else {
      print("bien")
      currentTerm <<- termList[1]
      
      emptyCorpus <<- FALSE
    }
    
    if(emptyCorpus){
      print("lol")
      
      tableTerms <<- data.frame()
      listChangesTerms <<- data.frame()
      
      dtTermFull <<- data.frame()
      dtTermExtracted <<- data.frame()
      
    } else {
      print("leemos CC")
      print(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      tableTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      listChangesTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyChanges.rds"))
      
      dtTermFull <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyFull.rds"))
      dtTermExtracted <<- readRDS(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyExtracted.rds"))
      
    }
    
    reactiveTerm$data <<- tableTerms
    reactiveListTerm$data <<- termsList
    reactiveCurrentCorpus$data <<- currentCorpus
    reactiveCurrentTerm$data <<- currentTerm
    
    updateSelectInput(session, "TermForDownload",
                      choices = termList,
                      selected = termList[1])
    
    updateRadioGroupButtons(session, 
                            "termOpt", 
                            label = NULL, 
                            choices = termList,
                            selected = termList[1]
    )
    
    #DATOS DE TERMINOLOGÍA
    output$TermExtracted = DT::renderDataTable({
      dtTermExtracted
    })
    
    output$termFull = DT::renderDataTable({
      dtTermFull %>% select (1,4,9, 10, 11, 18)
    })
    
    #Contextualizar Terminologías
    output$dtTermsRaw = DT::renderDataTable(
      tableTerms %>% select(1,4),selection = 'single'
    )
    
    observeEvent(input$dtTermsRaw_rows_selected, {
      output$dtTerms = DT::renderDataTable({
        kwic(corp, pattern = phrase(tableTerms[input$dtTermsRaw_rows_selected,1])) %>% select(1,4,5,6)
      })
    })
    
    # Estadisticas básicas
    output$Metadata = DT::renderDataTable({
      dtMetadata
    })
    
    #Corpus Actual
    output$docSelected <- renderText({ input$corpusOpt })
    
    #Terminología Actual
    output$termSelected <- renderText({ input$termOpt })
    
    output$docAdd <- renderUI({
      fileInput("docAdd", label = "Añadir documentos", multiple = TRUE)
    })
    
    output$docDelText <- renderUI({
      textInput("docDelText", label = "Eliminar documentos", placeholder = "Escriba el nombre del documento para borrar")
    })
    
    output$docDelBut <- renderUI({
      actionButton("docDelBut", label = "Eliminar", width = 'auto')
    })
    
    remove_modal_spinner()
  })
  
  observeEvent(input$docAdd, {
    file.copy(input$docAdd$datapath, paste0(corpusPathSession, "/raw/", input$docAdd$name))
  })

  output$dtTerminology = DT::renderDataTable({
    isolate(reactiveTerm$data)
  })
  
  observe( {
    DT::replaceData(proxy, reactiveTerm$data, resetPaging = FALSE) 
  }
  )
  
  proxy <- DT::dataTableProxy('dtTerminology')
  
  observe( {
    updateSelectizeInput(session, 'search', choices = reactiveListTerm$data, server = TRUE, options = list(placeholder = 'Insertar términos de búsqueda', create = TRUE,                                                                                                          delimiter = '/n', create = I("function(input, callback){return {value: input, text: input};}")))
  })
  
  
  observeEvent(input$addTerm, {
    if(input$user == "") {
      showModal(
        modalDialog(
          renderText({
            paste0("Debe especificar un nombre de usuario para realizar cambios.")
          })
        ))
    }
    else {
      if(input$addTermText != "") {
        actualDate <- Sys.Date()
        print(colnames(tableTerms))
        tableTermsModify <- rbind(tableTerms, data.frame("keyword" = input$addTermText, "Autor" = input$user, "Fecha" = actualDate, "Frecuencia" = "0"))
        if(dim(tableTermsModify[duplicated(tableTermsModify$keyword),])[1] >= 1) {
          showModal(
            modalDialog(
              renderText({
                paste0("El termino ", input$addTermText, " ya existe en la lista de terminos.")
              })
            ))
        }
        else {
          listChangesTerms <<- rbind(listChangesTerms, data.frame("Tipo" = "Adicion", 
                                                                  "keyword" = input$addTermText, 
                                                                  "Descripcion" = paste0("Se ha añadido el termino |", input$addTermText, "| a la lista terminologica."),
                                                                  "Autor" = input$user,
                                                                  "Fecha" = actualDate,
                                                                  stringsAsFactors = FALSE))
          tableTerms <<- tableTermsModify
          # tableTerms <<- data.frame("Terminos" = sort(unique(tableTerms$Terminos)), stringsAsFactors = FALSE)
          tableTerms <<- tableTerms[order(tableTerms$keyword), ]
          termsList <<- tableTerms$keyword
          # saveRDS(tableTerms, paste0(getwd(),"/data/processed/terminology/terminology.rds"))
          # saveRDS(listChangesTerms, paste0(getwd(),"/data/processed/terminology/terminologyChanges.rds"))
          reactiveTerm$data <<- tableTerms
          reactiveListTerm$data <<- termsList
        }
      }
      else {
        showModal(
          modalDialog(
            renderText({
              paste0("No ha escrito ningun termino.")
            })
          ))
      }}
  })
  
  observeEvent(input$removeTerm, {
    if(input$user == "") {
      showModal(
        modalDialog(
          renderText({
            paste0("Debe especificar un nombre de usuario para realizar cambios.")
          })
        ))
    }
    else {
      if(!is.null(input$dtTerminology_rows_selected)) {
        indexTerms <- input$dtTerminology_rows_selected
        listChangesTerms <<- rbind(listChangesTerms, data.frame("Tipo" = "Eliminacion", 
                                                                "keyword" = tableTerms[indexTerms,]$keyword, 
                                                                "Descripcion" = paste0("Se ha eliminado el termino |", tableTerms[indexTerms,]$keyword, "| de la lista terminologica."),
                                                                "Autor" = input$user,
                                                                "Fecha" = Sys.Date(),
                                                                stringsAsFactors = FALSE))
        tableTerms <<- tableTerms[-indexTerms, ]
        termsList <<- tableTerms$keyword
        # saveRDS(tableTerms, paste0(getwd(),"/data/processed/terminology/terminology.rds"))
        # saveRDS(listChangesTerms, paste0(getwd(),"/data/processed/terminology/terminologyChanges.rds"))
        reactiveTerm$data <<- tableTerms
        reactiveListTerm$data <<- termsList
      }
      else {
        showModal(
          modalDialog(
            renderText({
              paste0("No ha seleccionado ningun termino.")
            })
          ))
      }}
  })
  
  observe({
    x <- input$paternType
    
    if(input$paternType == "pos"){
      updateSelectInput(session, "patern",
                        label = NULL,
                        choices = c("(A|N)*N(P+D*(A|N)*N)*",
                                    "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)",
                                    "N"),
                        selected = tail(x, 1)
      )
    } else if(input$paternType == "upos"){
      updateSelectInput(session, "patern",
                        label = NULL,
                        choices = c("((ADJ|NUM)|(NOUN|PROPN|PRON))*(NOUN|PROPN|PRON)(ADP+DET*((ADJ|NUM)|(NOUN|PROPN|PRON))*(NOUN|PROPN|PRON))*",
                                    "NOUN|PROPN|PRON|NOUNADPDETNOUN|PROPNPROPN|ADJNOUN|NUMNOUN|PROPNPROPNPROPN|PROPNPROPNPROPNPROPN|NOUNPRON|NUMPROPN|NOUNPROPN"),
                        selected = tail(x, 1)
      )
    } else {
      updateSelectInput(session, "patern",
                        label = NULL,
                        choices = c("No se puede proporcionar patrones al metodo RAKE"),
                        selected = tail(x, 1)
      )
    }
  })
  
  observeEvent(input$dtTerminology_rows_selected, {
    updateTextInput(session,"modifyTermText", label = NULL, value=tableTerms[input$dtTerminology_rows_selected[1], "keyword"])
  })
  
  observeEvent(input$modifyTerm, {
    if(input$user == "") {
      showModal(
        modalDialog(
          renderText({
            paste0("Debe especificar un nombre de usuario para realizar cambios.")
          })
        ))
    }
    else {
      if(input$modifyTermText == "") {
        showModal(
          modalDialog(
            renderText({
              paste0("No ha escrito ningun termino.")
            })
          ))
      }
      else {
        if(!is.null(input$dtTerminology_rows_selected)) {
          if(tableTerms[input$dtTerminology_rows_selected[1], "keyword"] != input$modifyTermText) {
            actualDate <- Sys.Date()
            tableTermsModify <<- rbind(tableTerms, data.frame("keyword" = input$modifyTermText, "Autor" = input$user, "Fecha" = actualDate))
            if(dim(tableTermsModify[duplicated(tableTermsModify$keyword),])[1] >= 1) {
              showModal(
                modalDialog(
                  renderText({
                    paste0("El termino ", input$modifyTermText, " ya existe en la lista de terminos.")
                  })
                ))
            }
            else {
              indexTerms <- input$dtTerminology_rows_selected[1]
              listChangesTerms <<- rbind(listChangesTerms, data.frame("Tipo" = "Modificacion", 
                                                                      "keyword" = paste0(tableTerms[indexTerms, "keyword"], " --> ", input$modifyTermText),
                                                                      "Descripcion" = paste0("Se ha modificado el termino |", tableTerms[indexTerms, "keyword"], "| por |", input$modifyTermText, "| en la lista terminologica."),
                                                                      "Autor" = input$user,
                                                                      "Fecha" = actualDate,
                                                                      stringsAsFactors = FALSE))
              tableTerms <<- tableTermsModify
              tableTerms <<- tableTerms[-indexTerms, ]
              tableTerms <<- tableTerms[order(tableTerms$keyword), ]
              termsList <<- tableTerms$keyword
              # saveRDS(tableTerms, paste0(getwd(),"/data/processed/terminology/terminology.rds"))
              # saveRDS(listChangesTerms, paste0(getwd(),"/data/processed/terminology/terminologyChanges.rds"))
              reactiveTerm$data <<- tableTerms
              reactiveListTerm$data <<- termsList
            }
          }
          else {
            showModal(
              modalDialog(
                renderText({
                  paste0("El termino ", input$modifyTermText, " ya existe en la lista de terminos.")
                })
              ))
          }
        }
        else {
          showModal(
            modalDialog(
              renderText({
                paste0("No ha seleccionado ningun termino.")
              })
            ))
        }}}
  })
  
  observeEvent(input$seeListChanges,{
    showModal(
      modalDialog(
        renderDataTable({
          listChangesTerms
        })
        
      ))
  })
  
  session$onSessionEnded(function() {
    
    if(!emptyCorpus){
      print("Guardamos EOP")
      print(paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      saveRDS(tableTerms, paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminology.rds"))
      saveRDS(listChangesTerms, paste0(corpusPathSession, "/processed/terminology/",currentTerm,"/terminologyChanges.rds"))
    }
    
    print('Session ended')
  })
  
}