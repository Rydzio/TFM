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
  
  shinyDirChoose(input, 'dir', roots = getVolumes())
  
  observeEvent(input$allowContains, {
    allowContains <<- input$allowContains
  })
  
  observeEvent(input$dirCreate, {
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
      
      createCorpus(path, nameCorpus, 16, input$patern, input$paternType)
      corpusList <<- basename(list.dirs(path = paste0(getwd(), "/data/corpus_data/"), recursive = FALSE))
      reactiveCorpusList$data <<- corpusList
    }
  })
  
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
  
  observeEvent(input$corpusOpt, {
    #w$show()
    
    saveRDS(tableTerms, paste0(corpusPathSession, "/processed/terminology/terminology.rds"))
    saveRDS(listChangesTerms, paste0(corpusPathSession, "/processed/terminology/terminologyChanges.rds"))
    
    currentCorpus <<- input$corpusOpt
    corpusPathSession <<- paste0(getwd(), "/data/corpus_data/", input$corpusOpt)
    
    statisticsSession <<- list()
    
    tableTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/terminology.rds"))
    listChangesTerms <<- readRDS(paste0(corpusPathSession, "/processed/terminology/terminologyChanges.rds"))
    
    dtMetadata <<- readRDS(paste0(corpusPathSession, "/processed/corpus/metadata.rds"))
    
    dtTermFull <<- readRDS(paste0(corpusPathSession, "/processed/terminology/terminologyFull.rds"))
    dtTermExtracted <<- readRDS(paste0(corpusPathSession, "/processed/terminology/terminologyExtracted.rds"))
    
    termsList <<- tableTerms$Terminos
    
    reactiveTerm$data <<- tableTerms
    reactiveListTerm$data <<- termsList
    reactiveCurrentCorpus$data <<- currentCorpus
    
    #DATOS DE TERMINOLOGÍA
    output$TermExtracted = DT::renderDataTable({
      dtTermExtracted
    })
    
    output$termFull = DT::renderDataTable({
      dtTermFull
    })
    
    # Estadisticas básicas
    output$Metadata = DT::renderDataTable({
      dtMetadata
    })
    
    output$docSelected <- renderText({ input$corpusOpt })
    output$docAdd <- renderUI({
      fileInput("docAdd", label = "Añadir documentos", multiple = TRUE)
    })
    
    output$docDelText <- renderUI({
      textInput("docDelText", label = "Eliminar documentos", placeholder = "Escriba el nombre del documento para borrar")
    })
    
    output$docDelBut <- renderUI({
      actionButton("docDelBut", label = "Eliminar", width = 'auto')
    })
  })
  
  observeEvent(input$docAdd, {
    file.copy(input$docAdd$datapath, paste0(corpusPathSession, "/raw/", input$docAdd$name))
  })
  
  # Barra buscadora. En termsList se debe añadir la lista de términos completa
  updateSelectizeInput(session, 'search', choices = termsList, server = TRUE, options = list(placeholder = 'Insertar términos de búsqueda', create = TRUE, 
                                                                                             delimiter = '/n', create = I("function(input, callback){return {value: input, text: input};}")
  ))

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
        tableTermsModify <- rbind(tableTerms, data.frame("Terminos" = input$addTermText, "Autor" = input$user, "Fecha" = actualDate))
        if(dim(tableTermsModify[duplicated(tableTermsModify$Terminos),])[1] >= 1) {
          showModal(
            modalDialog(
              renderText({
                paste0("El termino ", input$addTermText, " ya existe en la lista de terminos.")
              })
            ))
        }
        else {
          listChangesTerms <<- rbind(listChangesTerms, data.frame("Tipo" = "Adicion", 
                                                                  "Terminos" = input$addTermText, 
                                                                  "Descripcion" = paste0("Se ha añadido el termino |", input$addTermText, "| a la lista terminologica."),
                                                                  "Autor" = input$user,
                                                                  "Fecha" = actualDate,
                                                                  stringsAsFactors = FALSE))
          tableTerms <<- tableTermsModify
          # tableTerms <<- data.frame("Terminos" = sort(unique(tableTerms$Terminos)), stringsAsFactors = FALSE)
          tableTerms <<- tableTerms[order(tableTerms$Terminos), ]
          termsList <<- tableTerms$Terminos
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
                                                                "Terminos" = tableTerms[indexTerms,]$Terminos, 
                                                                "Descripcion" = paste0("Se ha eliminado el termino |", tableTerms[indexTerms,]$Terminos, "| de la lista terminologica."),
                                                                "Autor" = input$user,
                                                                "Fecha" = Sys.Date(),
                                                                stringsAsFactors = FALSE))
        tableTerms <<- tableTerms[-indexTerms, ]
        termsList <<- tableTerms$Terminos
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
  
  observeEvent(input$dtTerminology_rows_selected, {
    updateTextInput(session,"modifyTermText", label = NULL, value=tableTerms[input$dtTerminology_rows_selected[1], "Terminos"])
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
          if(tableTerms[input$dtTerminology_rows_selected[1], "Terminos"] != input$modifyTermText) {
            actualDate <- Sys.Date()
            tableTermsModify <<- rbind(tableTerms, data.frame("Terminos" = input$modifyTermText, "Autor" = input$user, "Fecha" = actualDate))
            if(dim(tableTermsModify[duplicated(tableTermsModify$Terminos),])[1] >= 1) {
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
                                                                      "Terminos" = paste0(tableTerms[indexTerms, "Terminos"], " --> ", input$modifyTermText),
                                                                      "Descripcion" = paste0("Se ha modificado el termino |", tableTerms[indexTerms, "Terminos"], "| por |", input$modifyTermText, "| en la lista terminologica."),
                                                                      "Autor" = input$user,
                                                                      "Fecha" = actualDate,
                                                                      stringsAsFactors = FALSE))
              tableTerms <<- tableTermsModify
              tableTerms <<- tableTerms[-indexTerms, ]
              tableTerms <<- tableTerms[order(tableTerms$Terminos), ]
              termsList <<- tableTerms$Terminos
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
    saveRDS(tableTerms, paste0(corpusPathSession, "/processed/terminology/terminology.rds"))
    saveRDS(listChangesTerms, paste0(corpusPathSession, "/processed/terminology/terminologyChanges.rds"))+
      
      file.remove(list.files(paste0(corpusPathSession, "/processed/doc_images/"), pattern = "*.(png|svg)",full.names = TRUE, recursive = FALSE))
    
    print('Session ended')
  })
  
}