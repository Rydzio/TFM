library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(waiter)
library(rsvg)
library(svgPanZoom)

fluidPage(
  useShinyjs(),
  use_waiter(),
  fluidRow(navbarPage(id = "inNavbar", title = strong("TermInteract"), 
                      # pestaña de inicio
                      tabPanel("Inicio", value = "home", icon = icon("home"),
                               #h2(strong("Inicio")),
                               #br(),
                               h4(strong("Proyecto realizado en AI.nnovation Space. \n")),
                               h5("El Centro Tecnológico Mixto UPM-Accenture AI.nnovation Space 
                                         tiene como objetivo promocionar la investigación y la innovación 
                                         en diferentes dominios, que puedan generar soluciones innovadoras 
                                         basadas en técnicas de Inteligencia Artificial y que puedan ser 
                                         transferidas al mercado."),
                               tags$br(),
                               fluidRow(             
                                 column(4, offset = 1, tags$img(id = "imag_acc", height = 55, src = "accenture.png")), 
                                 column(4, tags$img(id = "imag_upm", height = 75, src = "upm.png"))
                               )
                               
                      ),
                      tabPanel("Gestor de terminología", value = "termin", icon = icon("file-alt"),
                               fluidRow( 
                                 column(3, actionButton("seeListChanges", "Lista de cambios", width = '130px', style='height:30px')),
                                 column(3, offset = 7, helpText("Indique su nombre para mantener un control de los cambios", style = "font-size:12px")),
                                 column(2, textInputAddon(inputId = "user", label = NULL, placeholder = "Usuario", addon = icon("user")))),
                               fluidRow( 
                                 column(4,
                                        wellPanel( style = "height:170px",
                                                   textInput("addTermText", h4("Añadir términos"), placeholder = "Escriba el nuevo término..."),
                                                   #updateTextInput(session,"addTermText", h4("Añadir términos"), value="") #probar en el server
                                                   actionButton("addTerm", "Añadir", width = '90px', style='height:30px')
                                        )),
                                 column(4,
                                        
                                        wellPanel( style = "height:170px",
                                                   h4("Eliminar términos"),
                                                   p("Seleccione aquellos términos que desee eliminar y pulse el botón."),
                                                   actionButton("removeTerm", "Eliminar", width = 'auto', style='height:30px')
                                        )
                                 ),
                                 column(4,
                                        wellPanel( style = "height:170px",
                                                   h4("Modificar términos"),
                                                   p("Seleccione el término, escriba la modificación y pulse el botón."),
                                                   fluidRow(
                                                     column(8, textInput("modifyTermText", label = NULL, placeholder = "Término a modificar")), #dtTerminology_rows_selected
                                                     column(2, actionButton("modifyTerm", label = icon("check-circle"), width = '50px', style='height:30px'))
                                                   )
                                        )
                                 )),
                               wellPanel( style = "background: white",
                                          h4("Tabla con los términos actuales:"),
                                          dataTableOutput("dtTerminology")
                               )
                      ),
                      tabPanel("Gestor de documentos", value = "add", icon = icon("folder-open"),
                               #h2(strong("Gestor de documentos")),
                               h1("Seleccionar una terminología"),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(radioGroupButtons("corpusOpt", label = h4(("Seleccione el Corpus sobre el que desea trabajar")), justified= TRUE, choices = corpusList, selected = currentCorpus, direction = "vertical")))
                                 ),
                                 column(6,
                                        fluidRow(h4("El corpus seleccionado es: ", strong(textOutput("docSelected"))))
                                        #fluidRow(fileInput("docAdd", label = "Añadir documentos", multiple = TRUE)),
                                        # fluidRow(uiOutput("docAdd")),
                                        # fluidRow(uiOutput("docDelText")),
                                        # fluidRow(uiOutput("docDelBut"))
                                        
                                 )
                               ), br(),
                               h1("Crear una nueva terminología"),
                               fluidRow(
                                 column(12,
                                        wellPanel(
                                          selectInput("paternType", "Selecciona el tipo de patron:",
                                                      c("POS" = "pos",
                                                        "UPOS" = "upos",
                                                        "RAKE" = "rake")),
                                          h5("Escriba el patrón que desea emplear para la extracción de terminos"),
                                          selectizeInput("patern", label = NULL, choices = c("(A|N)*N(P+D*(A|N)*N)*",
                                                                                             "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)",
                                                                                             "N", 
                                                                                             "((ADJ|NUM)|(NOUN|PROPN|PRON))*(NOUN|PROPN|PRON)(ADP+DET*((ADJ|NUM)|(NOUN|PROPN|PRON))*(NOUN|PROPN|PRON))*",
                                                                                             "NOUN|PROPN|PRON|NOUNADPDETNOUN|PROPNPROPN|ADJNOUN|NUMNOUN|PROPNPROPNPROPN|PROPNPROPNPROPNPROPN|NOUNPRON|NUMPROPN|NOUNPROPN")),
                                          fluidRow(
                                            column(6, h5("Escriba el nombre de la nueva terminología")),
                                            column(6, textInput("nameCorp", label = NULL, placeholder = "Nombre de la nueva terminología"))),
                                          fluidRow(column(12, shinyDirButton("dir", "Seleccionar documentos", "Seleccionar documentos")))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(12, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Eliminar corpus")))),
                                          fluidRow(column(12, textInput("corpusDelText", label = NULL, placeholder = "Escriba el nombre del documento para borrar"))),
                                          fluidRow(column(12, actionButton("corpusDelBut", label = "Eliminar", width = 'auto')))
                                        ))
                               )),
                      tabPanel("Datos estadisticos", value = "stat", icon = icon("list"),
                               #h2(strong("Datos estadisticos")),
                               h5("Información estadistica relacionada con el corpus inicial de los documentos."),
                               br(),
                               wellPanel( style = "background: white",
                                          h4("Metadatos del corpus actual:"),
                                          dataTableOutput("Metadata")
                               )
                      ),
                      tabPanel("Comparar Terminologías", value = "CompTerm", icon = icon("less-than"),
                               h5("Aquí irá el sistema comparativo de terminologías interactivo"),
                               
                               wellPanel( style = "background: white",
                                          h4("Terminologia Extraida: "),
                                          dataTableOutput("TermExtracted")
                               ),
                               wellPanel( style = "background: white",
                                          h4("Terminologia completa:"),
                                          dataTableOutput("termFull")
                               )
                      ),
                      tags$head(tags$style(HTML('#search+ div>.selectize-input{min-width: 300px; max-width:100%; !important;}')))
  )))
