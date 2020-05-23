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
                               column(12, align = "center",
                                 h1("Terminologías interactivas"),
                                 h4(strong("Proyecto fin de master de Pedro Hernández Vegas \n")),
                                 h5("Aplicación interactiva que permite la creación, modificacion y comparación de
                                    terminologías creadas a traves de la extracciond de terminos con patrones POS y UPOS
                                    de corpus de datos."),
                                 tags$br(),
                               )
                               
                      ),
                      tabPanel("Gestor de documentos", value = "add", icon = icon("folder-open"),
                               h1("Seleccionar"),
                               fluidRow(
                                 column(6, align = "center",
                                        wellPanel(
                                          fluidRow(h4("El corpus seleccionado es: ")),
                                          fluidRow(h2(strong(textOutput("docSelected"))))
                                        )
                                 ),
                                 column(6, align = "center",
                                        wellPanel(
                                          fluidRow(h4("La terminología seleccionada es: ")),
                                          fluidRow(h2(strong(textOutput("termSelected"))))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(radioGroupButtons("corpusOpt", label = h4(("Seleccione el Corpus sobre el que desea trabajar:")), justified= TRUE, choices = corpusList, selected = currentCorpus, direction = "vertical")),
                                        )
                                 ),
                                 column(6, 
                                        wellPanel(
                                          fluidRow(radioGroupButtons("termOpt", label = h4(("Seleccione el Corpus sobre el que desea trabajar:")), justified= TRUE, choices = termList, selected = currentCorpus, direction = "vertical")),
                                        )
                                 ),
                               ), br(),
                               h1("Crear"),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Crear corpus")))),
                                          fluidRow(
                                            column(6,
                                                   h5(strong("Documentos: ")),
                                                   shinyDirButton("dir", "Seleccionar documentos", "Seleccionar documentos")
                                            ),
                                            column(6,
                                                   h5(strong("Codificación:")),
                                                   selectInput("encoding", label = NULL,
                                                              choices = c("Default", "UTF-8-BOM", "ISO-8859-1"), selected = "Default")
                                            ),
                                          ),
                                          fluidRow(
                                            column(12,
                                                   h5(strong("Escriba el nombre del nuevo corpus: ")),
                                                   textInput("nameCorp", label = NULL, placeholder = "Nombre del nuevo corpus")
                                            ),
                                          ),
                                          fluidRow(
                                            column(12, align = "right", actionButton("dirCreate", label = "Crear", width = 'auto')),
                                          )
                                        )
                                 ),
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Crear terminologia")))),
                                          fluidRow(
                                            column(6,
                                                   h5(strong("Corpus: ")),
                                                   selectInput("corpusForTerm",label = NULL,
                                                               choices = corpusList, selected = currentCorpus)
                                                   #shinyDirButton("dir", "Seleccionar corpus", "Seleccionar documentos")
                                            ),
                                            column(6,
                                                   h5(strong("Modelo:")),
                                                   selectInput("idioma", label = NULL,
                                                               choices = c("afrikaans-afribooms",
                                                                 "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt",
                                                                 "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
                                                                 "buryat-bdt", "catalan-ancora", "chinese-gsd", "classical_chinese-kyoto",
                                                                 "coptic-scriptorium", "croatian-set", "czech-cac", "czech-cltt",
                                                                 "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino",
                                                                 "dutch-lassysmall", "english-ewt", "english-gum", "english-lines",
                                                                 "english-partut", "estonian-edt", "estonian-ewt", "finnish-ftb",
                                                                 "finnish-tdt",      "french-gsd", "french-partut", "french-sequoia",
                                                                 "french-spoken", "galician-ctg", "galician-treegal", "german-gsd",
                                                                 "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", "hungarian-szeged",
                                                                 "indonesian-gsd", "irish-idt", "italian-isdt", "italian-partut",
                                                                 "italian-postwita", "italian-vit", "japanese-gsd", "kazakh-ktb", "korean-gsd",
                                                                 "korean-kaist", "kurmanji-mg", "latin-ittb", "latin-perseus", "latin-proiel",
                                                                 "latvian-lvtb", "lithuanian-alksnis", "lithuanian-hse", "maltese-mudt",
                                                                 "marathi-ufal",      "north_sami-giella", "norwegian-bokmaal",
                                                                 "norwegian-nynorsk", "norwegian-nynorsklia", "old_church_slavonic-proiel",
                                                                 "old_french-srcmf", "old_russian-torot", "persian-seraji", "polish-lfg",
                                                                 "polish-pdb", "polish-sz", "portuguese-bosque", "portuguese-br",
                                                                 "portuguese-gsd", "romanian-nonstandard", "romanian-rrt", "russian-gsd",
                                                                 "russian-syntagrus", "russian-taiga", "sanskrit-ufal", "serbian-set",
                                                                 "slovak-snk", "slovenian-ssj", "slovenian-sst", "spanish-ancora",
                                                                 "spanish-gsd", "swedish-lines", "swedish-talbanken",      "tamil-ttb",
                                                                 "telugu-mtg", "turkish-imst", "ukrainian-iu", "upper_sorbian-ufal",
                                                                 "urdu-udtb", "uyghur-udt", "vietnamese-vtb", "wolof-wtb"), selected = "spanish-gsd")
                                            )
                                            
                                          ),
                                          fluidRow(
                                            column(6,
                                                   h5(strong("Tipo de patrón:")),
                                                   selectInput("paternType", label = NULL,
                                                               choices = c("POS" = "pos",
                                                                           "UPOS" = "upos",
                                                                           "RAKE" = "rake"))
                                            ),
                                            column(6,
                                                   h5(strong("Patrón:")),
                                                   selectizeInput("patern", label = NULL, 
                                                                  choices = c("(A|N)*N(P+D*(A|N)*N)*",
                                                                              "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)",
                                                                              "N"))
                                                   )
                                          ),
                                          fluidRow(
                                            column(12,
                                                   h5(strong("Escriba el nombre de la nueva terminología: ")),
                                                   textInput("nameTerm", label = NULL, placeholder = "Nombre de la nueva terminología")
                                            ),
                                          ),
                                          fluidRow(
                                            column(12, align = "right", actionButton("dirCreateTerm", label = "Crear", width = 'auto')),
                                          )
                                        ),
                                 )
                               ),
                               h1("Subir"),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Subir corpus")))),
                                          fluidRow(
                                            column(6, align = "left", shinyDirButton("dirCorp", "Seleccionar corpus", "Seleccionar documentos")),
                                            column(6, align = "right", actionButton("dirCreateCorp", label = "Subir", width = 'auto'))
                                          )
                                        )
                                 ),
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Subir terminologia")))),
                                          fluidRow(
                                            column(6, align = "left", shinyDirButton("dirTerm", "Seleccionar Terminología", "Seleccionar documentos")),
                                            column(6, align = "right", actionButton("dirCreateTerm", label = "Subir", width = 'auto'))
                                          )
                                        )
                                 )
                               ),
                               h1("Eliminar"),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Eliminar corpus")))),
                                          fluidRow(
                                            column(8, textInput("corpusDelText", label = NULL, placeholder = "Escriba el nombre del corpus para borrar")),
                                            column(4, align = "right", actionButton("corpusDelBut", label = "Eliminar", width = 'auto'))
                                          )
                                        ),
                                  ),
                                 column(6,
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Eliminar Terminología")))),
                                          fluidRow(
                                            column(8, textInput("termDelText", label = NULL, placeholder = "Escriba el nombre de la terminología para borrar")),
                                            column(4, align = "right", actionButton("termDelBut", label = "Eliminar", width = 'auto'))
                                          )
                                        )
                                 )
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
                      tabPanel("Datos estadisticos", value = "stat", icon = icon("list"),
                               #h2(strong("Datos estadisticos")),
                               h5("Información estadistica relacionada con el corpus inicial de los documentos."),
                               br(),
                               wellPanel( style = "background: white",
                                          h4("Metadatos del corpus actual:"),
                                          dataTableOutput("Metadata")
                               )
                      ),
                      tabPanel("Contextualizar", value = "context", icon = icon("quote-left"),
                               h2("Ver terminos en contexto"),
                               fluidRow(
                                 column(6,
                                        wellPanel( style = "background: white",
                                                   h4("Tabla con los términos actuales:"),
                                                   dataTableOutput("dtTermsRaw")
                                        )
                                 ),
                                 column(6,
                                        wellPanel( style = "background: white",
                                                   h4("Tabla con los términos en contexto:"),
                                                   dataTableOutput("dtTerms")
                                        )
                                 )
                               )
                      ),
                      tabPanel("Comparar Terminologías", value = "CompTerm", icon = icon("less-than"),
                               h5("Aquí irá el sistema comparativo de terminologías interactivo"),
                               
                      ),
                      tags$head(tags$style(HTML('#search+ div>.selectize-input{min-width: 300px; max-width:100%; !important;}')))
  )))
