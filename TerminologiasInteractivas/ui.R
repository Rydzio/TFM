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
                      tabPanel("Home", value = "home", icon = icon("home"),
                               #h2(strong("Inicio")),
                               #br(),
                               column(12, align = "center",
                                 h1("Interactive terminologies"),
                                 h4(strong("Project by Pedro Hernández Vegas \n")),
                                 h5("Interactive application that allows the creation, modification and comparison of terminologies created through the extraction of terms with POS and UPOS patterns from data corpora."),
                                 tags$br(),
                               )
                               
                      ),
                      tabPanel("Document manager", value = "add", icon = icon("folder-open"),
                               column(12, align="center", h1("Select")),
                               fluidRow(
                                 column(6, align = "center",
                                        wellPanel(
                                          fluidRow(h4("The selected corpus is: ")),
                                          fluidRow(h2(strong(textOutput("docSelected"))))
                                        )
                                 ),
                                 column(6, align = "center",
                                        wellPanel(
                                          fluidRow(h4("The selected terminology is: ")),
                                          fluidRow(h2(strong(textOutput("termSelected"))))
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(radioGroupButtons("corpusOpt", label = h4(("Select the Corpus you wish to work on:")), justified= TRUE, choices = corpusList, selected = currentCorpus, direction = "vertical")),
                                        )
                                 ),
                                 column(6, 
                                        wellPanel(
                                          fluidRow(radioGroupButtons("termOpt", label = h4(("Select the terminology you wish to work on:")), justified= TRUE, choices = termList, selected = NULL, direction = "vertical")),
                                        )
                                 ),
                               ), br(),
                               column(12, align="center",h1("Create")),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Create corpus")))),
                                          fluidRow(
                                            column(6,
                                                   h5(strong("Documents: ")),
                                                   shinyDirButton("dir", "Select documents", "Seleccionar documentos")
                                            ),
                                            column(6,
                                                   h5(strong("Codification:")),
                                                   selectInput("encoding", label = NULL,
                                                              choices = c("Default", "UTF-8-BOM", "ISO-8859-1"), selected = "Default")
                                            ),
                                          ),
                                          fluidRow(
                                            column(12,
                                                   h5(strong("Write the name of the new corpus: ")),
                                                   textInput("nameCorp", label = NULL, placeholder = "Name of the new corpus")
                                            ),
                                          ),
                                          fluidRow(
                                            column(12, align = "right", actionButton("dirCreate", label = "Create", width = 'auto')),
                                          )
                                        )
                                 ),
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Create terminology")))),
                                          fluidRow(
                                            column(6,
                                                   h5(strong("Corpus: ")),
                                                   selectInput("corpusForTerm",label = NULL,
                                                               choices = corpusList, selected = currentCorpus)
                                                   #shinyDirButton("dir", "Seleccionar corpus", "Seleccionar documentos")
                                            ),
                                            column(6,
                                                   h5(strong("Model:")),
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
                                                   h5(strong("Pattern type:")),
                                                   selectInput("paternType", label = NULL,
                                                               choices = c("POS" = "pos",
                                                                           "UPOS" = "upos",
                                                                           "RAKE" = "rake"))
                                            ),
                                            column(6,
                                                   h5(strong("Pattern:")),
                                                   selectizeInput("patern", label = NULL, 
                                                                  choices = c("N(A|N)*(PD*N(A|N)*)*",
                                                                              "(A|N)*N(P+D*(A|N)*N)*",
                                                                              "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)",
                                                                              "N"),
                                                                  selected = "N(A|N)*(PD*N(A|N)*)*",
                                                                  options = list(create = TRUE)
                                                                  )
                                                   )
                                          ),
                                          fluidRow(
                                            column(12,
                                                   h5(strong("Enter the name of the new terminology: ")),
                                                   textInput("nameTerm", label = NULL, placeholder = "Name of the new terminology")
                                            ),
                                          ),
                                          fluidRow(
                                            column(12, align = "right", actionButton("dirCreateTerm", label = "Create", width = 'auto')),
                                          )
                                        ),
                                 )
                               ),
                               column(12, align="center",h1("Download")),
                               fluidRow(
                                 column(6,
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Download corpus")))),
                                          fluidRow(
                                            column(8, selectInput("CorpusForDownload",label = NULL, choices = corpusList, selected = currentCorpus)),
                                            column(4, align = "right", downloadButton("downloadDataCorpus", "Download"))
                                          )
                                        )
                                 ),
                                 column(6,
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Download terminology")))),
                                          fluidRow(
                                            column(8, selectInput("TermForDownload",label = NULL, choices = termList, selected = NULL)),
                                            column(4, align = "right", downloadButton("downloadDataTerm", "Download"))
                                          )
                                        )
                                 )
                               ),
                               column(12, align="center",h1("Upload")),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Upload corpus")))),
                                          fluidRow(
                                            # column(6, align = "left", shinyDirButton("dirCorp", "Seleccionar corpus", "Seleccionar documentos")),
                                            # column(6, align = "right", actionButton("dirCreateCorp", label = "Subir", width = 'auto'))
                                            column(12,
                                              fileInput("dirCorp", "CSV file type",
                                                        multiple = FALSE,
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values,text/plain",
                                                                   ".csv")
                                                        )
                                            ),
                                          )
                                        )
                                 ),
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Upload terminolgy")))),
                                          fluidRow(
                                            # column(6, align = "left", shinyDirButton("dirTerm", "Seleccionar Terminología", "Seleccionar documentos")),
                                            # column(6, align = "right", actionButton("dirCreateTerm", label = "Subir", width = 'auto'))
                                            column(12,
                                                   fileInput("dirTerm", "CSV file type",
                                                             multiple = FALSE,
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")
                                                   )
                                            ),
                                          )
                                        )
                                 )
                               ),
                               column(12, align="center",h1("Delete")),
                               fluidRow(
                                 column(6, 
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Delete corpus")))),
                                          fluidRow(
                                            column(8, textInput("corpusDelText", label = NULL, placeholder = "Enter the name of the corpus to be deleted")),
                                            column(4, align = "right", actionButton("corpusDelBut", label = "Delete", width = 'auto'))
                                          )
                                        ),
                                  ),
                                 column(6,
                                        wellPanel(
                                          fluidRow(column(12, h4(strong("Delete Terminology")))),
                                          fluidRow(
                                            column(8, textInput("termDelText", label = NULL, placeholder = "Enter the name of the terminology to be deleted")),
                                            column(4, align = "right", actionButton("termDelBut", label = "Delete", width = 'auto'))
                                          )
                                        )
                                 )
                               )
                      ),
                      tabPanel("Terminology manager", value = "termin", icon = icon("file-alt"),
                               fluidRow( 
                                 column(3, actionButton("seeListChanges", "List of changes", width = '130px', style='height:30px')),
                                 column(3, offset = 7, helpText("Enter your name to keep track of changes", style = "font-size:12px")),
                                 column(2, textInputAddon(inputId = "user", label = NULL, placeholder = "User", addon = icon("user")))),
                               fluidRow( 
                                 column(4,
                                        wellPanel( style = "height:170px",
                                                   textInput("addTermText", h4("Add terms"), placeholder = "Write the new term..."),
                                                   #updateTextInput(session,"addTermText", h4("Añadir términos"), value="") #probar en el server
                                                   actionButton("addTerm", "Add", width = '90px', style='height:30px')
                                        )),
                                 column(4,
                                        
                                        wellPanel( style = "height:170px",
                                                   h4("Delete terms"),
                                                   p("Select the terms you wish to delete and press the button."),
                                                   actionButton("removeTerm", "Delete", width = 'auto', style='height:30px')
                                        )
                                 ),
                                 column(4,
                                        wellPanel( style = "height:170px",
                                                   h4("Modify terms"),
                                                   p("Select the term, enter the modification and press the button."),
                                                   fluidRow(
                                                     column(8, textInput("modifyTermText", label = NULL, placeholder = "Term to be modified")), #dtTerminology_rows_selected
                                                     column(2, actionButton("modifyTerm", label = icon("check-circle"), width = '50px', style='height:30px'))
                                                   )
                                        )
                                 )),
                               wellPanel( style = "background: white",
                                          h4("Table with the current terms:"),
                                          dataTableOutput("dtTerminology")
                               )
                      ),
                      tabPanel("Statistical data", value = "stat", icon = icon("list"),
                               h4("Statistical information related to the corpus of documents"),
                               br(),
                               wellPanel(
                                 fluidRow(
                                   column(6,
                                          p("Size of the corpus:", span(textOutput("corpSize", inline = TRUE), style = "font-weight:bold"), "MB"),
                                          p("Size of the original documents:", span(textOutput("docSize", inline = TRUE), style = "font-weight:bold"), "MB"),
                                          p("Number of documents: ", span(textOutput("numDocs", inline = TRUE), style = "font-weight:bold"), "Documentos")
                                          ),
                                   column(6,
                                          p("Total number of tokens:", span(textOutput("tokenSize", inline = TRUE), style = "font-weight:bold"), "tokens"),
                                          p("Total number of pages:", span(textOutput("pageTotal", inline = TRUE), style = "font-weight:bold"), "paginas"),
                                        ),
                                 ),
                               ),
                               wellPanel( style = "background: white",
                                          h4("Metadata of the current corpus:"),
                                          dataTableOutput("Metadata")
                               )
                      ),
                      tabPanel("Contextualize", value = "context", icon = icon("quote-left"),
                               h2("See terms in context"),
                               fluidRow(
                                 column(5,
                                        wellPanel(
                                          style = "background: white",
                                          h4("Table with the current terms:"),
                                          dataTableOutput("dtTermsRaw")
                                        )
                                 ),
                                 column(7,
                                        wellPanel( style = "background: white",
                                                   h4("Table with the terms in context:"),
                                                   dataTableOutput("dtTerms")
                                        )
                                 )
                               )
                      ),
                      tabPanel("Compare Terminologies", value = "CompTerm", icon = icon("less-than"),
                               fluidRow(
                                 column(6,
                                        wellPanel(
                                          style = "background: white",
                                          h4("Table with the current terms:"),
                                          fluidRow(radioGroupButtons("termComp1", label = h4(("Select a terminology to compare:")), justified= TRUE, choices = termList, selected = NULL, direction = "vertical")),
                                          dataTableOutput("tdTermsComp1")
                                        )
                                 ),
                                 column(6,
                                        wellPanel( 
                                          style = "background: white",
                                          h4("Table with the terms in context:"),
                                          fluidRow(radioGroupButtons("termComp2", label = h4(("Select another terminology to compare:")), justified= TRUE, choices = termList, selected = NULL, direction = "vertical")),
                                          dataTableOutput("tdTermsComp2")
                                        )
                                 )
                               ),
                               wellPanel(
                                 fluidRow(
                                   column(6, align = "center",
                                          p( span(textOutput("dtComp1Rows", inline = TRUE), style = "font-weight:bold"), "Wrong terms in Terminology 1"),
                                   ),
                                   column(6, align = "center",
                                          p( span(textOutput("dtComp2Rows", inline = TRUE), style = "font-weight:bold"), "Wrong terms in Terminology 2"),
                                   ),
                                   column(12, align = "center",
                                          p("The degree of overlap between the two terminologies is", span(textOutput("Solapamiento", inline = TRUE), style = "font-weight:bold"), "Terms"),
                                   ),
                                 ),
                               ),
                      ),
                      tags$head(tags$style(HTML('#search+ div>.selectize-input{min-width: 300px; max-width:100%; !important;}')))
  )))
