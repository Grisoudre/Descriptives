options(encoding = "UTF-8")
library(shinythemes)
library(shinyjs)
library(ggplot2)

# --------------------------- TITRE -------------------------------#

shinyUI(
  navbarPage("Descri",


             #--------------------------- ONGLET importation et filtres -------------------------#

             tabPanel("1. Importation et sélection des individus",
                      fluidPage(
                        tags$head(tags$style(HTML(
                          "h6{ background-color: #FFF3BE ; font-size:16px;
                          font-family: calibri, Arial, sans-serif ; font-size:16px;}")
                          #,type="text/css",
                          #          ".shiny-output-error { visibility: hidden; }",
                          #           ".shiny-output-error:before { visibility: hidden; }"
                        )
                        )

                        ,theme=shinytheme("simplex"),
                        (column(12,h6('ATTENTION : Avant toute action,
                                    cliquer sur "Open in browser", sans quoi les
                                    boutons et téléchargements ne fonctionneront pas.'))),

                        column(5,h4("1. Fichier à charger (.txt ou .csv)"),
                               h6("La table brute est une base de données dont les lignes correspondent aux individus statistiques.
                                  Elle doit être en format texte (.txt ou .csv) ; Le délimitateur,
                                  l'extension du fichier et l'encodage des caractères sont précisés.
                                  Elle est importée en passant par le bouton \"browse\"."),
                               wellPanel(
                                 uiOutput("donnees.fichier.ui")),
                               h4("2. Détail des variables importées"),
                               h6("Vérifier que le tableau a correctement été
                                  importé à l'aide du résumé suivant :"),
                               textOutput("Dimensions"),
                               tableOutput("Resume")),
                        column(7,
                               fluidRow(column(6,
                                               h4("3. Choix de l'identifiant (Obligatoire)"),
                                               uiOutput("SelectID")),
                                        column(6,
                                               h6("Si la table ne contient pas d'identifiant
                                                  unique, le bouton ci-dessous permet de
                                                  télécharger la table à laquelle sera ajoutée
                                                  une variable \"ID\""),
                                               downloadButton("PasDID",'Télécharger avec ajout d\'un identifiant ("ID")'))
                               ),


                               h4("4. Sélection des individus / Filtres (Optionnel)"),
                               h6("Ces filtres peuvent réduire et préciser la population étudiée. Ce sont des filtres logiques.
                                  Si on ne souhaite aucune sélection, penser à sélectionner un choix vide sur chaque champ."),
                               h6("Les filtres intermédiaires entre les critères portent sur les individus :"),
                               h6("ET : Une même ligne ne sera conservée que si elle satisfait aux deux critères ;"),
                               h6("OU : Une ligne sera conservée si elle répond au 1er critère ou bien si elle répond au 2ème critère."),
                               wellPanel(
                                 p(strong("Critère 1 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar1")),
                                           column(2,
                                                  selectInput("Operateur1", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,

                                                  uiOutput("Select")))


                               ),
                               selectInput("OperateurMid","",
                                           choices=as.list(c(" ","OU","ET")), selected = NULL),
                               wellPanel(
                                 p(strong("Critère 2 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar2")),
                                           column(2,
                                                  selectInput("Operateur2", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,

                                                  uiOutput("Select2")))),

                               selectInput("OperateurMid2","",
                                           choices=as.list(c(" ","OU","ET")), selected = NULL),
                               wellPanel(
                                 p(strong("Critère 3 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar3")),
                                           column(2,
                                                  selectInput("Operateur3", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,

                                                  uiOutput("Select3")))
                               ),

                               selectInput("OperateurMid3","",
                                           choices=as.list(c(" ","OU","ET")), selected = NULL),
                               wellPanel(
                                 p(strong("Critère 4 :"), align="left"),
                                 fluidRow( column(5,
                                                  uiOutput("SelectVar4")),
                                           column(2,
                                                  selectInput("Operateur4", "Opérateur :",
                                                              choices=as.list(c(" ","=","diff. de",">",">=","<","<=")))),
                                           column(5,

                                                  uiOutput("Select4")))))
                        )),


             #--------------------------- ONGLET tris à plat / croisés -------------------------#

             tabPanel("2. Tris à plat et tris croisés",
                      fluidPage(

                             h4("1. Choix des variables"),
                             h6("- Si les listes de variables ne s'affichent pas, vérifier que
                                l'importation a correctement fonctionné et qu'un identifiant a été
                                sélectionné (onglet 1. Importation et sélection)."),
                              h6  ("- Pour un tri à plat, sélectionner uniquement la 1ère variable et
                                 laisser la 2ème vide (première option)."),
                             h6  ("  - Pour un tri croisé, sélectionner la 1ère variable (représentée en
                                lignes) et la 2ème (en colonnes)."),
                               h6  (" - Les types (qualitatif ou quantitatif) des variables sont modifiables."),
                                  h6  ("  - Si une variable est qualitative et l'autre quantitative,
                                 sélectionner la qualitative dans le champ \"1ère variable\"
                                "),
                      fluidRow(column(6,
                             uiOutput("SelectVarCrois1")),
                      column(6,
                             textOutput("TypeVar1"),
                             selectInput("TypeVarCrois1","",
                                         choices=as.list(c("Pas de modification","Qualitative",
                                                           "Quantitative - entier",
                                                          # "Quantitative - réel",
                                                           "Logique")), selected = "Pas de modification"))),
                      fluidRow( column(6,
                              uiOutput("SelectVarCrois2")),
                       column(6,
                              textOutput("TypeVar2"),
                             selectInput("TypeVarCrois2","",
                                         choices=as.list(c("Pas de modification","Qualitative",
                                                           "Quantitative - entier",
                                                          # "Quantitative - réel",
                                                           "Logique")), selected = "Pas de modification"))),

                                           h4("2. Tableau"),
                      fluidRow   (  column(3,wellPanel("Affichage :",
                      uiOutput("ChoixCumul"),
                      uiOutput("ChoixReprez"),
                      uiOutput("h6_Reprez"),
                      uiOutput("ChoixAfficherNA"))),
                      column(6, wellPanel("Ordonner les modalités",
                                          uiOutput("ChoixOrdreTab1"),
                                          uiOutput("ChoixOrdreTab2"),
                                          uiOutput("ChoixTrier")))),

                        tableOutput("Tableau"),
                      textOutput("Population"),

                      downloadButton("DlTable","Télécharger la table"),
                      hr(),
                             h4("3. Graphique"),
                      h4("Options graphiques :"),
                             fluidRow(column(3,
                             wellPanel("Affichage :",
                             uiOutput("ChoixGrapheAffichage"),
                             uiOutput("ChoixGrapheEmpile"),
                             uiOutput("ChoixAjoutEns"))),
                             column(3,
                             wellPanel("Ordre des modalités",
                                       uiOutput("ChoixOrdre"),
                                       uiOutput("ChoixOrdreVar2"),
                                       uiOutput("ChoixGrapheOrdonne")
                             )),
                             column(3,
                                    wellPanel("Gestion des vides :",uiOutput("ChoixGrapheSansNA"),
                                              uiOutput("ChoixGrapheSansVide"),
                                              uiOutput("ChoixGrapheSansNAVar1"),
                                              uiOutput("ChoixGrapheSansVideVar1"),
                                              uiOutput("ChoixGrapheSansNAVar2"),
                                              uiOutput("ChoixGrapheSansVideVar2"))),
                             column(3,
                             wellPanel("Titres :",
                                       textInput("TextTitre","Général (auto. si vide) :", ""),
                                       uiOutput("TitreLegende")))),
                             fluidRow(column(3,
                             uiOutput("ChoixGraphePas"),
                             uiOutput("SelectFacetGrid"),
                             uiOutput("ChoixOrdreGrid"),
                             wellPanel("Mise en forme :",
                                       sliderInput("largeur", "Largeur du graphe (px)", min=100,
                                                    max = 2000, 900),
                                       sliderInput("hauteur","Hauteur du graphe (px)",min=100,
                                                    max = 2000, 550),
                                       numericInput("resolution","Résolution (pour le téléchargement)", 80),

                                       downloadButton("DlGraphe","Télécharger le graphe")
                                       )),
                             column(9,
                                    h4("Graphique :"),
                                    textOutput("Population2"),


                                    plotOutput("Graphique2") )

                             )
                      )
                      ),

             #--------------------------- SUPPRIMER messages d'erreur -------------------------#

             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }")



  )
)
