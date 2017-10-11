#=====================================================
# Packages
#=====================================================

library(shiny)
library(questionr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

options(shiny.maxRequestSize=30*1024^2)

#=========================================================
# Fichier server
#=========================================================

shinyServer(function(input, output, session) { 
  
  # A/ Importation du tableau de données
  #===========================================================
  
  # Données importées (adaptation d'explore-data, Paris Descartes) :
  output$donnees.fichier.ui <- renderUI({
    list(
      fileInput("donnees.fichier.input", "Choisir le fichier :"),
      radioButtons("donnees.fichier.header", 
                   "Noms de variables en 1ère ligne :",
                   c("oui", "non")),
      radioButtons("donnees.fichier.sep", 
                   "Séparateur de champs :", 
                   c("point-virgule" = ";", 
                     "virgule" = ",", 
                     "espace" = " ", 
                     "tabulation" = "\t")),
      radioButtons("donnees.fichier.dec", 
                   "Séparateur de décimales :",
                   c("point" = ".", "virgule" = ",")),
      radioButtons("donnees.fichier.enc",
                   "Encodage des caractères :",
                   c("UTF-8 (par défaut sur Linux/Mac)" = "UTF-8",
                     "Windows-1252 (par défaut sur Windows)" = "WINDOWS-1252")),
      uiOutput("donnees.fichier.ok")
    )
    
  })
  
  file_name <- reactive({
    inFile <- input$donnees.fichier.input
    
    if (is.null(inFile))
      return("NULL")
    
    return (stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
  })
  donnees_entree <-reactive({
    if (is.null(input$donnees.fichier.input)) return (NULL)
    don <- NULL
    try({
      don <- read.table(
        input$donnees.fichier.input$datapath, 
        header = input$donnees.fichier.header == "oui", 
        sep = input$donnees.fichier.sep,
        dec = input$donnees.fichier.dec, 
        fileEncoding = input$donnees.fichier.enc,
        stringsAsFactors = FALSE)
    }, silent = TRUE)
    don <- unique(don)
    don
  })
  
  
  
  
  # taille et str du tableau de départ :  
  
  output$Dimensions <- renderText(
    if (is.null(input$donnees.fichier.input)) return ("")
    else {
      paste("Tableau constitué de", ncol(donnees_entree()),
            "colonnes et de", nrow(donnees_entree()),"lignes.
            Détail des variables :")
      
    })
  
  
  output$Resume <- renderTable({
    tmp <- donnees_entree()
    if (is.null(tmp)) {return (NULL)}else{
      
      donnees_entree <- data.frame( Variable = names(tmp[1]),
                                    Type = class(tmp[,1]),
                                    NbreValeursDiff = nrow(unique(tmp[1])))
      for (i in (2:ncol(tmp))) {
        donnees_entree <-rbind(donnees_entree, data.frame( Variable = names(tmp[i]),
                                                           Type = class(tmp[,i]),
                                                           NbreValeursDiff = nrow(unique(tmp[i]))))
      }
      donnees_entree
    }
  })
  
  
  
  # Listes déroulantes dynamiques :
  
  
  ## Modalités du 1er critère :
  Choose_Field <- reactive({
    Var1 <- input$Variable1
    donnees_entree <- donnees_entree()
    if (is.null(input$Variable1)) return (NULL)
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var1]))))
  })
  
  ## Modalités du 2ème critère :
  Choose_Field2 <- reactive({
    if (is.null(input$Variable2)) return (NULL)
    donnees_entree <- donnees_entree()
    Var2 <- input$Variable2
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var2]))))
  })
  
  ## Modalités du 3ème critère :
  Choose_Field3 <- reactive({
    donnees_entree <- donnees_entree()
    Var3 <- input$Variable3
    if (is.null(input$Variable3)) return (NULL)
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var3]))))
  })
  
  ## Modalités du 4ème critère :
  Choose_Field4 <- reactive({
    donnees_entree <- donnees_entree()
    Var4 <- input$Variable4
    if (is.null(input$Variable4)) return (NULL)
    Choose_Field <- as.list(c("",unique(as.character(donnees_entree[,Var4]))))
  })
  
  ## Variables conservées dans l'ACM (pour var illustratives) :
  Choose_Illus <- reactive ({
    Vars <- input$VarPourACM
    donnees_entree <- test()
    validate(need(length(input$VarPourACM)>1, " "))
    if (is.null(input$VarPourACM)) return (NULL)
    Choose_Illus <- data.frame(donnees_entree[,Vars])
  })
  

  
  
  # Elements de sélection / choix / input
  #=================================================
  
  # Sélection des individus :
  
  ## Choix de l'identifiant (pour jointures des différents tableaux "critères"):
  output$SelectID <- renderUI({
    selectInput("ID", "Choix de l'identifiant (doit être unique) :",
                choices=c(" ",names(donnees_entree())) , selected = NULL)  
  }) 
  
 
  # Sélection des variables, opérateurs et modalités de chaque critère :
  
  output$SelectVar1 <- renderUI ({
    selectInput("Variable1", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })

    output$Select <- renderUI({
    
    Variable1 <- input$Variable1
    Donnees <- donnees_entree()
    validate(
      need(input$Variable1 !=" "  , "Choisir une variable")
    )
    
    if (class(Donnees[,Variable1])=="character" |
        class(Donnees[,Variable1])=="logical"){
      selectInput("Modalite1", "Modalité :",
                  choices=Choose_Field() , selected = NULL)  
    }
    else if (class(Donnees[,Variable1])=="integer" |
             class(Donnees[,Variable1])=="numeric"){
      sliderInput("Modalite1", "Modalité :",
                  min=min(Donnees[,Variable1], na.rm=T),
                  max=max(Donnees[,Variable1], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  output$SelectVar2 <- renderUI ({
    
    selectInput("Variable2", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  output$Select2 <- renderUI({
    Variable2 <- input$Variable2
    Donnees <- donnees_entree()
    
    
    validate(
      need(input$Variable2 !=" "  , "Choisir une variable")
    )
    if (class(Donnees[,Variable2])=="character" |
        class(Donnees[,Variable2])=="logical"){
      selectInput("Modalite2", "Modalité :",
                  choices=Choose_Field2() , selected = NULL)  
    }
    else if (class(Donnees[,Variable2])=="integer" |
             class(Donnees[,Variable2])=="numeric"){
      sliderInput("Modalite2", "Modalité :",
                  min=min(Donnees[,Variable2], na.rm=T),
                  max=max(Donnees[,Variable2], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  output$SelectVar3 <- renderUI ({
    selectInput("Variable3", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  output$Select3 <- renderUI({
    
    validate(
      need(input$Variable3 !=" "  , "Choisir une variable")
    )
    Variable3 <- input$Variable3
    Donnees <- donnees_entree()
    if(is.null(input$Variable3)) return(NULL)
    if (class(Donnees[,Variable3])=="character" |
        class(Donnees[,Variable3])=="logical"){
      selectInput("Modalite3", "Modalité :",
                  choices=Choose_Field3() , selected = NULL)  
    }
    else if (class(Donnees[,Variable3])=="integer" |
             class(Donnees[,Variable3])=="numeric"){
      sliderInput("Modalite3", "Modalité :",
                  min=min(Donnees[,Variable3], na.rm=T),
                  max=max(Donnees[,Variable3], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  output$SelectVar4<- renderUI ({
    selectInput("Variable4", "Variable :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  output$Select4 <- renderUI({
    
    Variable4 <- input$Variable4
    Donnees <- donnees_entree()
    
    validate(
      need(input$Variable4 !=" "  , "Choisir une variable")
    )
    
    
    if (class(Donnees[,Variable4])=="character" |
        class(Donnees[,Variable4])=="logical"){
      selectInput("Modalite4", "Modalité :",
                  choices=Choose_Field4() , selected = NULL)  
    }
    else if (class(Donnees[,Variable4])=="integer" |
             class(Donnees[,Variable4])=="numeric"){
      sliderInput("Modalite4", "Modalité :",
                  min=min(Donnees[,Variable4], na.rm=T),
                  max=max(Donnees[,Variable4], na.rm=T), round=1, step=.5, value=0)
    }
  })
  
  
  # B/ Création de la table selon les sous-ensembles définis :
  #===========================================================
  
  
  Critere1 <- reactive({
    Var1 <- input$Variable1
    Moda1 <- input$Modalite1
    BiosFinal <- donnees_entree()
    BiosFinal<-  switch(input$Operateur1,
                        " " = BiosFinal,
                        "=" =  BiosFinal[BiosFinal[,Var1] == Moda1,], 
                        "diff. de" =  BiosFinal[BiosFinal[,Var1] != Moda1,],
                        ">" = BiosFinal[BiosFinal[,Var1] > Moda1,],
                        ">=" = BiosFinal[BiosFinal[,Var1] >= Moda1,],
                        "<" = BiosFinal[BiosFinal[,Var1] < Moda1,],
                        "<=" = BiosFinal[BiosFinal[,Var1] <= Moda1,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  Critere2 <- reactive({
    Var2 <- input$Variable2
    Moda2 <- input$Modalite2
    BiosFinal <- donnees_entree()
    BiosFinal <- switch(input$Operateur2,
                        " " = BiosFinal,
                        "=" =  BiosFinal[BiosFinal[,Var2] == Moda2,], 
                        "diff. de" =  BiosFinal[BiosFinal[,Var2] != Moda2,],
                        ">" = BiosFinal[BiosFinal[,Var2] > Moda2,],
                        ">=" = BiosFinal[BiosFinal[,Var2] >= Moda2,],
                        "<" = BiosFinal[BiosFinal[,Var2] < Moda2,],
                        "<=" = BiosFinal[BiosFinal[,Var2] <= Moda2,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  Critere3 <- reactive({
    Var3 <- input$Variable3
    Moda3 <- input$Modalite3
    BiosFinal <- donnees_entree()
    BiosFinal<- switch(input$Operateur3,
                       " " = BiosFinal,
                       "=" =  BiosFinal[BiosFinal[,Var3] == Moda3,], 
                       "diff. de" =  BiosFinal[BiosFinal[,Var3] != Moda3,],
                       ">" = BiosFinal[BiosFinal[,Var3] > Moda3,],
                       ">=" = BiosFinal[BiosFinal[,Var3] >= Moda3,],
                       "<" = BiosFinal[BiosFinal[,Var3] < Moda3,],
                       "<=" = BiosFinal[BiosFinal[,Var3] <= Moda3,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  Critere4 <- reactive({
    Var4 <- input$Variable4
    Moda4 <- input$Modalite4
    BiosFinal <- donnees_entree()
    BiosFinal<- switch(input$Operateur4,
                       " " = BiosFinal,
                       "=" =  BiosFinal[BiosFinal[,Var4] == Moda4,], 
                       "diff. de" =  BiosFinal[BiosFinal[,Var4] != Moda4,],
                       ">" = BiosFinal[BiosFinal[,Var4] > Moda4,],
                       ">=" = BiosFinal[BiosFinal[,Var4] >= Moda4,],
                       "<" = BiosFinal[BiosFinal[,Var4] < Moda4,],
                       "<=" = BiosFinal[BiosFinal[,Var4] <= Moda4,])
    BiosFinal <- BiosFinal[!(str_detect(row.names(BiosFinal),"NA")),] 
  })
  
  
  test <- reactive({
    Critere1 <- Critere1()
    Critere2 <- Critere2()
    ID <- input$ID
    Crit <- data.frame(Critere2[,ID]) 

    names(Crit)[1]<- ID
    don<-switch(input$OperateurMid,
                " " = Critere1,
                "OU" =  unique(rbind(Critere1, Critere2)), 
                "ET" =  unique(merge(Critere1, Crit, by=ID)))

  })
  test2 <- reactive({
    Critere3 <- Critere3()
    Critere2 <- test()
    ID <- input$ID
    Crit <- data.frame(Critere3[,ID])
    names(Crit)[1]<- ID
    switch(input$OperateurMid2,
           " " = Critere2,
           "OU" =  unique(rbind(Critere3, Critere2)), 
           "ET" =  unique(merge(Critere2, Crit, by=ID)))
  })
  
  
  test3 <- reactive({
    Critere4 <- Critere4()
    Critere3 <- test2()
    ID <- input$ID
    Crit <- data.frame(Critere4[,ID])

    names(Crit)[1]<- ID
    switch(input$OperateurMid3,
           " " = Critere3,
           "OU" =  unique(rbind(Critere4, Critere3)), 
           "ET" =  unique(merge(Critere3, Crit, by=ID)))

  })

#====================================================
# Choix des variables et de leurs types pour les croisements
#====================================================

  
  
  
  # Selection des variables :
  
  output$SelectVarCrois1 <- renderUI ({
 
    validate( 
      need( input$ID != " ", "Vérifier l'importation (onglet 1)")
    ) 
    
    selectInput("VariableCrois1", "1ère variable (en lignes, si 2 variables) :",
                choices=as.list(c(" ",names(test3()))),selected=" ")
  }) 
  
  
  output$SelectVarCrois2 <- renderUI ({
    validate( 
      need( input$ID != " ", "Vérifier l'importation (onglet 1)")
    ) 
    
    selectInput("VariableCrois2", "2ème Variable (en colonnes, si 2 variables) :",
                choices=as.list(c(" ",names(test3()))),selected=" ")
  })
  
  # Type des variables
  
  TypeVar1 <- reactive({
    
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    validate(
      need(input$VariableCrois1!=" " , "Choisir une variable")
    )
    
    class(TableCrois2[,VariableCrois1])
  })  
  TypeVar2 <- reactive({
    
    TableCrois2 <- TableCrois2()
    VariableCrois2 <-  input$VariableCrois2
    validate(
      need(input$VariableCrois2!=" " , "Choisir une variable")
    )
    
    class(TableCrois2[,VariableCrois2])
  })
  
  output$TypeVar1 <- renderText({
    validate(
      need(input$VariableCrois1!=" " , "Choisir une variable")
    )
    paste0("Type importé de la variable : ",TypeVar1(), ". Pour modifier :")})
  
  
  output$TypeVar2 <- renderText({
    validate(
      need(input$VariableCrois2!=" " , "Choisir une variable")
    )
    paste0("Type importé de la variable : ",TypeVar2(), ". Pour modifier :")})
  
  TableCrois1 <- reactive({
    Table <- test3()
    VariableCrois1 <- input$VariableCrois1
    Table[,VariableCrois1] <- switch(input$TypeVarCrois1,
                                     "Pas de modification" = Table[,VariableCrois1] ,
                                     "Qualitative" = as.character ( Table[,VariableCrois1]),
                                     "Quantitative - entier" =  as.integer (Table[,VariableCrois1]), 
                                     "Quantitative - réel" =  as.numeric (Table[,VariableCrois1]),
                                     "Logique" = as.logical (Table[,VariableCrois1]))
    Table <- Table[!(str_detect(row.names(Table),"NA")),] 
  })  
  
  
  
  TableCrois2 <- reactive({
    Table <- TableCrois1()
    VariableCrois2 <- input$VariableCrois2
    if (VariableCrois2 == " ") Table <- Table
    else {
    Table[,VariableCrois2] <- switch(input$TypeVarCrois2,
                                     "Pas de modification" = Table[,VariableCrois2] ,
                                     "Qualitative" = as.character ( Table[,VariableCrois2]),
                                     "Quantitative - entier" =  as.integer (Table[,VariableCrois2]), 
                                     "Quantitative - réel" =  as.numeric (Table[,VariableCrois2]),
                                     "Logique" = as.logical (Table[,VariableCrois2]))
    Table <- Table[!(str_detect(row.names(Table),"NA")),] 
    }
  })  
  
  output$ChoixReprez <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
  if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical") ){
      selectInput("Reprez", "Affichage :",
                  choices=as.list(c("Effectifs","% colonnes","% lignes","% totaux")),selected=" ")
      
    }
    
  })
  
  output$h6_Reprez <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical") ){
      h6("Choix de l'affichage des valeurs")
      
    }
    
  })
  
  output$ChoixTrier <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        VariableCrois2 == " ") {
      checkboxInput("Trier", "Tri décroissant", value=FALSE)
    }
    
  })
  
  output$ChoixAjoutEns <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if (((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical")) |
      ((class(TableCrois2[,VariableCrois1])=="character" |
          class(TableCrois2[,VariableCrois1])=="logical") &
         (class(TableCrois2[,VariableCrois2])=="integer" |
            class(TableCrois2[,VariableCrois2])=="numeric")) ){
      checkboxInput("AjoutEns", "Afficher l'ensemble (var. 1)", value=FALSE)
    }
    
  })
  
  
  output$ChoixAfficherNA <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical")) {
      checkboxInput("AfficherNA", "Inclure les <NA>", value=FALSE)
    }
    
  })
  
  Tableau <- reactive ({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    Reprez <- input$Reprez
    Trier <- ifelse (input$Trier == TRUE, "dec","")
    AfficherNA <- input$AfficherNA
    
    # 1er cas : Une seule variable quali
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        VariableCrois2 == " ") {
      t<- freq(TableCrois2[,VariableCrois1], sort=Trier,total = T) 
      t$Modalites <- row.names(t)
      t <- t[,c(4,1:3)]
      names(t)[1] <- VariableCrois1
      names(t)[2] <- "Effectifs"
      t
    }

    
    # 2ème cas : une seule variable quanti :
    
    else if ((class(TableCrois2[,VariableCrois1])=="integer" |
              class(TableCrois2[,VariableCrois1])=="numeric") &
             VariableCrois2 == " ") {
      
      t <- TableCrois2 %>% 
        dplyr::summarise (Min = min(eval(parse(text=VariableCrois1)), na.rm= T), 
                          Quartile1 = quantile(eval(parse(text=VariableCrois1)), .25, na.rm = T),
                          Mediane = median(eval(parse(text=VariableCrois1)), na.rm = T),
                          Moyenne = round(mean(eval(parse(text=VariableCrois1)), na.rm = T),1), 
                          Quartile3 = quantile(eval(parse(text=VariableCrois1)), .75, na.rm = T),
                          Max = max(eval(parse(text=VariableCrois1)), na.rm = T), 
                          NbreNA = sum(is.na(eval(parse(text=VariableCrois1)))))
      t
    }
    
    # 3ème cas : les deux variables sont quali / logiques :
    else if  ((class(TableCrois2[,VariableCrois1])=="character" |
        class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical") ) 
      
      {
      if (Reprez == "% colonnes") {
      if (AfficherNA == TRUE) {
        t<-as.data.frame.matrix(cprop(table(TableCrois2[,VariableCrois1], 
                                                 TableCrois2[,VariableCrois2], useNA = "always")))
        
        t$temp_VarCrois1 <- row.names(t)
        t <- renomme.variable(t, "","<NA>")
        t <- renomme.variable(t, "V1","")
        t <- t[, c(ncol(t),1:(ncol(t)-1))]
        names(t)[1] <- VariableCrois1
        t
      } else {
        t<-as.data.frame.matrix(cprop(table(TableCrois2[,VariableCrois1], 
                                                 TableCrois2[,VariableCrois2])))
        
        t$temp_VarCrois1 <- row.names(t)
        t <- renomme.variable(t, "V1","")
        t <- t[, c(ncol(t),1:(ncol(t)-1))]
        names(t)[1] <- VariableCrois1
        t
        
      }
      
      } else if (Reprez == "Effectifs") {
        if (AfficherNA == TRUE) {
        t<-as.data.frame.matrix(addmargins(table(TableCrois2[,VariableCrois1], 
                                  TableCrois2[,VariableCrois2], useNA = "always")))
        
        t$temp_VarCrois1 <- row.names(t)
        t <- renomme.variable(t, "","<NA>")
        t <- renomme.variable(t, "V1","")
        t <- t[, c(ncol(t),1:(ncol(t)-1))]
        names(t)[1] <- VariableCrois1
        t
        } else {
          t<-as.data.frame.matrix(addmargins(table(TableCrois2[,VariableCrois1], 
                                         TableCrois2[,VariableCrois2])))
          
          t$temp_VarCrois1 <- row.names(t)
          t <- renomme.variable(t, "V1","")
          t <- t[, c(ncol(t),1:(ncol(t)-1))]
          names(t)[1] <- VariableCrois1
          t
          
        }
      } else if (Reprez == "% lignes") {
        
        if (AfficherNA == TRUE) {
          t<-as.data.frame.matrix(rprop(table(TableCrois2[,VariableCrois1], 
                                              TableCrois2[,VariableCrois2], useNA = "always")))
          
          t$temp_VarCrois1 <- row.names(t)
          t <- renomme.variable(t, "","<NA>")
          t <- renomme.variable(t, "V1","")
          t <- t[, c(ncol(t),1:(ncol(t)-1))]
          names(t)[1] <- VariableCrois1
          t
        } else {
          t<-as.data.frame.matrix(rprop(table(TableCrois2[,VariableCrois1], 
                                              TableCrois2[,VariableCrois2])))
          
          t$temp_VarCrois1 <- row.names(t)
          t <- renomme.variable(t, "V1","")
          t <- t[, c(ncol(t),1:(ncol(t)-1))]
          names(t)[1] <- VariableCrois1
          t
          
        }
        
      } else if (Reprez == "% totaux") {
        if (AfficherNA == TRUE) {
          t<-as.data.frame.matrix(prop(table(TableCrois2[,VariableCrois1], 
                                              TableCrois2[,VariableCrois2], useNA = "always")))
          
          t$temp_VarCrois1 <- row.names(t)
          t <- renomme.variable(t, "","<NA>")
          t <- renomme.variable(t, "V1","")
          t <- t[, c(ncol(t),1:(ncol(t)-1))]
          names(t)[1] <- VariableCrois1
          t
        } else {
          t<-as.data.frame.matrix(prop(table(TableCrois2[,VariableCrois1], 
                                              TableCrois2[,VariableCrois2])))
          
          t$temp_VarCrois1 <- row.names(t)
          t <- renomme.variable(t, "V1","")
          t <- t[, c(ncol(t),1:(ncol(t)-1))]
          names(t)[1] <- VariableCrois1
          t
          
        }
        
      }
    }
    
    else if ((class(TableCrois2[,VariableCrois1])=="integer" |
              class(TableCrois2[,VariableCrois1])=="numeric") &
             (class(TableCrois2[,VariableCrois2])=="integer" |
              class(TableCrois2[,VariableCrois2])=="numeric")) {}
    
    
    # 4ème cas : Les deux variables sont quanti :
    
    # 5ème cas : une var quali et une quanti :
    
    else if ((class(TableCrois2[,VariableCrois1])=="character" |
              class(TableCrois2[,VariableCrois1])=="logical") &
             (class(TableCrois2[,VariableCrois2])=="integer" |
              class(TableCrois2[,VariableCrois2])=="numeric")) {
      
      t <- TableCrois2 %>% 
        group_by(eval(parse(text=VariableCrois1))) %>% 
        dplyr::summarise (Min = min(eval(parse(text=VariableCrois2)), na.rm= T), 
                          Quartile1 = quantile(eval(parse(text=VariableCrois2)), .25, na.rm = T),
                          Mediane = median(eval(parse(text=VariableCrois2)), na.rm = T),
                          Moyenne = round(mean(eval(parse(text=VariableCrois2)), na.rm = T),1), 
                          Quartile3 = quantile(eval(parse(text=VariableCrois2)), .75, na.rm = T),
                          Max = max(eval(parse(text=VariableCrois2)), na.rm = T), 
                          NR = sum(is.na(eval(parse(text=VariableCrois2)))))
      names(t)[1]<- VariableCrois1
      tt <- TableCrois2 %>% 
        dplyr::summarise (Min = min(eval(parse(text=VariableCrois2)), na.rm= T), 
                          Quartile1 = quantile(eval(parse(text=VariableCrois2)), .25, na.rm = T),
                          Mediane = median(eval(parse(text=VariableCrois2)), na.rm = T),
                          Moyenne = round(mean(eval(parse(text=VariableCrois2)), na.rm = T),1), 
                          Quartile3 = quantile(eval(parse(text=VariableCrois2)), .75, na.rm = T),
                          Max = max(eval(parse(text=VariableCrois2)), na.rm = T), 
                          NR = sum(is.na(eval(parse(text=VariableCrois2)))))
      
      tt[,VariableCrois1] <- "Ensemble"
      tt <- tt[, c(ncol(tt), (1:ncol(tt)-1))]
      t <- rbind(t, tt)
      
      t
    }
    
    
    
  })
  
  output$Tableau <- renderTable ({
    
    Tableau()
  })
  
  output$ChoixGrapheOrdonne <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") )
    {
      checkboxInput("GrapheOrdonne", "Graphique ordonné", value=FALSE)
    }
    
  })
  output$ChoixGrapheSansNA <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        VariableCrois2 == " ") {
      checkboxInput("GrapheSansNA", "Enlever les <NA>", value=FALSE)
    }
    
  })
  output$ChoixGrapheSansNAVar1 <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical" |
         class(TableCrois2[,VariableCrois2])=="integer" |
         class(TableCrois2[,VariableCrois2])=="numeric" 
        )) {
      checkboxInput("GrapheSansNAVar1", "Enlever les <NA> (var. 1)", value=FALSE)
    }
    
  })
  output$ChoixGrapheSansNAVar2 <- renderUI({
      TableCrois2 <- TableCrois2()
      VariableCrois1 <-  input$VariableCrois1
      VariableCrois2 <-  input$VariableCrois2
      
      if ((class(TableCrois2[,VariableCrois1])=="character" |
           class(TableCrois2[,VariableCrois1])=="logical") &
          (class(TableCrois2[,VariableCrois2])=="character" |
           class(TableCrois2[,VariableCrois2])=="logical" 
          ))
      {
        checkboxInput("GrapheSansNAVar2", "Enlever les <NA> (var. 2)", value=FALSE)
      }
      
  })
    
  output$ChoixGrapheSansVide <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        VariableCrois2 == " ")
      {
      checkboxInput("GrapheSansVide", "Enlever les vides", value=FALSE)
    }
    
  })
  output$ChoixGrapheSansVideVar1 <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical" |
         class(TableCrois2[,VariableCrois2])=="integer" |
         class(TableCrois2[,VariableCrois2])=="numeric" 
        ))
    {
      checkboxInput("GrapheSansVideVar1", "Enlever les vides (var. 1)", value=FALSE)
    }
    
  })
  output$ChoixGrapheSansVideVar2 <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        (class(TableCrois2[,VariableCrois2])=="character" |
         class(TableCrois2[,VariableCrois2])=="logical" 
        ))
    {
      checkboxInput("GrapheSansVideVar2", "Enlever les vides (var. 2)", value=FALSE)
    } })
  
  output$ChoixGraphePas <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
     if (((class(TableCrois2[,VariableCrois1])=="integer" |
              class(TableCrois2[,VariableCrois1])=="numeric") &
             VariableCrois2 == " ") |
         ((class(TableCrois2[,VariableCrois1])=="character" |
           class(TableCrois2[,VariableCrois1])=="logical") &
          (class(TableCrois2[,VariableCrois2])=="integer" |
           class(TableCrois2[,VariableCrois2])=="numeric") ) ) {
       numericInput("GraphePas", "Choix du pas :", 1)
    }  })  
  
  output$ChoixGrapheAffichage <- renderUI({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    

  
    # Pour 1 var quali :
    
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        VariableCrois2 == " ") {
      selectInput("GrapheAffichage", "Affichage :",
                  choices=as.list(c("Effectifs","Pourcentages")),selected="Effectifs")
    }
    
    # Pour une var quanti :
    
    else if ((class(TableCrois2[,VariableCrois1])=="integer" |
                  class(TableCrois2[,VariableCrois1])=="numeric") &
                 VariableCrois2 == " ") {
      
      selectInput("GrapheAffichage", "Affichage :",
                  choices=as.list(c("Boxplot","Histogramme (eff.)","Histogramme (pourc.)")),
                  selected="Boxplot")
     
      
    # Pour deux var quanti :
      
    } 
    else if  ((class(TableCrois2[,VariableCrois1])=="integer" |
                     class(TableCrois2[,VariableCrois1])=="numeric") &
                    (class(TableCrois2[,VariableCrois2])=="integer" |
                     class(TableCrois2[,VariableCrois2])=="numeric") ) {
      selectInput("GrapheAffichage", "Affichage :",
                  choices=as.list(c("Effectifs","Pourcentages (de var. 1)")),selected="Effectifs")
      
    }
    
    # Pour deux var quali :
    else if  ((class(TableCrois2[,VariableCrois1])=="character" |
               class(TableCrois2[,VariableCrois1])=="logical") &
              (class(TableCrois2[,VariableCrois2])=="character" |
               class(TableCrois2[,VariableCrois2])=="logical") ) {
      selectInput("GrapheAffichage", "Affichage :",
                  choices=as.list(c("Effectifs","Pourcentages (de var. 1)")),selected="Effectifs")
      
    }
    # Pour une var quali et une var quanti :
    else if  ((class(TableCrois2[,VariableCrois1])=="character" |
               class(TableCrois2[,VariableCrois1])=="logical") &
              (class(TableCrois2[,VariableCrois2])=="integer" |
               class(TableCrois2[,VariableCrois2])=="numeric") ) {
      selectInput("GrapheAffichage", "Affichage :",
                  choices=as.list(c("BoxPlot","Histogramme (eff.)")),selected="BoxPlot")
      
    }
    
  })
  

  # Graphique
  #========================================================
  
  Graphique <- reactive ({
    TableCrois2 <- TableCrois2()
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    GrapheOrdonne <- input$GrapheOrdonne
    GrapheSansNA <- input$GrapheSansNA
    GrapheSansVide <- input$GrapheSansVide
    GrapheSansNAVar1 <- input$GrapheSansNAVar1
    GrapheSansNAVar2 <- input$GrapheSansNAVar2
    GrapheSansVideVar1 <- input$GrapheSansVideVar1
    GrapheSansVideVar2 <- input$GrapheSansVideVar2
    TextTitre <- input$TextTitre
    GrapheAffichage <- input$GrapheAffichage
    AjoutEns <- input$AjoutEns
    GraphePas <- input$GraphePas
    
    # 1er cas : Une seule variable quali
    #===========================================
    
    # Condition :
    if ((class(TableCrois2[,VariableCrois1])=="character" |
         class(TableCrois2[,VariableCrois1])=="logical") &
        VariableCrois2 == " ") {
      
    # Options graphiques :
      
      if (GrapheSansNA == TRUE) {
        TableCrois2 <- TableCrois2[!(is.na(TableCrois2[,VariableCrois1])),]
      } 
      if (GrapheSansVide == TRUE) {
        TableCrois2 <- TableCrois2[!TableCrois2[,VariableCrois1]=="",]
      }
      
      if (TextTitre == "") {
        TextTitreDef <- paste0 ("Distribution de la variable ",VariableCrois1)
      }
      if (TextTitre != "") {
        TextTitreDef <- TextTitre
      }
      if (GrapheOrdonne == TRUE) {
        
        TableCrois2[,VariableCrois1] <- reorder(TableCrois2[,VariableCrois1],
                                                TableCrois2[,VariableCrois1], 
                                                function(x) -length(x))
      }
    
      # Création du graphique :
      
      if (GrapheAffichage == "Effectifs") {

    
        
      ggplot (data=TableCrois2, aes (x=eval(parse(text=VariableCrois1)), y=..count..)) +  
        geom_bar()+ # Type de représentation : diagramme en barres
        labs(x=VariableCrois1, y="Effectifs") + # Titres des axes x et y
        ggtitle(TextTitreDef)+ # Titre du graphique (\n : saut de ligne)
          theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                axis.text.y = element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                title = element_text(size=16, face="bold"),
                plot.title=element_text(hjust = 0.5)) 
    }
     else if (GrapheAffichage == "Pourcentages") {
        
          
          ggplot (data=TableCrois2, aes (x=eval(parse(text=VariableCrois1)), 
                                         y=((..count..)/sum(..count..)*100))) +  
            geom_bar()+ # Type de représentation : diagramme en barres
            labs(x=VariableCrois1, y="Pourcentages") + # Titres des axes x et y
            ggtitle(TextTitreDef)+ # Titre du graphique (\n : saut de ligne)
            theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                  axis.text.y = element_text(size=12),
                  axis.title=element_text(size=14,face="bold"),
                  title = element_text(size=16, face="bold"),
                  plot.title=element_text(hjust = 0.5)) # Angle et position des items de l'axe x
        }
      }
    
    
    
    # 2ème cas : une seule variable quanti :
    #===========================================

    # Conditions :
    
    else if ((class(TableCrois2[,VariableCrois1])=="integer" |
              class(TableCrois2[,VariableCrois1])=="numeric") &
             VariableCrois2 == " ") {
      
      # Options graphiques :
      
      if (TextTitre == "") {
        TextTitreDef <- paste0 ("Dispersion de ",VariableCrois1)
      }
      if (TextTitre != "") {
        TextTitreDef <- TextTitre
      }
      
      # Création du graphique :
      if (GrapheAffichage == "Boxplot") {
        ggplot (data=TableCrois2, aes (x="Ensemble",y=eval(parse(text=VariableCrois1)))) +  
          geom_boxplot() + 
          ggtitle(TextTitreDef) +
          ylab(VariableCrois1)+
          xlab("")+
        theme(
              title = element_text(size=16, face="bold"),
              plot.title=element_text(hjust = 0.5)) 
        
      }
      
      else if (GrapheAffichage == "Histogramme (eff.)" ){
          
          ggplot (data=TableCrois2, aes (x=eval(parse(text=VariableCrois1)))) +  
          geom_histogram(binwidth=GraphePas, colour="black",na.rm=T) +
          xlab(VariableCrois1) +
          ylab("Effectifs") +
          ggtitle(TextTitreDef)  +
          theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                axis.text.y = element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                title = element_text(size=16, face="bold"),
                plot.title=element_text(hjust = 0.5)) 
          
          
      }
      
      else if (GrapheAffichage == "Histogramme (pourc.)") {
        
       
        ggplot (data=TableCrois2, aes (x=eval(parse(text=VariableCrois1)),
                                       y= (..count../sum(..count..)*100)
        )) +  
          geom_histogram( binwidth=GraphePas, colour="black", na.rm=T) +
          
        xlab(VariableCrois1) +
          ylab("Pourcentage (%)") +
          ggtitle(TextTitreDef)  +
          
          theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                axis.text.y = element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                title = element_text(size=16, face="bold"),
                plot.title=element_text(hjust = 0.5)) 
        
      }
        
    }
    
    # 3ème cas : les deux variables sont quali / logiques :
    #=============================================================
    
    # Conditions :
    
    else if  ((class(TableCrois2[,VariableCrois1])=="character" |
               class(TableCrois2[,VariableCrois1])=="logical") &
              (class(TableCrois2[,VariableCrois2])=="character" |
               class(TableCrois2[,VariableCrois2])=="logical") ) 
      {  
    # Options graphiques :
  
      if (AjoutEns == TRUE) {
      
      TableCrois2demi <- TableCrois2
      TableCrois2demi[,VariableCrois1]<- "Ensemble"
      TableCrois3 <- rbind(TableCrois2, TableCrois2demi)
      
      TableCrois3[,VariableCrois1]<-forcats::fct_relevel(TableCrois3[,VariableCrois1],
                                                         "Ensemble", after = Inf)
      
    }
      if (AjoutEns == FALSE) {
        
        TableCrois3 <- TableCrois2
        
      } 
      
      if (TextTitre == "") {
        TextTitreDef <- paste0 ("Distribution de ",VariableCrois2,
                                " en fonction de ", VariableCrois1)
      }
      if (TextTitre != "") {
        TextTitreDef <- TextTitre
      }
      
    if (GrapheOrdonne == TRUE) {
      
      TableCrois3[,VariableCrois2] <- reorder(TableCrois3[,VariableCrois2],
                                              TableCrois3[,VariableCrois2], 
                                              function(x) -length(x))
    }
      
      
      if (GrapheSansNAVar1 == TRUE) {
        TableCrois3 <- TableCrois3[!(is.na(TableCrois3[,VariableCrois1])),]
      } 
      if (GrapheSansNAVar2 == TRUE) {
        TableCrois3 <- TableCrois3[!(is.na(TableCrois3[,VariableCrois2])),]
      } 
      if (GrapheSansVideVar1 == TRUE) {
        TableCrois3 <- TableCrois3[!TableCrois3[,VariableCrois1]=="",]
      }
      if (GrapheSansVideVar2 == TRUE) {
        TableCrois3 <- TableCrois3[!TableCrois3[,VariableCrois2]=="",]
      }
      
    # Création du graphique :
      
      if (GrapheAffichage == "Effectifs") {
       
          
          
          ggplot (data=TableCrois3, aes (x=eval(parse(text=VariableCrois1)),
                                         y=..count..,
                                         fill = eval(parse(text=VariableCrois2)))) +  
            geom_bar( colour="black", position="dodge")+ # Type de représentation : diagramme en barres
            labs(x=VariableCrois1, y="Effectifs") + # Titres des axes x et y
            ggtitle(TextTitreDef)+ # Titre du graphique (\n : saut de ligne)
            scale_fill_brewer(name=VariableCrois2, palette = "Spectral")  +
            theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                  axis.text.y = element_text(size=12),
                  axis.title=element_text(size=14,face="bold"),
                  legend.text=element_text(size=12),
                  title = element_text(size=16, face="bold"),
                  plot.title=element_text(hjust = 0.5)) # Angle et position des items de l'axe x
       
        
       
        }
      else if (GrapheAffichage == "Pourcentages (de var. 1)") {
        
          
          
          TableCrois4 <- TableCrois3 %>% group_by (eval(parse(text=VariableCrois1)),
                                                   eval(parse(text=VariableCrois2))) %>% 
            dplyr::summarise (n=n())%>%  dplyr::mutate(percent = (n / sum(n)*100))
          names(TableCrois4)[1]<- VariableCrois1
          names(TableCrois4)[2]<- VariableCrois2
          ggplot (data=TableCrois4, aes (x=eval(parse(text=VariableCrois1)),
                                         y=percent,
                                         fill = eval(parse(text=VariableCrois2)))) +  
            geom_bar( colour="black", position="dodge", stat="identity")+ # Type de représentation : diagramme en barres
            labs(x=VariableCrois1, y="Pourcentage (%)") + # Titres des axes x et y
            ggtitle(TextTitreDef)+ # Titre du graphique (\n : saut de ligne)
            scale_fill_brewer(name=VariableCrois2, palette = "Spectral")  +
            theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                  axis.title=element_text(size=14,face="bold"),
                  axis.text.y = element_text(size=12),
                  legend.text=element_text(size=12),
                  title = element_text(size=16, face="bold"),
                  plot.title=element_text(hjust = 0.5)) # Angle et position des items de l'axe x
          
        
      }
    }
    
    # 4ème cas : Les deux variables sont quanti :
    #========================================================   
    
    
    else if ((class(TableCrois2[,VariableCrois1])=="integer" |
              class(TableCrois2[,VariableCrois1])=="numeric") &
             (class(TableCrois2[,VariableCrois2])=="integer" |
              class(TableCrois2[,VariableCrois2])=="numeric")) {}
    
    
    
    # 5ème cas : une var quali et une quanti :
    #============================================================
    
    # Conditions : 
    else if ((class(TableCrois2[,VariableCrois1])=="character" |
              class(TableCrois2[,VariableCrois1])=="logical") &
             (class(TableCrois2[,VariableCrois2])=="integer" |
              class(TableCrois2[,VariableCrois2])=="numeric")) {
  
   # Options graphiques :
      
      if (AjoutEns == TRUE) {
        
        TableCrois2demi <- TableCrois2
        TableCrois2demi[,VariableCrois1]<- "Ensemble"
        TableCrois2 <- rbind(TableCrois2, TableCrois2demi)
        
        TableCrois2[,VariableCrois1]<-forcats::fct_relevel(TableCrois2[,VariableCrois1],
                                                           "Ensemble", after = Inf)
        
      }
      if (AjoutEns == FALSE) {
        
        TableCrois2 <- TableCrois2
        
      } 
      
      if (TextTitre == "") {
        TextTitreDef <- paste0 ("Dispersion de ",VariableCrois2,
                                " en fonction de ", VariableCrois1)
      }
      if (TextTitre != "") {
        TextTitreDef <- TextTitre
      }
      
      if (GrapheOrdonne == TRUE) {
        
        TableCrois2[,VariableCrois1] <- reorder(TableCrois2[,VariableCrois1],
                                                TableCrois2[,VariableCrois1], 
                                                function(x) -length(x))
      }
      
      if (GrapheSansNAVar1 == TRUE) {
        TableCrois2 <- TableCrois2[!(is.na(TableCrois2[,VariableCrois1])),]
      } 
      if (GrapheSansVideVar1 == TRUE) {
        TableCrois2 <- TableCrois2[!TableCrois2[,VariableCrois1]=="",]
      }
      
      
  # Création du graphique :
      
     if (GrapheAffichage == "BoxPlot") {
       
     
      ggplot (data=TableCrois2, aes ( x=eval(parse(text=VariableCrois1)), 
                                  y=eval(parse(text=VariableCrois2)), 
                                  fill=eval(parse(text=VariableCrois1)))) +
        geom_boxplot( colour="black") +
        ggtitle(TextTitreDef)+
        labs(y=VariableCrois2,x="") + 
         scale_fill_brewer(name=VariableCrois2, palette = "Spectral")  +
       theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
             axis.text.y = element_text(size=12),
             axis.title=element_text(size=14,face="bold"),
             legend.text=element_text(size=12),
             title = element_text(size=16, face="bold"),
             plot.title=element_text(hjust = 0.5))
       
     }
      else if (GrapheAffichage == "Histogramme (eff.)") {
        
        ggplot (data=TableCrois2, aes (x=eval(parse(text=VariableCrois2)),
                                       fill =eval(parse(text=VariableCrois1)) )) +  
          geom_histogram(binwidth=GraphePas, colour="black",na.rm=T, position = "dodge") +
          xlab(VariableCrois2) +
          ylab("Effectifs") +
          ggtitle(TextTitreDef)  +
          scale_fill_brewer(name=VariableCrois1, palette = "Spectral")  +
          theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1,size=12),
                axis.text.y = element_text(size=12),
                legend.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                title = element_text(size=16, face="bold"),
                plot.title=element_text(hjust = 0.5)) 
        
      }
      
    }
    
    
    
  })
  
  output$Graphique <- renderPlot ({
    
    Graphique()
  })
  
  
  # C/ EN SORTIE : Tables et graphiques
  #========================================================
  
  # Téléchargement du tableau croisé :
  output$PasDID <- downloadHandler( 
    filename=function() {
      paste0("TableAvecID_",Sys.Date(),".csv")
    },
    content = function(file) {
      
      donnees_entree <- donnees_entree()
      donnees_entree$ID <- row.names(donnees_entree)
      donnees_entree <- donnees_entree[, c(ncol(donnees_entree), 1:ncol(donnees_entree)-1)]
      write.csv2(donnees_entree, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
    }
  )
  
  output$DlTable <- downloadHandler( 
    
    
    filename=function() {
    VariableCrois1 <-  input$VariableCrois1
    VariableCrois2 <-  input$VariableCrois2
    
      paste0("Table_",VariableCrois1, "_", ifelse(is.na(VariableCrois2), "",VariableCrois2) ,".csv")
    },
    content = function(file) {
      
      Tableau <- Tableau()
      
      write.csv2(Tableau, file, fileEncoding = "UTF-8", na = "", row.names = FALSE , dec=",")
    }
  )


  output$DlGraphe <- downloadHandler(
    filename=function() {
      VariableCrois1 <-  input$VariableCrois1
      VariableCrois2 <-  input$VariableCrois2
      
      paste0("Graphe_",VariableCrois1, "_", ifelse(is.na(VariableCrois2), "",VariableCrois2) ,".png")
    },
    content = function(file) {
      
      largeur <- input$largeur
      hauteur <- input$hauteur
      
      png(file,  width = largeur, height = hauteur, units = "px")
      Graphe <- Graphique()
      
      print(Graphe)
      dev.off()
    },
    contentType = "image/png"
  )
  
 

  
  
  Population <- reactive ({
    
    Variable1 <- input$Variable1
    Modalite1 <- input$Modalite1
    Operateur1 <- input$Operateur1
    OperateurMid <- input$OperateurMid
    Variable2 <- input$Variable2
    Modalite2 <- input$Modalite2
    Operateur2 <- input$Operateur2
    OperateurMid2 <- input$OperateurMid2
    Variable3 <- input$Variable3
    Modalite3 <- input$Modalite3
    Operateur3 <- input$Operateur3
    OperateurMid3 <- input$OperateurMid3
    Variable4 <- input$Variable4
    Modalite4 <- input$Modalite4
    Operateur4 <- input$Operateur4
    
    if (Variable1 == " ") { paste0("Ensemble")} else{ 
    paste(Variable1, Operateur1, Modalite1, OperateurMid,
                        Variable2, Operateur2, Modalite2, OperateurMid2,
                        Variable3, Operateur3, Modalite3,OperateurMid3,
                        Variable4, Operateur4, Modalite4,
                        sep=" ")
    }
   
  })
  
  output$Population <- renderText({
    paste0("Population : ",Population())
  })
  output$Population2 <- renderText({
    paste0("Population : ",Population())
  })
  
  })



