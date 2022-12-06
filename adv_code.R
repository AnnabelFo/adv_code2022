#Import bibliothèques####
#Gestion des fichiers
library(tidyverse)
library(readxl)
library(stringr)


##adventure of code
##jour1 : trouver la somme max de calories pour un elfe ####

##création liste des ressources de différents elfes
  #import fichier
  chemin <- file.path("~","Travaux_R","adv_code","data_liste1.csv")
  liste <- read.csv2(chemin)

## identifier les elfes par des numéros
  #initialisation
  liste_temp <- liste %>%
    mutate(numero = 99999)
  i<-0
  j<-0
  
  #numéroter les elfes : incrémentation à chaque ligne "nouveau"
  for (i in 0:3000) { 
  ifelse (liste_temp[i,1] == "nouveau",
          {liste_temp[i,2] <- j;
          j = j+1 ;},
          liste_temp[i,2] <- j
          )
  }

## Sommer les calories par elfe et prendre le max
liste_temp <- liste_temp %>%
  filter(calories != "nouveau") %>%
  group_by(numero )%>%
  summarise(somme_cal = sum(as.numeric(calories)))%>%
  ##trouver le total des 3 elfes qui ont le plus de calories
  arrange(desc(somme_cal))

##somme des calories sur la selection sur les 3 max
sum(liste_temp[1:3,2])
  

         
##jour2 : Jeu Pierre Feuille ciseaux ####
#import fichier
chemin <- file.path("~","Travaux_R","adv_code","pfc_data.csv")
liste <- read.csv2(chemin)

# Etoile 1: Trouver la somme totale des différents affrontements
    #Valeurs
    # Pierre (A,X) = 1, feuille (B,Y) = 2, ciseaux(C,Z) = 3
    # Victoire 6, égalité = 3, défaite = 0

#Créer la colonne de score
score <- liste %>% 
  mutate (pt_victoire = case_when(
    adversaire == "A" & moi == "Y" ~ 6,
    adversaire == "A" & moi == "Z" ~ 0,
    adversaire == "B" & moi == "X" ~ 0,
    adversaire == "B" & moi == "Z" ~ 6,
    adversaire == "C" & moi == "X" ~ 6,
    adversaire == "C" & moi == "Y" ~ 0,
    #en cas d'égalité
    TRUE ~ 3
  ) )%>%
  mutate( pt_choix = case_when(
    moi == "X" ~ 1,
    moi == "Y" ~ 2,
    moi == "Z" ~ 3
  )) %>%
  mutate (pt_tot = pt_choix + pt_victoire)

#Score final
score_tot <- sum(score$pt_tot)
score_tot


# Etoile 2: Trouver la somme totale des différents affrontements préconisés
  #Valeurs
  # Pierre (A,X) = 1, feuille (B,Y) = 2, ciseaux(C,Z) = 3
  # X = défaite, Y = égalité, Z= Victoire

#Créer la colonne de score
score <- liste %>% 
  rename(objectif = moi)%>%
  mutate( choix = case_when(
    adversaire == "A" & objectif == "X" ~ "C",
    adversaire == "A" & objectif == "Z" ~ "B",
    adversaire == "B" & objectif == "X" ~ "A",
    adversaire == "B" & objectif == "Z" ~ "C",
    adversaire == "C" & objectif == "X" ~ "B",
    adversaire == "C" & objectif == "Z" ~ "A",
    #au cas où on cherche l'égalité
    TRUE ~ adversaire),
    pt_choix = case_when(
      choix == "A" ~ 1,
      choix == "B" ~ 2,
      choix == "C" ~ 3
    ),
    pt_victoire = case_when(
      objectif == "X" ~ 0,
      objectif == "Y" ~ 3,
      objectif == "Z" ~ 6),
    pt_tot = pt_choix + pt_victoire)

#Score final
score_tot <- sum(score$pt_tot)
score_tot

#Jour 3 : Trouver les priorités des objets situés dans les sacoches ####
  #import fichier
  chemin <- file.path("~","projets_git","adv_code","adv_code2022","data","sacoches_data.csv")
  data <- read.csv2(chemin)  

  ## Etoile 1 ####
  # Séparer la ligne en 2 sacs
  data_modif <- data %>%
    mutate(nb_item = nchar(sac),
           #extrait la première moitié de la chaine de charactères
           sac1 = str_sub(sac,1,nb_item/2),
           #extrait la deuxième moitié de la chaine de charactères
           sac2 = str_sub(sac,nb_item/2+1, nb_item))
  data_modif <- data_modif %>%
     #transformer les sacs en chaine
    mutate( chaine1 = str_extract_all(data_modif$sac1, "[:alpha:]"),
            chaine2 = str_extract_all(data_modif$sac2, "[:alpha:]"))

  # Trouver la lettre commune dans les 2 sacs
  objets_commun <- unlist(mapply(FUN = intersect, data_modif$chaine1,data_modif$chaine2))
 objets_commun
   class(objets_commun)
  objets <- as.data.frame(objets_commun)
  objets
 colnames(objets) <- c("lettre")
 
  # Attribuer une note et sommer
    
    #Créer le vecteur de valeurs des lettres
      val_min = cbind(letters, seq(1,26))
      val_maj = cbind(LETTERS, seq(27,52))
      val_lettres = rbind(val_min, val_maj)
      valeurs_lettres <- as.data.frame(val_lettres)
      colnames( valeurs_lettres ) <- c("lettre","valeur")
      
    #faire une jointure pour récupérer les valeurs des lettres
    jointure <-  left_join(objets,valeurs_lettres)

    #somme des valeurs des objets
    somme <- sum(as.numeric(jointure$valeur))
    somme      
   
    
    ##Etoile 2 : trouver la lettre commune sur 3 sacs ####
    # Transformer le sac en chaine de caratères
    data_modif <- data
    
    #créer un vecteur pour identifier les groupes
    num <- as.data.frame(rep(seq(1,3), times = 100))
    colnames(num) <- c("numero")
    
    #fusion des num et des listes d'objets
    objets_id <- cbind(num, data_modif) 
    
    #arranger le tableau pour faire passer en colonnes les lignes numérotée de 1 à 3
    objets <- objets_id %>%
      mutate(chaine_sac = str_extract_all(data_modif$sac, "[:alpha:]"))%>%
      select(numero, chaine_sac)
      
    objets1 <- objets %>%
      filter(num == 1)%>%
      select(chaine_sac)
    colnames(objets1) <- c("chaine1")
     
    objets2 <- objets %>%
      filter(num == 2)%>%
      select(chaine_sac)
    colnames(objets2) <- c("chaine2")
    
    objets3 <- objets %>%
      filter(num == 3)%>%
      select(chaine_sac)
    colnames(objets3) <- c("chaine3")
    
    objets_tri <- cbind(objets1, objets2, objets3)
    
    #créer une fonction intersect à 3 entrées
    intersect3 <- function(a,b,c){
    out <- intersect(intersect(a,b),c)
    return(out)
    }
    
    
    # Trouver la lettre commune dans chaque groupe de 3 lignes
    objets_communs <- unlist(mapply(FUN = intersect3,
                                     objets_tri$chaine1, objets_tri$chaine2,
                                     objets_tri$chaine3))
    
    objets_communs <- as.data.frame(objets_communs)
    colnames(objets_communs) <- c("lettre")
    
    # Attribuer une note et sommer
    
    #Créer le vecteur de valeurs des lettres
    val_min = cbind(letters, seq(1,26))
    val_maj = cbind(LETTERS, seq(27,52))
    val_lettres = rbind(val_min, val_maj)
    valeurs_lettres <- as.data.frame(val_lettres)
    colnames(valeurs_lettres) <- c("lettre","valeur")
    
    #faire une jointure pour récupérer les valeurs des lettres
    jointure <-  left_join(objets_communs,valeurs_lettres)
    
    #somme des valeurs des objets
    somme <- sum(as.numeric(jointure$valeur))
    somme      
    