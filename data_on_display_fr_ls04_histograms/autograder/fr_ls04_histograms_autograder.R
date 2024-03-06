# Ch03 Ls04 Histogrammes
## Sabina Rodriguez
## 2023-03-06

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")
pacman::p_load(tidyverse,
               praise,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialiser ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 5)   # Mettez le nombre total de questions comme argument `times`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les données ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.malidd <- read.csv(here::here("data/clean/malidd.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Tracer un histogramme montrant la distribution de l'âge (`age_months`) dans `malidd`.
#' Mettez les bordures et le remplissage des barres en "seagreen", et réduisez l'opacité à 40%.

# [backend]
.CHECK_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months)) +
      geom_histogram(fill = "seagreen",
                     color = "seagreen",
                     alpha = 0.4) 
    
    gg_req <- .q1_correct
    gg_ans <- q1
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q1 <- function(){
  'D’abord, fournissez le bon data frame à la couche de données de `ggplot()`.
  Ensuite, placez la variable que vous souhaitez mapper sur votre axe des x à l’intérieur de `mapping = aes()`. 
  Ensuite, ajoutez la bonne fonction de géométrie pour un histogramme.
  Vous devez attribuer les bons arguments esthétiques pour la couleur, le remplissage et la transparence (alpha). Ceux-ci sont des esthétiques FIXES, assurez-vous donc de les placer à l’intérieur de la fonction geom_*() et NON à l’intérieur de la fonction aes().
  Si tout est correct : recherchez des fautes de frappe, des virgules ou des signes plus manquants, ou des crochets/parenthèses non assortis.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_q1 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months)) +
  geom_histogram(fill = "seagreen",
                 color = "seagreen",
                 alpha = 0.4)`' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#En s'appuyant sur le code ci-dessus, visualisez la relation entre le temps du PIB par habitant dans le cadre de données `gap_US` avec à la fois des points et des lignes. Changez la couleur des points selon vos préférences.

# [backend]
.CHECK_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months)) +
      geom_histogram(fill = "seagreen",
                     color = "seagreen",
                     alpha = 0.4) +
      labs(x = "Âge (mois)",
           y = "Nombre d'enfants") 
    
    gg_req <- .q2_correct
    gg_ans <- q2
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q2 <- function(){
  'Les étiquettes des axes dans ggplot2 sont modifiées à l’aide de la fonction labs(), qui a été introduite dans notre leçon sur les graphiques linéaires.
  Le texte de chaque étiquette d’axe doit être entre guillemets. Vérifiez qu’il est identique au texte demandé dans la question.
  Si tout est correct : recherchez des fautes de frappe, des virgules ou des signes plus manquants, ou des crochets/parenthèses non assortis.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_q2 <- function(){
  'ggplot(data = .malidd, 
          mapping = aes(x = age_months)) +
  geom_histogram(fill = "seagreen",
                 color = "seagreen",
                 alpha = 0.4) +
  labs(x = "Âge (mois)",
       y = "Nombre d’enfants")' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Faites un histogramme de la fréquence de la respiration (`freqrespi`), 
#' mesurée en respirations par minute. 
#' Définissez la couleur intérieure sur "indianred3" et la couleur de la bordure sur "lightgray".
#' Diminuez le nombre de bacs jusqu'à ce qu'il n'y ait plus d'intervalles vides. 
#' Vous devriez choisir la valeur la plus élevée des bacs pour lesquels il n'y a pas d'espaces vides.

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = freqrespi)) +
      geom_histogram(fill = "indianred3",
                     color = "lightgray",
                     bins = 20)
    
    gg_req <- .q3_correct
    gg_ans <- q3
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q3 <- function(){
  "D'abord, fournissez le data frame malidd à la couche de données de `ggplot()`.
  Ensuite, attribuez la variable correcte à l'axe des x à l'intérieur de `aes()`. 
  Ensuite, ajoutez la fonction de géométrie appropriée pour un histogramme.
  Vous devez attribuer les bons arguments esthétiques pour la couleur, le remplissage et le nombre de bacs. Ceux-ci sont des esthétiques FIXES, assurez-vous donc de les placer à l'intérieur de la fonction geom_*() et NON à l'intérieur de la fonction aes().
  Pour obtenir le nombre de bacs qui ne laisse pas d'espaces vides, vous devrez essayer différents nombres. Essayez de commencer par un faible nombre de bacs comme 15, et augmentez par de petits incréments.
  Si tout est correct : recherchez des fautes de frappe, des virgules ou des signes plus manquants, ou des crochets/parenthèses non assortis." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q3 <- function(){
  'ggplot(data = malidd, 
       mapping = aes(x = freqrespi)) +
    geom_histogram(fill = "indianred3",
                   color = "lightgray",
                   bins = 20)' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'Créez le même histogramme de freqrespi que dans la dernière question pratique, 
#'mais cette fois, réglez la largeur du bac sur une valeur qui donne 18 bacs. 
#'Ensuite, alignez les barres sur les coupures de l'axe x en ajustant les limites des bacs.

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = freqrespi)) +
      geom_histogram(binwidth = 2,
                     fill = "indianred3",
                     color = "lightgray",
                     boundary = 24)
    
    gg_req <- .q4_correct
    gg_ans <- q4
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q4 <- function(){
  "La variable freqrespi va de 24 à 60. Pour trouver la largeur de bac nécessaire pour obtenir 18 bacs, vous pouvez calculer (60-24)/18.
  Pour aligner les coupures sur les marques de l'axe x, vous pouvez définir l'argument boundary sur la valeur la plus basse." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q4 <- function(){
  'ggplot(data = malidd, 
       mapping = aes(x = freqrespi)) +
    geom_histogram(binwidth = 2,
                   fill = "indianred3",
                   color = "lightgray",
                   boundary = 24)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Tracez l'histogramme de freqrespi avec des coupures de bac allant du 
#' la plus basse valeur de freqrespi à la plus élevée, avec des intervalles de 4.
#' Ensuite, ajustez les coupures de l'axe x en ajoutant une fonction scale_*(). 
#' Définissez la plage de 24 à 60, avec des intervalles de 8.

# [backend]
.CHECK_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = freqrespi)) +
      geom_histogram(fill = "indianred3",
                     color = "lightgray", 
                     binwidth = 4) +
      scale_x_continuous(breaks = seq(24, 60, 8))
    
    gg_req <- .q5_correct
    gg_ans <- q5
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q5 <- function(){
  "Gardez le même remplissage et la même couleur que dans la question pratique précédente.
  Utilisez l'argument binwidth pour définir les intervalles. 
  N'oubliez pas que ce sont des esthétiques FIXES et doivent être placées dans geom_histogram().
  Ensuite, utilisez la fonction seq() pour définir des coupures à l'intérieur de scale_x_continuous()." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q5 <- function(){
  'ggplot(data = malidd, 
       mapping = aes(x = freqrespi)) +
    geom_histogram(fill = "indianred3",
                   color = "lightgray", 
                   binwidth = 4) +
  scale_x_continuous(breaks = seq(24, 60, 8))' -> out
  cat(out)
}
