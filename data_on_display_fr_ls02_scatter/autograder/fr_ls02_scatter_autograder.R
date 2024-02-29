# Data Viz Ls02 Nuages de points
## Laure Vancauwenberghe, Joy Vaz, Kene Nwosu
## 08-07-2022

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
.scores <- rep(-1, times = 6)   # Mettre le nombre total de questions comme argument `times`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les données ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.malidd <- read_csv(here::here("data/clean/malidd.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q1 age_height ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# En utilisant le dataframe `malidd`, créez un nuage de points montrant la relation entre l'âge et la taille (`height_cm`).

# [backend]
.CHECK_age_height <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(data = .malidd,
             mapping = aes(x = age_months, 
                           y = height_cm)) +
      geom_point()
    
    gg_req <- .q1_correct
    gg_ans <- age_height
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_age_height <- function(){
  'Vous pouvez examiner le code du dernier exemple pour vous aider avec cette question.
  Les entrées requises pour une réponse correcte sont :
  (1) argument de données - Avez-vous correctement orthographié le nom du dataframe ?
  (2) mappages x et y - Vérifiez que vous mappez les bonnes variables dans aes().
  (3) fonction geom - Pensez à la forme géométrique dont vous avez besoin pour un nuage de points.
  Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, les signes plus manquants et lisez attentivement les messages d’erreur.'  -> out
  cat(out)
}
# solution de la question
.SOLUTION_age_height <- function(){
  'ggplot(data = malidd,
          mapping = aes(x = age_months, 
                        y = height_cm)) +
  geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q2 v1 age_height_respi (nom incorrect) ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#En utilisant le dataframe `malidd`, créez un nuage de points montrant la relation entre l'âge et la charge virale, et mappez une troisième variable, `freqrespi`, à la couleur :
  
  # [backend]
  .CHECK_age_height_respi <-
    function() {
      .problem_number <<- 2
      
      .q2_correct <- 
        ggplot(data = .malidd, 
               mapping = aes(x = age_months, 
                             y = viral_load)) + 
        geom_point(mapping = aes(color = freqrespi))
      
      gg_req <- .q2_correct
      gg_ans <- age_height_respi
      
      .autograder <<-
        function(){
          if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
          
          # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
          
          
          perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
          
          if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
          if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
        }
      .apply_autograder()
    }
  
  # [backend]
  # créer un indice par question
  .HINT_age_height_respi <- function(){
    'INDICE : Examinez le code d’un exemple précédent où nous avons mapé la taille à la couleur, et faites de même avec la fréquence de respiration.
    Les entrées requises pour une réponse correcte sont :
      (1) argument de données - Avez-vous correctement orthographié le nom du dataframe ?
      (2) mappages x et y - Vérifiez que vous mappez les bonnes variables à x et y à l’intérieur de aes().
      (3) mapping couleur - Vérifiez que vous mappez la bonne variable à la couleur à l’intérieur de aes().
      (4) fonction geom - Pensez aux formes géométriques dont vous avez besoin pour un nuage de points.
    Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, les signes plus manquants, et lisez attentivement les messages d’erreur.'  -> out
    cat(out)
  }
  # solution de la question
  .SOLUTION_age_height_respi <- function(){
    'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = freqrespi))' -> out
    cat(out)
  }
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Q2 v2 age_viral_respi (renommer) ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #En utilisant le dataframe `malidd`, créez un nuage de points montrant la relation entre l'âge et la charge virale, et mappez une troisième variable, `freqrespi`, à la couleur :
  
  # [backend]
  .CHECK_age_viral_respi <-
    function() {
      .problem_number <<- 2
      
      .q2_correct <- 
        ggplot(data = .malidd, 
               mapping = aes(x = age_months, 
                             y = viral_load)) + 
        geom_point(mapping = aes
                   
                   (color = freqrespi))
      
      gg_req <- .q2_correct
      gg_ans <- age_viral_respi
      
      .autograder <<-
        function(){
          if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
          
          # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
          
          
          perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
          
          if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
          if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
        }
      .apply_autograder()
    }
  
  # [backend]
  # créer un indice par question
  .HINT_age_viral_respi <- function(){'INDICE : Examinez le code pour le tracé où nous avons mapé la taille à la couleur, et faites de même avec la fréquence de respiration.
  Les entrées requises pour une réponse correcte sont :
  (1) argument de données - Avez-vous correctement orthographié le nom du dataframe ?
  (2) mappages x et y - Vérifiez que vous mappez les bonnes variables à x et y à l’intérieur de aes().
    (3) mapping couleur - Vérifiez que vous mappez la couleur à l’intérieur de aes() à la bonne variable.
  (4) fonction geom - Pensez aux formes géométriques dont vous avez besoin pour un nuage de points.
  Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, les signes plus manquants, et lisez attentivement les messages d’erreur.'   -> out
  cat(out)
}
# solution de la question
.SOLUTION_age_viral_respi <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, 
                        y = viral_load)) + 
    geom_point(mapping = aes(color = freqrespi))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q3 age_height_fever ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.CHECK_age_height_fever <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(mapping = aes(color = factor(fever)))
    
    gg_req <- .q3_correct
    gg_ans <- age_height_fever
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }
## essayer de résoudre le conflit de fusion 

# [backend]
# créer un indice par question
.HINT_age_height_fever <- function(){
  'INDICE : Examinez le code pour le tracé où nous avons mapé l’allaitement à la couleur, et faites de même avec la fièvre à la place.
  Les entrées requises pour une réponse correcte sont :
  (1) argument de données - Avez-vous correctement orthographié le nom du dataframe ?
  (2) mappages x et y - Vérifiez que vous mappez les bonnes variables à x et y à l’intérieur de aes().
  (3) mapping couleur -  Vérifiez que vous mappez la couleur à l’intérieur de aes() à la bonne variable. Gardez à l’esprit que ggplot traitera la variable binaire `fever` comme une variable continue, mais ici nous voulons que vous lui donniez deux couleurs distinctes.
  (4) fonction geom - Pensez aux formes géométriques dont vous avez besoin pour un nuage de points.
  Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, ou les signes plus manquants.'  -> out
  cat(out)
}
# solution de la question
.SOLUTION_age_height_fever <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(mapping = aes(color = factor(fever)))' -> out
  cat(out)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q4 age_viral_blue ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Créez un nuage de points avec les mêmes variables que l'exemple précédent, mais changez la couleur des points en `cornflowerblue`, augmentez la taille des points à 3mm et réglez l'opacité à 60%.

# [backend]
.CHECK_age_viral_blue <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "cornflowerblue",
                 size = 3,
                 alpha = 0.6) 
    
    gg_req <- .q4_correct
    gg_ans <- age_viral_blue
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_age_viral_blue <- function(){
  'Les entrées requises pour une réponse correcte sont :
  (1) argument de données - Avez-vous correctement orthographié le nom du dataframe ?
  (2) mappages x et y - Vérifiez que vous mappez les bonnes variables à x et y à l’intérieur de aes().
  (3) fonction geom -  Pensez aux formes géométriques dont vous avez besoin pour un nuage de points.
  (4) esthétique couleur fixe -  Vérifiez que vous définissez la couleur sur une valeur fixe à l’EXTÉRIEUR de aes(). Gardez à l’esprit que le nom de la couleur doit être entre guillemets.
  (5) esthétique taille fixe -  Vérifiez que vous définissez la taille des points sur une valeur numérique fixe à l’EXTÉRIEUR de aes().
(6) esthétique alpha fixe pour l’opacité -  Vérifiez que vous définissez alpha sur une valeur numérique fixe entre 0 et 1.
  Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, les signes plus manquants, et lisez attentivement les messages d’erreur.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_age_viral_blue <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, 
                        y = viral_load)) + 
  geom_point(color = "cornflowerblue",
             size = 3,
             alpha = 0.6)' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q5 age_height_2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Créez un nuage de points avec les mêmes variables que l'exemple précédent, mais changez la couleur des points en "steelblue", la taille en 2.5mm, la transparence en 80%, et ajoutez une ligne de tendance avec la méthode de lissage `lm` (modèle linéaire). Pour faire ressortir la ligne de tendance, changez sa couleur en "indianred3".

# [backend]
.CHECK_age_height_2 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8) +
      geom_smooth(method = "lm", color = "indianred3")
    
    gg_req <- .q5_correct
    gg_ans <- age_height_2
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
        
        
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_age_height_2 <- function(){
  'Les entrées requises pour une réponse correcte sont :
  (1) Un dataframe de données, 
  (2) variables x et y
  (3) Deux fonctions geom - une pour les points du nuage de points et une pour une ligne de lissage. 
  (4) esthétique couleur fixe pour les points -  Vérifiez que vous définissez la couleur sur une valeur fixe à l’INTÉRIEUR de la bonne fonction geom. 
  (5) esthétique taille fixe pour les points -  Vérifiez que vous définissez la taille des points sur une valeur numérique fixe à l’EXTÉRIEUR de aes().
  (6) esthétique alpha fixe pour l’opacité des points -  Vérifiez que vous définissez alpha sur une valeur numérique fixe entre 0 et 1.
  (7) méthode fixe pour la ligne de lissage. Gardez à l’esprit que cela pourrait être entre guillemets.
  Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, les signes plus manquants, et lisez attentivement les messages d’erreur.'  -> out
  cat(out)
}
# solution de la question
.SOLUTION_age_height_2 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, 
                        y = height_cm)) + 
  geom_point(color = "steelblue",
             size = 2.5,
             alpha = 0.8) +
  geom_smooth(method = "lm", color = "indianred3")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## age_height_3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Recréez le tracé ci-dessus, mais cette fois, changez la forme pour pointer vers des rectangles inclinés (numéro 23), et mappez la variable de température corporelle (`temp`) à la couleur de remplissage.


# [backend]
.CHECK_age_height_3 <-
  function() {
    .problem_number <<- 6
    
    .q6_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "lm", color = "indianred3")
    
    gg_req <- .q6_correct
    gg_ans <- age_height_3
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = "Correct !"))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez vérifier l'indice et réessayer."))
        
        
     

 }
    .apply_autograder()
  }
  
# [backend]
# créer un indice par question
.HINT_age_height_3 <- function(){
  'Les entrées requises pour une réponse correcte sont :
  (1) Un dataframe de données, 
  (2) variables x et y
  (3) Deux fonctions geom - une pour les points du nuage de points et une pour une ligne de lissage. 
  (4) esthétique couleur fixe pour les points -  Vérifiez que vous définissez la couleur sur une valeur fixe à l’INTÉRIEUR de la bonne fonction geom. 
  (5) esthétique taille fixe pour les points -  Vérifiez que vous définissez la taille des points sur une valeur numérique fixe à l’EXTÉRIEUR de aes().
  (6) esthétique alpha fixe pour l’opacité des points -  Vérifiez que vous définissez alpha sur une valeur numérique fixe entre 0 et 1.
  (7) forme fixe pour les points -  Vérifiez que vous définissez la forme sur une valeur numérique fixe à l’INTÉRIEUR de aes(). 
  (8) mapping variable pour la couleur de remplissage -  Vérifiez que vous mappez la bonne variable à la couleur de remplissage.
  (9) méthode fixe pour la ligne de lissage. Gardez à l’esprit que cela pourrait être entre guillemets.
  Enfin, recherchez les fautes de frappe, les parenthèses non fermées, les virgules manquantes, les signes plus manquants, et lisez attentivement les messages d’erreur.'  -> out
  cat(out)
}
# solution de la question
.SOLUTION_age_height_3 <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "lm", color = "indianred3")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fin ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~