# Ch03 Ls06 Boîtes à moustaches
## Sabina Rodriguez
## 2024-03-06

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")
pacman::p_load(tidyverse,
               gapminder,
               praise,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialisation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Mettez le nombre total de questions comme argument `times`

set.seed(1024)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les données ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.gapminder <- gapminder::gapminder

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Tracer un histogramme montrant la distribution de l'âge (`age_months`) dans `malidd`. 
#' Définissez les bordures et le remplissage des barres sur "seagreen" et réduisez l'opacité à 40%.

# [backend]
.CHECK_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(gapminder, aes(continent, gdpPercap, fill = continent)) +
      geom_boxplot(linewidth = 1) 
    
    gg_req <- .q1_correct
    gg_ans <- q1
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat doit être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# Créer un indice par question
.HINT_q1 <- function(){
  'D’abord, fournissez le bon data frame à la couche de données de `ggplot()`.
  Ensuite, mettez les variables que vous voulez mapper sur vos axes x et y à l’intérieur de `mapping = aes()`. 
  Ensuite, ajoutez la bonne fonction de géométrie pour un boxplot.
  Vous devez attribuer les arguments esthétiques corrects pour la couleur de remplissage (fill) et la largeur de ligne (linewidth). 
  Rappelez-vous que les esthétiques qui sont MAPPÉES à une VARIABLE vont à l’intérieur de aes(), mais
  les esthétiques qui sont définies à une valeur FIXE doivent aller directement à l’intérieur de la fonction geom_*().
  Si tout cela est correct : recherchez les fautes de frappe, les virgules manquantes ou les signes plus, ou les crochets non fermés.' -> out
  cat(out)
}
# Solution de la question
.SOLUTION_q1 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot(linewidth = 1)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' En vous basant sur votre code de la dernière question, ajoutez une fonction **`scale_*()`** 
#' qui transforme l'axe y en une échelle **logarithmique**.
#' 
# [backend]
.CHECK_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = gapminder,
             mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10() 
    
    gg_req <- .q2_correct
    gg_ans <- q2
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat doit être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# Créer un indice par question
.HINT_q2 <- function(){
  'Utilisez le code correct de la question précédente, et ajoutez une nouvelle couche avec un signe plus.
Pensez à la fonction scale_*() dont vous avez besoin pour transformer l’axe en logarithme. Cela a été enseigné dans la leçon sur les graphiques linéaires. 
  Si tout cela est correct : recherchez les fautes de frappe, les virgules manquantes ou les signes plus, ou les crochets non fermés.' -> out
cat(out)
}

# Solution de la question
.SOLUTION_q2 <- function(){
  'ggplot(data = gapminder,
  mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Créez le boxplot montrant la distribution du PIB par habitant pour chaque continent, 
#' comme vous l'avez fait dans la question pratique 2. Conservez le remplissage, la largeur de ligne et l'échelle de ce tracé.
#' Maintenant, **reordonnez** les boîtes par **moyenne** du `gdpPercap`, dans un ordre **décroissant**.

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(gapminder, 
             aes(x = reorder(continent, -gdpPercap),
                 y = gdpPercap,
                 fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10()
    
    gg_req <- .q3_correct
    gg_ans <- q3
    gg_req_alt <- ggplot(gapminder, 
                         aes(x = reorder(continent, -gdpPercap, mean),
                             y = gdpPercap,
                             fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10()
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat doit être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match1 <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        perfect_match2 <- suppressWarnings(compare_ggplots(gg_req_alt, gg_ans))
        if(isTRUE(perfect_match1) | isTRUE(perfect_match2)) return(c(value = 1, message = paste("Correct !")))
        if(!isTRUE(perfect_match1) & !isTRUE(perfect_match2)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
        
      }
    .apply_autograder()
  }

# Créer un indice par question
.HINT_q3 <- function(){
  'Utilisez le code correct de la question pratique précédente, et modifiez la cartographie esthétique x.
  Vous devrez utiliser la fonction reorder() et spécifier deux arguments dans cet ordre :
  (1) La variable catégorielle à réordonner (votre variable d’axe x)
(2) La variable numérique par laquelle vous voulez réordonner
Pour trier les boîtes dans le boxplot dans l’ordre DÉCROISSANT, ajoutez un signe moins à la variable numérique.
  N’oubliez pas de conserver toutes les couches et les arguments précédents de la question 2.
Si tout cela est correct : recherchez les fautes de frappe, les virgules manquantes ou les signes plus, ou les crochets non fermés.' -> out
  cat(out)
}

# Solution de la question
.SOLUTION_q3 <- function(){
  'ggplot(data = gapminder,
          mapping = aes(
            x = reorder(continent, -gdpPercap), 
            y = gdpPercap, 
            fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' En vous basant sur le code de la question précédente, ajoutez des **étiquettes** à votre tracé.
#' -   Définissez le **titre principal** sur "Variation du PIB par habitant à travers les continents (1952-2007)"
#' -   Changez le **titre de l'axe x** en "Continent", et
#' -   Changez le **titre de l'axe y** en "Revenu par personne (USD)".

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(data = gapminder,
             mapping = aes(
               x = reorder(continent, -gdpPercap), 
               y = gdpPercap, 
               fill = continent)) +
      geom_boxplot(linewidth = 1) +
      scale_y_log10() +
      labs(title = "Variation du PIB par habitant à travers les continents (1952-2007)",
           x = "Continent",
           y = "Revenu par personne (USD)") 
    
    gg_req <- .q4_correct
    gg_ans <- q4
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat doit être un objet ggplot2. Veuillez réessayer."))
        
        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# Créer un indice par question
.HINT_q4 <- function(){
  'Utilisez le code correct de la question pratique précédente, et ajoutez une nouvelle couche avec un signe plus.
  Utilisez la fonction labs() pour changer les trois étiquettes demandées dans la question.
  Assurez-vous qu’elles correspondent exactement au texte de la question (copiez et collez-les pour minimiser les chances d’erreur).
  N’oubliez pas de conserver toutes les couches et les arguments précédents des questions pratiques, et ne faites pas de changements qui ne sont pas demandés dans la question.
  Si tout cela est correct : recherchez les fautes de frappe, les virgules manquantes ou les signes plus, ou les crochets non fermés.' -> out
  cat(out)
}

# Solution de la question
.SOLUTION_q4 <- function(){
  'ggplot(data = gapminder,
          mapping = aes(
            x = reorder(continent, -gdpPercap), 
            y = gdpPercap, 
            fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Variation du PIB par habitant à travers les continents (1952-2007)",
       x = "Continent",
       y = "Revenu par personne (USD)")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' -   Prenez votre graphique de la question pratique 4, et ajoutez une couche de points éparpillés.

## [backend]
#.CHECK_q5 <-
#  function() {
#    .problem_number <<- 5
#    
#    .q5_correct <- 
#      ggplot(data = gapminder,
#        mapping = aes(
#          x = reorder(continent, -gdpPercap), 
#          y = gdpPercap, 
#          fill = continent)) +
#      geom_boxplot(linewidth = 1) +
#      scale_y_log10() +
  #      labs(title = "Variation du PIB par habitant à travers les continents (1952-2007)",
  #        x = "Continent",
  #        y = "Revenu par personne (USD)") + 
  #      geom_jitter()
  #    
  #    gg_req <- .q5_correct
  #    gg_ans <- q5
  #    
  #    .autograder <<-
  #      function(){
  #        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat doit être un objet ggplot2. Veuillez réessayer."))
#        
#        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
#        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
#        
#        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
#        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
#      }
#    .apply_autograder()
#  }

# Fonction de vérification temporaire
.CHECK_q5 <- function(){
  'Malheureusement, ce graphique ne peut pas être vérifié automatiquement en raison
  de l’aléatoire introduit par la fonction geom_jitter(). 
  Veuillez utiliser l’indice pour vous aider à répondre à cette question, et
  comparez votre graphique à la réponse montrée dans la vidéo de la leçon.
  Si vous ne pouvez pas reproduire la réponse, exécutez `.SOLUTION_Q5()` 
  pour obtenir la réponse.' -> out
  cat(out)
}

# Créer un indice par question
.HINT_q5 <- function(){
  'Utilisez le code correct de la question pratique précédente, et ajoutez une nouvelle couche avec un signe plus.
  Choisissez la fonction ggplot correcte dont vous avez besoin pour ajouter une couche de points éparpillés.
  N’oubliez pas de conserver toutes les couches et les arguments précédents des questions pratiques, et ne faites pas de changements qui ne sont pas demandés dans la question.
  Si tout cela est correct : recherchez les fautes de frappe, les virgules manquantes ou les signes plus, ou les crochets non fermés.' -> out
  cat(out)
}

# Solution de la question
.SOLUTION_q5 <- function(){
  'ggplot(data = gapminder,
          mapping = aes(
            x = reorder(continent, -gdpPercap), 
            y = gdpPercap, 
            fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Variation du PIB par habitant à travers les continents (1952-2007)",
       x = "Continent",
       y = "Revenu par personne (USD)") + 
  geom_jitter()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' -   Adaptez votre graphique pour rendre les points transparents à 45 % et changez la largeur du jitter à 0,3 mm.
#' 

# [backend]
#.CHECK_q6 <-
#  function() {
#    .problem_number <<- 6
#    
#    .q6_correct <- 
#      ggplot(data = gapminder, 
#        mapping = aes(x = reorder(continent, -gdpPercap),
#          y = gdpPercap,
#          fill = continent)) +
#      geom_boxplot(linewidth = 1) +
#      scale_y_log10() +
#      labs(title = "Variation du PIB par habitant à travers les continents (1952-2007)",
#        x = "Continent",
#        y = "Revenu par personne (USD)") + 
#      geom_jitter(width = 0.3, alpha = 0.55)
#    
#    gg_req <- .q6_correct
#    gg_ans <- q6
#    
#    .autograder <<-
#      function(){
#        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat doit être un objet ggplot2. Veuillez réessayer."))
#        
#        # Utilisez compare_ggplots pour vérifier si la réponse est parfaitement correcte
#        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
#        
#        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct !") ))
#        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
#      }
#    .apply_autograder()
#  }

# Fonction de vérification temporaire
.CHECK_q6 <- function(){
  'Malheureusement, ce graphique ne peut pas être vérifié automatiquement en raison
de l’aléatoire introduit par la fonction geom_jitter(). 
  Veuillez utiliser l’indice pour vous aider à répondre à cette question, et
comparez votre graphique à la réponse montrée dans la vidéo de la leçon.
Si vous ne pouvez pas reproduire la réponse, exécutez `.SOLUTION_Q6()` 
pour obtenir la réponse.' -> out
  cat(out)
}

# Créer un indice par question
.HINT_q6 <- function(){
  'Utilisez le code correct de la question pratique précédente, et ajoutez une nouvelle couche avec un signe plus.
Utilisez les arguments corrects de la fonction geom_jitter() pour rendre les points transparents à 45% et changer la largeur du jitter à 0,3 mm.
N’oubliez pas de conserver toutes les couches et les arguments précédents des questions pratiques, et ne faites pas de changements qui ne sont pas demandés dans la question.
  Si tout cela est correct : recherchez les fautes de frappe, les virgules manquantes ou les signes plus, ou les crochets non fermés.' -> out
cat(out)
}

# Solution de la question
.SOLUTION_q6 <- function(){
  'ggplot(data = gapminder, 
  mapping = aes(x = reorder(continent, -gdpPercap),
    y = gdpPercap,
    fill = continent)) +
  geom_boxplot(linewidth = 1) +
  scale_y_log10() +
  labs(title = "Variation du PIB par habitant à travers les continents (1952-2007)",
    x = "Continent",
    y = "Revenu par personne (USD)") + 
  geom_jitter(width = 0.3, alpha = 0.45)' -> out
  cat(out)
}
