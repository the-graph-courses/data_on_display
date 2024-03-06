# Ch03 FR Ls07 5NG Graphiques linéaires
## Sabina
## 2024-03-05

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")
pacman::p_load(tidyverse,
               praise,
               gapminder,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialiser ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 8)   # Mettez le nombre total de questions comme argument de `times`

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Charger les données ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gapminder <- gapminder::gapminder

gap_US <- dplyr::filter(gapminder, country == "United States")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Créer un graphique de séries temporelles du PIB par habitant (`gdpPercap`) enregistré dans le data frame `gap_US` en utilisant `geom_line()` pour créer un graphique linéaire.

# [backend]
.CHECK_q1 <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(gap_US, 
             mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line() 
    
    gg_req <- .q1_correct
    gg_ans <- q1
    
    .autograder <<-
      function(){
        if(!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q1 <- function(){
  'D’abord, fournissez le bon data frame à la couche de données de `ggplot()`.
  Ensuite, mettez les variables que vous souhaitez mapper sur votre axe x et votre axe y à l’intérieur de `mapping = aes()`. 
  Ensuite, ajoutez la bonne fonction de géométrie pour un graphique linéaire.
  Si tout cela est correct, recherchez les fautes de frappe, les virgules manquantes ou les crochets non fermés.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_q1 <- function(){
  'ggplot(gap_US, 
          mapping = aes(x = year, 
                        y = gdpPercap)) +
  geom_line()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# En s'appuyant sur le code ci-dessus, visualisez la relation entre le temps et le PIB par habitant du data frame `gap_US` avec à la fois des points et des lignes. Changez la couleur des points selon vos préférences.

# [backend]
.CHECK_q2 <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(gap_US, 
             mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line(lty = "dotdash") +
      geom_point(color = "aquamarine") 
    
    gg_req <- .q2_correct
    gg_ans <- q2
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q2 <- function(){
  'Nous voulons tracer une ligne et des points : vous devez utiliser deux fonctions de géométrie différentes.
  Ensuite, nous voulons rendre la ligne en pointillés et les points "aquamarine" en ajoutant des esthétiques fixes à chaque couche de géométrie.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_q2 <- function(){
  'ggplot(gap_US, 
          mapping = aes(x = year, 
                           y = gdpPercap)) +
      geom_line(lty = "dotdash") +
      geom_point(color = "aquamarine")' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# À l'aide du data frame `gap_mini`, créez un graphique de croissance de la **population** avec ces mappages esthétiques :

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 linetype = country)) +
      geom_line()
    
    gg_req <- .q3_correct
    gg_ans <- q3
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q3 <- function(){
  "Vérifiez les étiquettes des axes pour voir quelles variables sont mappées sur x et y.
  Consultez le guide sur le côté droit du graphique pour voir quelles esthétiques supplémentaires ont été mappées sur `country`. Deux sont affichées dans le graphique.
  Assurez-vous de ne pas ajouter de modifications supplémentaires que la question n'a pas demandées, sinon la fonction CHECK le considérera comme incorrect." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q3 <- function(){
  'ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 linetype = country)) +
      geom_line()' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Ensuite, ajoutez une couche de points au graphique précédent et ajoutez les mappages esthétiques requis pour produire un graphique qui ressemble à ceci :

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 4
    
    .q4_correct <- 
      ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 shape = continent,
                 lty = country)) +
      geom_line() +
      geom_point()
    
    gg_req <- .q4_correct
    gg_ans <- q4
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q4 <- function(){
  "Prenez le bon code de la q3 et ajoutez un mappage supplémentaire pour créer le graphique de la q4.
  Assurez-vous de ne pas ajouter de modifications supplémentaires que la question n'a pas demandées, sinon la fonction CHECK le considérera comme incorrect." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q4 <- function(){
  'ggplot(gap_mini,
             aes(x = year,
                 y = pop,
                 color = country,
                 shape = continent,
                 lty = country)) +
      geom_line() +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Ensuite, ajoutez une couche de points au graphique précédent et ajoutez les mappages esthétiques requis pour produire un graphique qui ressemble à ceci :

# [backend]
.CHECK_q5 <-
  function() {
    .problem_number <<- 5
    
    .q5_correct <- 
      ggplot(data = gap_mini2, 
             mapping = aes(x = year, 
                           y = gdpPercap, 
                           color = country)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = gap_years) +
      scale_y_continuous(breaks = seq(from = 1000, to = 7000, by = 1000))
    
    gg_req <- .q5_correct
    gg_ans <- q5
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q5 <- function(){
  "
  Assurez-vous de ne pas ajouter de modifications supplémentaires que la question n'a pas demandées, sinon la fonction CHECK le considérera comme incorrect." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q5 <- function(){
  'ggplot(data = gap_mini2, 
       mapping = aes(x = year, 
                     y = gdpPercap, 
                     color = country)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = gap_years) +
  scale_y_continuous(breaks = seq(from = 1000, to = 7000, by = 1000))' -> out
  cat(out)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q6 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sous-ensemble suivant `gapminder` pour ne contenir que les lignes de données pour **Ouganda :**
# utilisez **`gap_Uganda`** pour créer un graphique de séries temporelles de la population (**`pop`**) au fil du temps (**`year`**). Transformez l'axe y en une échelle logarithmique, modifiez les points d'échelle pour correspondre à **`gap_years`**, changez la couleur de la ligne en `forestgreen` et la largeur de la ligne en 1 mm.

# [backend]
.CHECK_q6 <-
  function() {
    .problem_number <<- 6
    
    .q6_correct <- 
      ggplot(data = gap_Uganda, aes(x = year, y = pop)) + 
      geom_line(linewidth = 1, color = "forestgreen")+
      scale_x_continuous(breaks = gap_years) +
      scale_y_log10()
    
    gg_req <- .q6_correct
    gg_ans <- q6
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q6 <- function(){
  'D’abord, vérifiez que vos couches essentielles sont le bon data frame (gap_Uganda), les mappages esthétiques et la fonction de géométrie.
   Vous devez effectuer deux modifications d’échelle : les points de l’axe x sont définis pour correspondre aux années dans le jeu de données, 
  et transformez l’échelle de l’axe y en logarithmique (utilisez la fonction d’échelle que nous venons d’apprendre).
   Ajoutez des esthétiques fixes color = "forestgreen" et "linewidth = 1" dans geom_line(), mais assurez-vous de ne pas ajouter de modifications supplémentaires que la question n’a pas demandées, sinon la fonction CHECK le considérera comme incorrect.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_q6 <- function(){
  'ggplot(data = gap_Uganda, mapping = aes(x = year, y = pop)) + 
  geom_line(linewidth = 1, color = "forestgreen")+
  scale_x_continuous(breaks = gap_years) +
  scale_y_log10()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q7 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.CHECK_q7 <-
  function() {
    .problem_number <<- 7
    
    .q7_correct <- 
      ggplot(data = my_gap_mini, 
             mapping = aes(y = lifeExp, 
                           x = year, 
                           color = country)) +
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = gap_years)
    
    gg_req <- .q7_correct
    gg_ans <- q7
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q7 <- function(){
  "
Assurez-vous de ne pas ajouter de modifications supplémentaires que la question n'a pas demandées, sinon la fonction CHECK le considérera comme incorrect." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q7 <- function(){
  'ggplot(data = my_gap_mini, 
          mapping = aes(y = lifeExp, 
                        x = year, 
                        color = country)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = gap_years)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q8 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# [backend]
.CHECK_q8 <-
  function() {
    .problem_number <<- 8
    
    .q8_correct <- 
      ggplot(data = my_gap_mini, 
             mapping = aes(y = lifeExp, 
                           x = year, 
                           color = country)) +
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = gap_years) +
      labs(x = "Année", 
           y = "Longévité",
           title = "Santé & richesse des nations",
           color = "Couleur")
    
    gg_req <- .q8_correct
    gg_ans <- q8
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Incorrect. Votre résultat devrait être un objet ggplot2. Veuillez réessayer."))
        
        # Utiliser compare_ggplots pour vérifier si la réponse est parfaitement correcte
        perfect_match <- suppressWarnings

(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!") ))
        if(!isTRUE(perfect_match)) return(c(value = 0, 
                                            message = "Incorrect. Veuillez consulter l'indice et réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_q8 <- function(){
  "
Assurez-vous de ne pas ajouter de modifications supplémentaires que la question n'a pas demandées, sinon la fonction CHECK le considérera comme incorrect." -> out
  cat(out)
}
# solution de la question
.SOLUTION_q8 <- function(){
  'ggplot(data = my_gap_mini, 
          mapping = aes(y = lifeExp, 
                        x = year, 
                        color = country)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = gap_years) +
  labs(x = "Année", 
       y = "Longévité",
       title = "Santé & richesse des nations",
       color = "Couleur")' -> out
  cat(out)
}
