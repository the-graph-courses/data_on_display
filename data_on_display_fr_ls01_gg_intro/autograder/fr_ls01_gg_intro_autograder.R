##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Chargement des packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load_gh("KO112/KO")

#pacman::p_load(tidyverse,
#               OpenImageR,
#               here,
#               png,
#               jpeg,
#               svglite)
#

pacman::p_load(tidyverse,
               digest,
               here,
               praise)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fonction ggplot_digest ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fonction digest personnalisée pour ggplot
## Joy Vaz
## 2022-11-17

#' Fonction pour générer des empreintes de hachage de graphiques ggplot.
#' Destiné à être utilisé dans des quiz de visualisation de données. 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fonction ggplot_digest ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot_digest <- function(myplot) {
  # Créer un nom de fichier temporaire, enregistrer le graphique, digérer le fichier et supprimer le fichier temporaire
  plotfile <- tempfile(pattern = "ggplot_", fileext = ".png")
  suppressWarnings(suppressMessages(ggplot2::ggsave(filename = plotfile, plot = myplot, type = "cairo", device = "png")))
  plot_crypt <- digest::digest(file = plotfile)
  file.remove(plotfile)
  return(plot_crypt)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Chargement des données ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Charger nigerm dans un environnement caché
.my.env <- new.env()

load(here("data/clean/nigerm_cases_rgn.RData"),
     envir = .my.env)

.nigerm <- local(nigerm, envir = .my.env)

# Créer nigerm04
.nigerm04 <- .nigerm %>%
  # filtrer le dataframe pour inclure uniquement les données de 1996
  filter(year == 1996)  %>% 
  # supprimer la colonne de l'année
  select(-year)

# Créer nigerm04
.nigerm04 <- .nigerm %>%
  # filtrer le dataframe pour inclure uniquement les données de 2004
  filter(year == 2004)  %>% 
  # supprimer la colonne de l'année
  select(-year)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialisation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 4)   # Mettre le nombre total de questions comme argument `times`



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1: nigerm04_scatter ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' En utilisant le dataframe `nigerm04`, écrivez du code `ggplot` qui créera un 
#' nuage de points affichant la relation entre `cases` sur l'axe des y et 
#' `week` sur l'axe des x.

# [backend]
.CHECK_nigerm04_scatter <-
  function() {
    .problem_number <<- 1
    
    .q1_correct <- 
      ggplot(data = .nigerm04,
             mapping = aes(x = week, 
                           y = cases)) +
      geom_point()
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_scatter)) return(c(value = -1, message = "Votre résultat doit être un objet ggplot2."))
        
        # test 1
        # que les données utilisées sont correctes
        .q1_test1 <- all.equal(
          target = as_tibble(nigerm04_scatter$data), 
          current = as_tibble(.q1_correct$data))
        
        # test 2
        # que l'apprenant a utilisé geom_point()
        .q1_test2 <- any(stringr::str_detect(capture.output(nigerm04_scatter$layers), 
                                             "geom_point"))
        # test 3
        # vérifier la correspondance x
        .q1_test3 <- "* `x` -> `week`" %in% capture.output(nigerm04_scatter$mapping)
        
        # test 4
        # vérifier la correspondance y
        .q1_test4 <- "* `y` -> `cases`" %in% capture.output(nigerm04_scatter$mapping)
        
        if (isTRUE(!(.q1_test1))) 
          return(c(value = 0, message = "Vérifiez que vous tracez les bonnes données."))
        if (isTRUE(!(.q1_test2))) 
          return(c(value = 0, message = "Assurez-vous d'utiliser la fonction géométrique `geom_point()`."))
        if (isTRUE(!(.q1_test3 && .q1_test4))) 
          return(c(value = 0, message = "Vérifiez vos arguments de mappage pour x et y : mettez-vous les bonnes variables ?"))
        if (isTRUE(.q1_test1 && .q1_test2 && .q1_test3 && .q1_test4)) 
          return(c(value = 1, message = "Correct !"))
        # faux
        else
          return(c(value = 0, message = "Mauvaise réponse. Veuillez réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_nigerm04_scatter <- function(){
  'Tout d’abord, identifiez quel dataframe fournir à la couche de données.
  Ensuite, placez les variables que vous souhaitez mapper sur votre axe x et votre axe y à l’intérieur de `mapping = aes()`. 
  Ensuite, réfléchissez à la fonction de géométrie dont vous avez besoin pour un nuage de points.
  Si tout cela est correct, recherchez les fautes de frappe, les virgules manquantes ou les crochets non fermés.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_nigerm04_scatter <- function(){
  'ggplot(data = nigerm04,
          mapping = aes(x = week, 
                        y = cases)) +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2: nigerm04_bar ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Utilisez le dataframe `nigerm04` pour créer un graphique à barres des cases hebdomadaires avec la 
#' fonction `geom_col()`. Mappez `cases` sur l'axe des y et `week` sur l'axe des x.

# [backend]
.CHECK_nigerm04_bar <-
  function() {
    .problem_number <<- 2
    
    .q2_correct <- 
      ggplot(data = .nigerm04, 
             mapping = aes(x = week, 
                           y = cases)) + 
      geom_col()
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_bar)) return(c(value = -1, message = "Votre résultat doit être un objet ggplot2."))
        
        # test 1
        # que les données utilisées sont correctes
        .q2_test1 <- all.equal(
          target = as_tibble(nigerm04_bar$data), 
          current = as_tibble(.q2_correct$data))
        
        # test 2
        # que l'apprenant a utilisé geom_point()
        .q2_test2 <- any(stringr::str_detect(capture.output(nigerm04_bar$layers), 
                                             "geom_col"))
        # test 3
        # vérifier la correspondance x
        .q2_test3 <- "* `x` -> `week`" %in% capture.output(nigerm04_bar$mapping)
        
        # test 4
        # vérifier la correspondance y
        .q2_test4 <- "* `y` -> `cases`" %in% capture.output(nigerm04_bar$mapping)
        
        if (isTRUE(!(.q2_test1))) return(c(value = 0, message = "! Vérifiez que vous tracez le bon dataframe."))
        if (isTRUE(!(.q2_test2))) return(c(value = 0, message = "! N'oubliez pas d'utiliser la fonction géométrique ggplot2 `geom_col()`."))
        
        
        # KENE EDIT:
        if (ggplot_digest(.q2_correct) == ggplot_digest(nigerm04_bar)) 
          return(c(value = 1, message = "Correct !"))
        
        # 
        # 
        # if (isTRUE(!(.q2_test3 && .q2_test4))) return(c(value = 0, message = "! Check your mapping arguments for x and y: are you putting the right variables?"))
        # if (isTRUE(.q2_test1 && .q2_test2 && .q2_test3 && .q2_test4 && .q2_test5)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Mauvaise réponse. Veuillez réessayer."))
      }
    .apply_autograder()
  }

# [backend]
# créer un indice par question
.HINT_nigerm04_bar <- function(){
  'Tout d’abord, identifiez quel dataframe fournir à la couche de données.
  Ensuite, placez les variables que vous souhaitez mapper sur votre axe x et votre axe y à l’intérieur de `mapping = aes()`. 
  Ensuite, pensez à la fonction de géométrie dont vous avez besoin pour ce graphique à barres (la même que celle que nous avons utilisée plus tôt dans cette leçon).
  Si tout cela est correct, recherchez les fautes de frappe, les virgules manquantes ou les crochets non fermés.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_nigerm04_bar <- function(){
  'ggplot(data = nigerm04, 
          mapping = aes(x = week, 
                        y = cases)) + 
     geom_col()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3: nigerm04_line ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Utilisez le dataframe `nigerm04` pour créer un graphique linéaire des cases hebdomadaires, coloré
#'  par `region`. La fonction de géométrie pour un graphique linéaire s'appelle `geom_line()`. Mappez
#'  `cases` sur l'axe des y, `week` sur l'axe des x, et `region` à la couleur.

# [backend]
.CHECK_nigerm04_line <-
  function() {
    .problem_number <<- 3
    
    .q3_correct <- 
      ggplot(data = .nigerm04, 
             mapping = aes(x = week, 
                           y = cases,
                           color = region)) + 
      geom_line()
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_line)) return(c(value = -1, message = "Votre résultat doit être un objet ggplot2."))
        
        # test 1
        # que les données utilisées sont correctes
        .q3_test1 <- all.equal(
          target = as_tibble(nigerm04_line$data), 
          current = as_tibble(.q3_correct$data))
        
        # test 2
        # que l'apprenant a utilisé geom_point()
        .q3_test2 <- any(stringr::str_detect(capture.output(nigerm04_line$layers), 
                                             "geom_line"))
        # test 3
        # vérifier la correspondance x
        .q3_test3 <- "* `x` -> `week`" %in% capture.output(nigerm04_line$mapping)
        
        # test 4
        # vérifier la correspondance y
        .q3_test4 <- "* `y` -> `cases`" %in% capture.output(nigerm04_line$mapping)
        
        # test 5
        # vérifier l'argument de couleur: orthographe britannique
        .q3_test5 <- any(stringr::str_detect(capture.output(nigerm04_line$layers), 
                                             "colour = ~region"))
        
        if (isTRUE(!(.q3_test1))) return(c(value = 0, message = "! Vérifiez que vous tracez les bonnes données."))
        
        # KENE EDIT:
        if (ggplot_digest(.q3_correct) == ggplot_digest(nigerm04_line)) return(c(value = 1, message = "Correct !"))
        
        
        # 
        # if (isTRUE(!(.q3_test2))) return(c(value = 0, message = "! N'oubliez pas d'utiliser la fonction géométrique geom_line."))
        # if (isTRUE(!(.q3_test3 && .q3_test4))) return(c(value = 0, message = "! Vérifiez vos arguments de mappage pour x et y : mettez-vous les bonnes variables ?"))
        # if (isTRUE(!(.q3_test5))) return(c(value = 0, message = "! N'oubliez pas de colorer les points en utilisant un argument de mappage dans vos couches."))
        # 
        # if (isTRUE(.q3_test1 && .q3_test2 && .q3_test3 && .q3_test4 && .q3_test5)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
# faux
        return(c(value = 0, message = "Mauvaise réponse. Veuillez réessayer."))
  }
    .apply_autograder()
    }


# [backend]
# créer un indice par question
.HINT_nigerm04_line <- function(){
  'Tout d’abord, identifiez quel dataframe fournir à la couche de données.
  Ensuite, placez les variables que vous souhaitez mapper sur votre axe x, y, et couleur à l’intérieur de `mapping = aes()`. 
  Ensuite, utilisez la bonne fonction de géométrie nécessaire pour un graphique linéaire : `geom_line()`.
  Si tout cela est correct, recherchez les fautes de frappe, les virgules manquantes ou les crochets non fermés.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_nigerm04_line <- function(){
  "ggplot(data = nigerm04,
         mapping = aes(x = week,
                       y = cases,
                       color = region)) + 
    geom_line()" -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Étiquette de la section ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Utilisez le dataframe `nigerm04` pour créer un graphique à barres des cases hebdomadaires, et remplissez
#'  toutes les barres avec la couleur "hotpink". Mappez `cases` sur l'axe des y, `week` sur le 
#'  axe des x, et définissez les barres à couleur constante.


# ggplot(data = nigerm04, 
#        mapping = aes(x = week, 
#                      y = cases)) +
#   geom_col(fill = "hotpink")



# [backend]
.CHECK_nigerm04_pinkbar <-
  function() {
    .problem_number <<- 4
    
    .nigerm04_pinkbar <- 
      ggplot(data = .nigerm04, 
             mapping = aes(x = week, 
                           y = cases)) +
      geom_col(fill = "hotpink")
    
    
    .autograder <<-
      function(){
        if (!is.ggplot(nigerm04_pinkbar)) return(c(value = -1, message = "Votre résultat doit être un objet ggplot2."))
        
        if (ggplot_digest(.nigerm04_pinkbar) == ggplot_digest(nigerm04_pinkbar)) return(c(value = 1, message = "Correct !"))
        
        return(c(value = 0, message = "Mauvaise réponse. Veuillez réessayer."))
      }
    .apply_autograder()
  }


# [backend]
# créer un indice par question
.HINT_nigerm04_pinkbar <- function(){
  'Placez les variables que vous souhaitez mapper sur votre axe x, y à l’intérieur de `mapping = aes()`. 
  Ensuite, utilisez la bonne fonction de géométrie, `geom_col()` avec le bon argument `fill`.' -> out
  cat(out)
}
# solution de la question
.SOLUTION_nigerm04_pinkbar <- function(){
  'ggplot(data = nigerm04, 
          mapping = aes(x = week, 
                        y = cases)) +
  geom_col(fill = "hotpink")' -> out
  cat(out)
}
