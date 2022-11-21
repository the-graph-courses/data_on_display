# Ch03 Ls06 5NG Scatterplots
## Laure Vancauwenberghe, Joy Vaz
## 2022-07-08

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here,
               dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pacman::p_load(ggplot2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.malidd <- read.csv(here::here("ch03_intro_to_data_viz/data/clean/malidd.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q1 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using the `malidd` data frame, create a scatterplot showing the relationship between age and height (`height_cm`).

# [backend]
.CHECK_q1 <-
  function() {
    .problem_number <<- 1
   
    .q1_correct <- 
      ggplot(data = .malidd,
             mapping = aes(x = age_months, 
                           y = height_cm)) +
      geom_point()
    
    .autograder <<-
      function(){
        if (!is.ggplot(q1)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # test 1
        # that data used is correct
        .q1_test1 <- all_equal(
          target = as_tibble(q1$data), 
          current = as_tibble(.q1_correct$data))
        
        # test 2
        # that learner used geom_point()
        .q1_test2 <- any(stringr::str_detect(capture.output(q1$layers), 
                                             "geom_point"))
        # test 3
        # check the x mapping
        .q1_test3 <- "* `x` -> `age_months`" %in% capture.output(q1$mapping)
        
        # test 4
        # check the y mapping
        .q1_test4 <- "* `y` -> `height_cm`" %in% capture.output(q1$mapping)
        
        if (isTRUE(!(.q1_test1))) return(c(value = 0, message = "! Check which dataset you are plotting."))
        if (isTRUE(!(.q1_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function geom_point."))
        if (isTRUE(!(.q1_test3 && .q1_test4))) return(c(value = 0, message = "! Check your mapping arguments for x and y: are you putting the right variables?"))
        if (isTRUE(.q1_test1 && .q1_test2 && .q1_test3 && .q1_test4)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q1 <- function(){
  'First, identify what you want to plot on your x and your y axis. 
  Then, think about which geometry function you want to use to plot.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q1 <- function(){
  'ggplot(data = malidd,
             mapping = aes(x = age_months, 
                           y = height_cm)) +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q2 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the `malidd` data frame, create a scatterplot showing the relationship between age and viral load, and map a third variable, `fever`, to color:
  
# [backend]
.CHECK_q2 <-
  function() {
    .problem_number <<- 3
    
    .q2_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = fever))
    
    .autograder <<-
      function(){
        if (!is.ggplot(q2)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # test 1
        # that data used is correct
        .q2_test1 <- all_equal(
          target = as_tibble(q2$data), 
          current = as_tibble(.q2_correct$data))
        
        # test 2
        # that learner used geom_point()
        .q2_test2 <- any(stringr::str_detect(capture.output(q2$layers), 
                                             "geom_point"))
        # test 3
        # check the x mapping
        .q2_test3 <- "* `x` -> `age_months`" %in% capture.output(q2$mapping)
        
        # test 4
        # check the y mapping
        .q2_test4 <- "* `y` -> `viral_load`" %in% capture.output(q2$mapping)
        
        # test 5
        # check the color argument: UK spelling
        .q2_test5 <- any(stringr::str_detect(capture.output(q2$layers), 
                                             "colour = ~fever"))
        
        if (isTRUE(!(.q2_test1))) return(c(value = 0, message = "! Check which dataset you are plotting."))
        if (isTRUE(!(.q2_test2))) return(c(value = 0, message = "! Do not forget to use ggplot2 geometry function geom_point."))
        if (isTRUE(!(.q2_test3 && .q2_test4))) return(c(value = 0, message = "! Check your mapping arguments for x and y: are you putting the right variables?"))
        if (isTRUE(!(.q2_test5))) return(c(value = 0, message = "! Do not forget to color the points using a mapping argument in your layers."))
        if (isTRUE(.q2_test1 && .q2_test2 && .q2_test3 && .q2_test4 && .q2_test5)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q2 <- function(){
  'First, identify what you want to plot on your x and your y axis. 
  Then, think about which geometry function you want to use to plot.
  Then remember that we want to color these points.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q2 <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(mapping = aes(color = fever))' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a scatterplot with the same variables as the previous example, but change the color of the points to `cornflowerblue`, increase the size of points to 3mm and set the opacity at 60%.

# [backend]
.CHECK_q3 <-
  function() {
    .problem_number <<- 4
    
    ##### DISCLAIMER: 
    #It matters a LOT if the student puts the color, size, alpha in mapping =aes() or as is now. 
    #Color gets encoded with hex (manageable)
    #Size gets adapted so not 3 but 4.535534
    #Alpha gets adapted so not 0.6 but 0.55
    #We need to define if we give a scaffold or if here we anticipate different writings of the color,alpha,size
    #I vote scaffold
    .q3_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "cornflowerblue",
                 size = 3,
                 alpha = 0.6) 
    
    # to access color, size, alpha of SOLUTION
    .q3_correct_build <- ggplot_build(.q3_correct)

    
    .autograder <<-
      function(){
        if (!is.ggplot(q3)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # to access color, size, alpha of ANSWER
        .q3_build <- ggplot_build(q3)
        
        # test 1
        # that learner used geom_point()
        .q3_test1 <- any(stringr::str_detect(capture.output(q3$layers), 
                                             "geom_point"))
        
        
        # test 2
        # check the color argument
        .q3_test2 <- .q3_build$data[[1]]["colour"][1,1] == .q3_correct_build$data[[1]]["colour"][1,1]
        
        # test 3 
        # check the size argument
        .q3_test3 <- .q3_build$data[[1]]["size"][1,1] == .q3_correct_build$data[[1]]["size"][1,1]
        
        # test 4
        # check the alpha argument
        .q3_test4 <- .q3_build$data[[1]]["alpha"][1,1] == .q3_correct_build$data[[1]]["alpha"][1,1]
        
        if (isTRUE(!(.q3_test1))) return(c(value = 0, message = "! Are you using geom_point?"))
        if (isTRUE(!(.q3_test2))) return(c(value = 0, message = "! Your color argument is wrong: cornflowerblue as a string or hex code."))
        if (isTRUE(!(.q3_test3))) return(c(value = 0, message = "! Your size argument is wrong. It should be 3."))
        if (isTRUE(!(.q3_test4))) return(c(value = 0, message = "! Your alpha argument is wrong. We want it set to 0.6."))
        if (isTRUE(.q3_test1 && .q3_test2 && .q3_test3 && .q3_test4)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q3 <- function(){
  'Choose the right geom function, then remember to set your color, alpha and size arguments.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q3 <- function(){
  'ggplot(data = malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "cornflowerblue",
                 size = 3,
                 alpha = 0.6)' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q4 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a scatterplot with the same variables as the previous example, but change the color of the points to "steelblue", the size to 2.5mm, the transparency to 80%, and add trend line with the smoothing method `lm` (linear model). To make the trend line stand out, change it's color "indianred3".

# [backend]
.CHECK_q4 <-
  function() {
    .problem_number <<- 5
    
    ##### DISCLAIMER: 
    #It matters a LOT if the student puts the color, size, alpha in mapping =aes() or as is now. 
    #We need to define if we give a scaffold or if here we anticipate different writings of the color,alpha,size
    #I vote scaffold
    .q4_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8) +
      geom_smooth(method = "lm")
    
    # to access color, size, alpha of SOLUTION
    .q4_correct_build <- ggplot_build(.q4_correct)
    
    
    .autograder <<-
      function(){
        if (!is.ggplot(q4)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # to access color, size, alpha of ANSWER
        .q4_build <- ggplot_build(q4)
        
        # test 1
        # that learner used geom_point()
        .q4_test1 <- any(stringr::str_detect(capture.output(q4$layers), 
                                             "geom_point"))
        
        # test 2
        # check the color argument
        .q4_test2 <- .q4_build$data[[1]]["colour"][1,1] == .q4_correct_build$data[[1]]["colour"][1,1]
        
        # test 3 
        # check the size argument
        .q4_test3 <- .q4_build$data[[1]]["size"][1,1] == .q4_correct_build$data[[1]]["size"][1,1]
        
        # test 4
        # check the alpha argument
        .q4_test4 <- .q4_build$data[[1]]["alpha"][1,1] == .q4_correct_build$data[[1]]["alpha"][1,1]
        
        # test 5
        # that learner used geom_smooth()
        .q4_test5 <- any(stringr::str_detect(capture.output(q4$layers), 
                                             "geom_smooth"))
        
        # test 6
        # check learner used "lm" method in geom_smooth
        # Other option: go through prediction of the lm model for both plot data
        # setequal(predict(lm(viral_load~age_months,q4$data)) , predict(lm(viral_load~age_months,.q4_correct$data)) )
        .q4_test6 <- any(stringr::str_detect(capture.output(q4$layers), 
                                             "method = lm"))
        
        if (isTRUE(!(.q4_test1))) return(c(value = 0, message = "! Are you using geom_point?"))
        if (isTRUE(!(.q4_test2))) return(c(value = 0, message = "! Your color argument is wrong."))
        if (isTRUE(!(.q4_test3))) return(c(value = 0, message = "! Your size argument is wrong."))
        if (isTRUE(!(.q4_test4))) return(c(value = 0, message = "! Your alpha argument is wrong."))
        if (isTRUE(!(.q4_test5))) return(c(value = 0, message = "! Are you using geom_smooth?"))
        if (isTRUE(!(.q4_test6))) return(c(value = 0, message = "! Are you using the lm method for geom_smooth?"))
        if (isTRUE(.q4_test1 && .q4_test2 && .q4_test3 && .q4_test4 && .q4_test5 && .q4_test6)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q4 <- function(){
  'Choose the right geom functions, 
  then remember to set your color, alpha and size arguments for your points
  and your smoothing method.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q4 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, 
                        y = viral_load)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8) +
      geom_smooth(method = "lm")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## q5 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Recreate the plot above, but this time change the shape to point to tilted rectangles (number 23), and map the body temperature variable (`temp`) to fill color.


# [backend]
.CHECK_q5 <-
  function() {
    .problem_number <<- 6
    
    ##### DISCLAIMER: 
    #It matters a LOT if the student puts the color, size, alpha in mapping =aes() or as is now. 
    #We need to define if we give a scaffold or if here we anticipate different writings of the color,alpha,size
    #I vote scaffold
    .q5_correct <- 
      ggplot(data = .malidd, 
             mapping = aes(x = age_months, 
                           y = viral_load)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "gam",
                  color = "indianred3")
    
    # to access color, size, alpha of SOLUTION
    .q5_correct_build <- ggplot_build(.q5_correct)
    
    
    .autograder <<-
      function(){
        if (!is.ggplot(q5)) return(c(value = -1, message = "Your result should be a ggplot2 object."))
        
        # to access color, size, alpha of ANSWER
        .q5_build <- ggplot_build(q5)
        
        # test 1
        # that learner used geom_point()
        .q5_test1 <- any(stringr::str_detect(capture.output(q5$layers), 
                                             "geom_point"))
        
        # test 2
        # check the color argument
        .q5_test2 <- .q5_build$data[[1]]["colour"][1,1] == .q5_correct_build$data[[1]]["colour"][1,1]
        
        # test 3 
        # check the size argument
        .q5_test3 <- .q5_build$data[[1]]["size"][1,1] == .q5_correct_build$data[[1]]["size"][1,1]
        
        # test 4
        # check the alpha argument
        .q5_test4 <- .q5_build$data[[1]]["alpha"][1,1] == .q5_correct_build$data[[1]]["alpha"][1,1]
        
        # test 5
        # check the shape argument
        .q5_test5 <- .q5_build$data[[1]]["shape"][1,1] == .q5_correct_build$data[[1]]["shape"][1,1]
        
        # test 6
        # check that learner is using a fill inside geom_point
        .q5_test6 <- any(stringr::str_detect(capture.output(q5$layers), 
                                             "fill = ~temp"))
        
        # test 7
        # that learner used geom_smooth()
        .q5_test7 <- any(stringr::str_detect(capture.output(q5$layers), 
                                             "geom_smooth"))
        
        # test 8
        # check learner used "gam" method in geom_smooth
        .q5_test8 <- any(stringr::str_detect(capture.output(q4$layers), 
                                             "method = gam"))
        
        # test 9
        # check learner used correct color in geom_smooth
        .q5_test9 <- .q5_build$data[[2]]["colour"][1,1] == .q5_correct_build$data[[2]]["colour"][1,1]
        
        if (isTRUE(!(.q5_test1))) return(c(value = 0, message = "! Are you using geom_point?"))
        if (isTRUE(!(.q5_test2))) return(c(value = 0, message = "! Your color argument is wrong in geom_point."))
        if (isTRUE(!(.q5_test3))) return(c(value = 0, message = "! Your size argument is wrong."))
        if (isTRUE(!(.q5_test4))) return(c(value = 0, message = "! Your alpha argument is wrong."))
        if (isTRUE(!(.q5_test5))) return(c(value = 0, message = "! Your shape argument is wrong."))
        if (isTRUE(!(.q5_test6))) return(c(value = 0, message = "! Your filling for geom_point is wrong. Remember to put it inside the mapping."))
        if (isTRUE(!(.q5_test7))) return(c(value = 0, message = "! Are you using geom_smooth?"))
        if (isTRUE(!(.q5_test8))) return(c(value = 0, message = "! Are you using the lm method for geom_smooth?"))
        if (isTRUE(!(.q5_test9))) return(c(value = 0, message = "! Your color argument is wrong in geom_smooth."))

        if (isTRUE(.q5_test1 && .q5_test2 && .q5_test3 && .q5_test4 && .q5_test5 && .q5_test6 && .q5_test7 && .q5_test8 && .q5_test9)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        # wrong
        return(c(value = 0, message = "Wrong. Please try again."))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_q5 <- function(){
  'Choose the right geom functions, then remember to set your color, alpha, shape, size and method arguments. 
  Think about which should be in the mapping (aes"-thetics") and which should be layer local parameters.' -> out
  cat(out)
}
# solution of question
.SOLUTION_q5 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, y = viral_load)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "gam",
                  color = "indianred3")' -> out
  cat(out)
}
