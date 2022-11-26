# Data Viz Ls02 Scatterplots
## Laure Vancauwenberghe, Joy Vaz, Kene Nwosu
## 2022-07-08

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('graph-courses/autograder')
pacman::p_load(tidyverse,
               praise,
               here,
               dplyr,
               KO)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Init ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.scores <- rep(-1, times = 6)   # Put total number of questions as `times` argument

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.malidd <- read_csv(here::here("data/clean/malidd.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q1 age_height ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Using the `malidd` data frame, create a scatterplot showing the relationship between age and height (`height_cm`).

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
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = paste("Correct!", praise::praise()) ))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_age_height <- function(){
  'First, identify which data variables you want to plot on your x and your y axis. 
  Then, think about which geometry function you need to create a scatter plot.
  Always remember to check for typos and unclosed brackets!' -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height <- function(){
  'ggplot(data = malidd,
             mapping = aes(x = age_months, 
                           y = height_cm)) +
      geom_point()' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Q2 age_height_respi ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the `malidd` data frame, create a scatterplot showing the relationship between age and viral load, and map a third variable, `freqrespi`, to color:

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
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        

        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = paste("Correct!", praise::praise()) ))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_age_height_respi <- function(){
  'First, identify what you want to plot on your x and your y axis. 
  Then, think about which geometry function you want to use to plot points in ggplot.
  Then remember that we want to color these points.' -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_respi <- function(){
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
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = paste("Correct!", praise::praise()) ))
      }
    .apply_autograder()
  }
## try to fix merge conflict 

# [backend]
# create one hint per question
.HINT_age_height_fever <- function(){
  'Identify which variables from the data you want to map to the x and y positions. 
  Next, add the correct geometry function needed to create a scatter plot.
  Remember that we want to color these points. 
  Keep in mind that ggplot will treat the binay variable `fever` as a continuous variable, but here we want you to give it two distinct colors.' -> out
  cat(out)
}
# solution of question
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

#Create a scatterplot with the same variables as the previous example, but change the color of the points to `cornflowerblue`, increase the size of points to 3mm and set the opacity at 60%.

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
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = paste("Correct!", praise::praise()) ))
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_age_viral_blue <- function(){
  'Choose the right geom function, then remember to set your color, alpha and size arguments.' -> out
  cat(out)
}
# solution of question
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

#Create a scatterplot with the same variables as the previous example, but change the color of the points to "steelblue", the size to 2.5mm, the transparency to 80%, and add trend line with the smoothing method `lm` (linear model). To make the trend line stand out, change it's color "indianred3".

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
      geom_smooth(method = "lm")
    
    gg_req <- .q5_correct
    gg_ans <- age_height_2
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = paste("Correct!", praise::praise()) ))
        
    
    }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_age_height_2 <- function(){
  'Choose the right geom functions, 
  then remember to set your color, alpha and size arguments for your points
  and your smoothing method.' -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_2 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, 
                        y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8) +
      geom_smooth(method = "lm")' -> out
  cat(out)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## age_height_3 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Recreate the plot above, but this time change the shape to point to tilted rectangles (number 23), and map the body temperature variable (`temp`) to fill color.


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
      geom_smooth(method = "gam",
                  color = "indianred3")
    
    gg_req <- .q6_correct
    gg_ans <- age_height_3
    
    .autograder <<-
      function(){
        if (!is.ggplot(gg_ans)) return(c(value = -1, message = "Wrong. Your result should be a ggplot2 object. Please try again."))
        
        # Use compare_ggplots to check if answer is perfectly correct
        perfect_match <- suppressWarnings(compare_ggplots(gg_req, gg_ans))
        
        if(isTRUE(perfect_match)) return(c(value = 1, message = paste("Correct!", praise::praise()) ))
        if(!isTRUE(perfect_match)) return(c(value = 0, message = paste("Correct!", praise::praise()) ))
        
        
      }
    .apply_autograder()
  }

# [backend]
# create one hint per question
.HINT_age_height_3 <- function(){
  'Choose the right geom functions, then remember to set your color, alpha, shape, size and method arguments. 
  Think about which should be in the mapping (aes"-thetics") and which should be layer local parameters.' -> out
  cat(out)
}
# solution of question
.SOLUTION_age_height_3 <- function(){
  'ggplot(data = malidd, 
          mapping = aes(x = age_months, y = height_cm)) + 
      geom_point(color = "steelblue",
                 size = 2.5,
                 alpha = 0.8,
                 shape = 23,
                 mapping = aes(fill = temp)) +
      geom_smooth(method = "gam",
                  color = "indianred3")' -> out
  cat(out)
}
