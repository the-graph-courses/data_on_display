# Custom ggplot digest function
## Joy Vaz
## 2022-11-17

#' Function to generate hash function digests of ggplot objects.
#' Intended for use in data viz data quizzes. 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               digest,
               here)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ggplot_digest function ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot_digest <- function(myplot) {
  # Create temp file name, save plot, digest file, and remove temp file
  plotfile <- tempfile(pattern = "ggplot_", fileext = ".png")
  suppressMessages(ggplot2::ggsave(filename = plotfile, plot = myplot, type = "cairo"))
  plot_crypt <- digest::digest(file = plotfile)
  file.remove(plotfile)
  return(plot_crypt)
}
get_plot_key <- function(myplot) {
  # Create temp file name, save plot, digest file, and remove temp file
  plotfile <- tempfile(pattern = "ggplot_", fileext = ".png")
  suppressMessages(ggplot2::ggsave(filename = plotfile, plot = myplot, type = "cairo"))
  plot_crypt <- digest::digest(file = plotfile)
  file.remove(plotfile)
  return(plot_crypt)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Example usage ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Correct plot ----

# Load data from gapminder package
p_load(gapminder)

gap_data <- gapminder::gapminder %>% 
 filter(year == 2007)

# create target plot
gap_plot <-
 gap_data %>% 
 ggplot(aes(gdpPercap, lifeExp, color = continent, size = pop)) +
 geom_point()

# Digest target
ggplot_digest(gap_plot)

### Fake student plot ----

# Create plot with slightly different code
student_plot <-
 gap_data %>% 
 ggplot() +
 geom_point(aes(gdpPercap, lifeExp, color = continent, size = pop))

# Digest student plot
ggplot_digest(student_plot)

### Compare plot digests ----

# Student plot and target plot digests should match
identical(ggplot_digest(gap_plot), ggplot_digest(student_plot))

# digest::digest is too sensitive
identical(digest(gap_plot), digest(student_plot))
