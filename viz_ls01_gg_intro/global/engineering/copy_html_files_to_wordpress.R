# Copy staging repo to student repo ----
## GRAPH Courses team
## 2021-03-27

#' Copies internal staging repo to a repo hosted on GitHub pages. Lessons are then embedded in our workspace

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, fs)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Re-render  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## First re-render all markdown files if necessary
# rmds_to_render <- list.files(here::here(), pattern = "Rmd", full.names = T, recursive = T)
# for (rmd in rmds_to_render[1:10]) rmarkdown::render(rmd)
# for (rmd in rmds_to_render[11:20]) rmarkdown::render(rmd)
# for (rmd in rmds_to_render[21:30]) rmarkdown::render(rmd)
# for (rmd in rmds_to_render[31:length(rmds_to_render)]) rmarkdown::render(rmd)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Copy  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from <- here::here()
to   <- stringr::str_replace(from, "staging", "wp")

# list files to be copied
all_top_level_files <- list.files(from, full.names = TRUE)
# top_level_files to copy
top_level_files_to_copy_search_string <- "ch\\d\\d_"
top_level_files_to_copy <- all_top_level_files[stringr::str_detect(all_top_level_files, top_level_files_to_copy_search_string)]

file.copy(from = top_level_files_to_copy, 
          to = to, 
          recursive = TRUE)

## Now delete non HTML files (there is smarter way to do this [we should be able to copy JUST the html files], but I leave like this for now)
all_copied_files <- list.files(to, recursive = T)

files_to_delete <- 
  all_copied_files[!stringr::str_ends(all_copied_files, ".html")] %>% 
  str_replace("staging", "wp")
  
files_to_delete_full_path <- paste0(to, "/", files_to_delete_base_name)

file.remove(files_to_delete_full_path)


## finally delete empty folders with terminal command (as at 2022-03-17, this does not seem to delete everything)
system2(command = "find",
        args    = c(to, "-empty", "-type d", "-delete"))
