# Copy staging repo to student repo ----
## Kene David Nwosu
## 2021-03-27

#' Copies internal staging repo to the outward-facing student repo. 
#' The outward-facing repo will be made available to students for download.
#' The copy procedure below is aimed at only copying over the files that the students actually need.
#' If you have ideas for simplifying the script, please suggest them! Start a merge request.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, 
               here, 
               fs)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Establish paths  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from <- here()

to   <- gsub("-staging", "", here()) # the repo you are copying to, "intro-to-data-analysis-with-r" 

# should be located in the same parent directory as the staging repo, from which this script is run

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  List top level files and directories  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# list files to be copied
all_files <- list.files(from, full.names = TRUE)

# files to copy
files_to_copy_search_string <- paste(c("ch0",
                                       "global"),
                                     collapse = "|")

# note that this list contains only top level files
files_to_copy <- all_files[str_detect(all_files, files_to_copy_search_string)]


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Copy, then delete extraneous things  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

file.copy(from = files_to_copy, 
          to = to, 
          recursive = TRUE) # `recursive = TRUE` makes sure you catch nested files

# there are a bunch files in the copied folders that we do not want students to have, delete these
# dirs first
all_copied_dirs <- fs::dir_ls(to, type = "directory", recurse = T)
dirs_to_delete <- all_copied_dirs[str_ends(all_copied_dirs, "/quizzes|/recordings|/global/engineering|global/trash|global/templates")]
fs::dir_delete(dirs_to_delete)

# then files
all_copied_files <- fs::dir_ls(to, type = "file", recurse = T)
files_to_delete <- all_copied_files[str_ends(all_copied_files, ".docx")]
fs::file_delete(files_to_delete)

# finally delete empty folders with terminal command (as at 2022-03-17, this doesn't work perfectly)
system2(command = "find",
        args    = c(to, "-empty", "-type d", "-delete"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Render all Rmd's in the STUDENT repo  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fs::dir_ls(to, regexp = )

