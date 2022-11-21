# Copy staging repo to student repo ----
## GRAPH Courses team
## 2021-03-27

#' Copies internal staging repo to a repo hosted on GitHub pages. Lessons are then embedded in our workspace

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages and functions ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, fs, cli)

blue_print <- function(x) cat(cli::bg_blue(cli::col_white(cli::style_bold(x))))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Re-render if necessary  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter files for rendering
rmds_to_render <- 
  fs::dir_ls(here::here(), 
             regexp = "ch\\d\\d_ls\\d\\d_.*Rmd$",
             recurse = T) %>%
  as_tibble() %>% 
  filter(str_detect(value, "/lessons/")) %>% 
  filter(!str_detect(str_to_lower(value), "ch99|ls99|/old/")) %>% 
  filter(!str_detect(str_to_lower(value), "-copy|-paste")) %>% 
  filter(!str_detect(str_to_lower(value), "/bookdown/")) %>% 
  pull(1)

# batched re-rendering in case of errors
for (rmd in rmds_to_render[14:length(rmds_to_render)]) {
  blue_print(paste0("Rendering: \n", rmd, 
                    "\n(", which(rmd == rmds_to_render), " of ", length(rmds_to_render), ")"
                    ))
  rmarkdown::render(rmd)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Copy  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from <- here::here()
to <- stringr::str_replace(from, "staging", "wp")

# list files to be copied
all_top_level_files <- list.files(from, full.names = TRUE)
# top_level_files to copy
top_level_files_to_copy_search_string <- "ch\\d\\d_"
top_level_files_to_copy <- all_top_level_files[str_detect(all_top_level_files, top_level_files_to_copy_search_string)]

file.copy(
  from = top_level_files_to_copy,
  to = to,
  recursive = TRUE
)

## Now delete non HTML files (there is smarter way to do this [we should be able to copy JUST the html files], but I leave like this for now)
all_copied_files <- dir_ls(to, recurse = T, all = T)
files_to_delete <- all_copied_files[!str_ends(all_copied_files, "\\.html|\\.Rproj") & 
                                      !str_detect(all_copied_files, "\\.git")]


files_to_delete_full_path <- paste0(files_to_delete)
file.remove(files_to_delete_full_path)

## finally delete empty folders with terminal command (as at 2022-03-17, this does not seem to delete everything)
system2(
  command = "find",
  args = c(to, "-empty", "-type d", "-delete")
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TODO: Pull then push -wp repo  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~








## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TODO: Squash all but most recent commit  ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
