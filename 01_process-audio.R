library(tidyverse)
library(tuneR)
library(signal)
library(oce)
library(caret)


# load in the audio functions
source("02_audio-functions.R")

# get a list of all mp3s
get_files <- fs::dir_ls("mp3/", recursive = TRUE, type = "file")

# create the directories for the plots
if(!fs::dir_exists("plot/leisure")){fs::dir_create("plot/leisure")}
if(!fs::dir_exists("plot/modern_life_is_rubbish")){fs::dir_create("plot/modern_life_is_rubbish")}
if(!fs::dir_exists("plot/parklife")){fs::dir_create("plot/parklife")}
if(!fs::dir_exists("plot/the_great_escape")){fs::dir_create("plot/the_great_escape")}
if(!fs::dir_exists("plot/blur")){fs::dir_create("plot/blur")}
if(!fs::dir_exists("plot/thirteen")){fs::dir_create("plot/thirteen")}
if(!fs::dir_exists("plot/think_tank")){fs::dir_create("plot/think_tank")}

# generate all the spectrograms and save to plot directories
walk(get_files, chunk_and_save)

# get all the new files into a big list
# NOTE using -size as there's a problem
get_plots <- dir_info("plot/", recursive = TRUE, type = "file") %>%
  select(-size) %>% 
  mutate(album = str_extract(str_replace(path, "plot/", ""), "^[a-z_]+"))

# sort them into train and validate sets, create directories and then start copying files over
set.seed(1979)
train_index <- createDataPartition(get_plots$album, p = 0.75, list = FALSE)
train <- get_plots[train_index, ] %>%
  mutate(dataset = "train") %>%
  mutate(new_path = str_replace(path, "plot/", paste0("images/", dataset, "/")))

train %>% 
  transmute(new_dir = paste0("images/", dataset, "/", album)) %>% 
  unique() %>%
  walk(dir_create)

walk2(train$path, train$new_path, file_copy)

validate <- get_plots[-train_index, ] %>%
  mutate(dataset = "validate") %>%
  mutate(new_path = str_replace(path, "plot/", paste0("images/", dataset, "/")))

validate %>% 
  transmute(new_dir = paste0("images/", dataset, "/", album)) %>% 
  unique() %>%
  walk(dir_create)

walk2(validate$path, validate$new_path, file_copy)
