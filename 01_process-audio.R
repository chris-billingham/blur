library(tidyverse)
library(tuneR)
library(signal)
library(oce)

# load in the audio functions
source("02_audio-functions.R")

# get a list of all mp3s
get_files <- fs::dir_ls("mp3/", recursive = TRUE, type = "file") %>%
  tolower()

# generate all the spectrograms
walk(get_files, chunk_and_save)





