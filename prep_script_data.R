library(tidyverse) # tidy data functions
library(rvest) # for working with html data on websites
library(tidytext) # tidy text data functions

sw_1_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-The-Phantom-Menace.html") %>%
  # then, extract out the node with the script
  html_nodes(".scrtext pre") %>%
  # take just the text from the node
  html_text() 

sw_2_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-Attack-of-the-Clones.html") %>%
  html_nodes(".scrtext pre") %>%
  html_text()

sw_3_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-Revenge-of-the-Sith.html") %>%
  html_nodes(".scrtext") %>%
  html_text()

sw_4_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-A-New-Hope.html") %>%
  html_nodes(".scrtext pre") %>%
  html_text()

sw_5_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-The-Empire-Strikes-Back.html") %>%
  html_nodes(".scrtext pre") %>%
  html_text()

sw_6_script <- read_html("https://www.imsdb.com/scripts/Star-Wars-Return-of-the-Jedi.html") %>%
  html_nodes(".scrtext pre") %>%
  html_text()

sw_names <- c("The Phantom Menace", "Attack of the Clones",
              "Revenge of the Sith", "A New Hope", 
              "Empire Strikes Back", "Return of the Jedi") 

scripts <- data.frame(scripts = c(sw_1_script,
                     sw_2_script,
                     sw_3_script,
                     sw_4_script,
                     sw_5_script,
                     sw_6_script),
                     movie = sw_names)
saveRDS(scripts,'data/script_data.rds')

