
### Run ahead of time


library(tidyverse)
library(tidytext)
library(DataExplorer)
library(kableExtra)
library(wordcloud2)
library(ggwordcloud)
theme_set(theme_bw())
update_geom_defaults("bar", list(fill = "#523178"))

iv <- read.table("data/SW_EpisodeIV.txt") |> 
  mutate(movie = "Episode IV")


v <- read.table("data/SW_EpisodeV.txt") |> 
  mutate(movie = "Episode V")

vi <- read.table("data/SW_EpisodeVI.txt") |> 
  mutate(movie = "Episode VI")

full <- bind_rows(iv,v,vi) |> mutate(character = str_to_title(character))

remove(iv,v,vi)

bing <- get_sentiments('bing')

SW_movie_scripts <- full|> 
  # group_by(movie)|>
  # mutate(
  #   line_number = row_number())|>
  # ungroup()|>
  unnest_tokens(word,dialogue)

# 
# SW_movie_scripts2 <- full|> 
#   # group_by(movie)|>
#   # mutate(
#   #   line_number = row_number())|>
#   # ungroup()|>
#   unnest_tokens(word,dialogue,token = "ngrams", n = 2)

SW_movie_scripts2 <- 
  SW_movie_scripts |> 
  #filter(movie == "Episode V")|>
  inner_join(get_sentiments('bing'))|>
  mutate(your_side = ifelse(sentiment=="positive","light side","dark side"),
         word = as.factor(word))|>
  count(word,your_side) |> 
  rename(freq=n)#|>
  #filter(freq >1)



create_SW_Wordcloud <- function(chosen_side){
  chosen_side <- "dark side"
  
  plot_data <-
    SW_movie_scripts2 |> filter(your_side == chosen_side)|>
    select(-your_side) |>
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))
  
  
  img = png::readPNG(
    paste0("imgs/wordcloud_masks/",
           ifelse(chosen_side == "light side","yoda","vader"),".png"))
  
  if (dim(img)[3] == 2) {
    img2 <- array(NA, dim = c(dim(img)[1], dim(img)[2], 4))
    img2[, , 1:3] <- img[, , 1]  # Duplicate grayscale to RGB
    img2[, , 4] <- img[, , 2]    # Keep alpha as is
  }else{
    img2 <- img
  }

  

p1 <- ggplot(plot_data, aes(label = word, size = freq,color = freq,angle = angle)) +
  geom_text_wordcloud_area(mask = img2,rm_outside = TRUE) +
  scale_size_area(max_size = 40) +
  scale_color_gradient(low = "#523178", high = "#D6b4fc")

return(p1)
}



                            