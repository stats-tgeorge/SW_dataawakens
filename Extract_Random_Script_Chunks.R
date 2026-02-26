library(tidyverse)
library(tidytext)

# install.packages('textdata')

iv <- read.table("data/SW_EpisodeIV.txt")


set.seed(454350)

ints <- round(runif(6,1,1010),0)

for(i in 1:6){
  write_csv(iv[ints[i]:(ints[i]+29),],paste0("data/epiv_piece_",i,'.csv'))
}

## Get relevant sentiments
temp <- NULL
for(i in 1:6){
  temp <- bind_rows(temp,iv[ints[i]:(ints[i]+29),])
}

temp |> unnest_tokens(word,dialogue) |> 
  inner_join(get_sentiments('afinn')) |>
  select(-movie)|>
  rename(sentiment_afinn = value)|>
  arrange(word)|>
  write_csv('data/afinn_sentiment_ss.csv')



