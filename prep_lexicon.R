

library(tidyverse)

library(readxl)

library(tidytext)

star_wars_quotes <- read_excel("data/star_wars_quotes.xlsx")

star_wars_quotes |>
  unnest_tokens(word,Quote)|>
  inner_join(get_sentiments('bing')) |>
  select(word,sentiment)|>
  write_csv('data/quote_sentiments.csv')
