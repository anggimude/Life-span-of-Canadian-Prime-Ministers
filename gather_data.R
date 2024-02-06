library(rvest)
library(tidyverse)
library(janitor)

raw_data <-
  read_html(
    "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_Canada"
  )
write_lines(as.character(raw_data), "pms.html")

