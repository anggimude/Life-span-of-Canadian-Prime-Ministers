library(tidyverse)
library(rvest)
library(janitor)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(ggplot2)
raw_data <- read_html("pms.html")
second_wikitable <- raw_data %>%
  html_elements(".wikitable") %>%
  .[2]
parse_data <- second_wikitable %>%
  html_table()
head(parse_data)

name_birth_death <- parse_data[[1]]$`Name(Birth–Death)`
print(name_birth_death)

unique_prime_minister <- unique(name_birth_death)
unique_prime_minister <- unique_prime_minister[-c(24)]
print(unique_prime_minister)

pm_data <- data.frame(
  unique_prime_minister,
  stringAsFactors = FALSE
)
organized_data = select(pm_data, unique_prime_minister)
print(organized_data)

cclean_data <- organized_data |>
  separate(
    unique_prime_minister, into = c("Prime_Minister", "not_name"), sep = "\\(", extra = "merge",
  ) |>
  mutate(
    birth_death_year = str_replace_all(not_name, "\\(b\\. ([[:digit:]]{4})\\)", "\\1"),
    born = str_extract(not_name, "born[[:space:]][[:digit:]]")
  ) |>
  select(Prime_Minister, birth_death_year, born)
head(cclean_data)

extract_years <- function(year_str) {
  if (grepl("\\d{4}–\\d{4}", year_str)) {
    born_died <- strsplit(gsub("[^0-9–]", "", year_str), "–")[[1]]
  } else if (grepl("b\\. (\\d{4})", year_str)) {
    born_died <- c(sub("\\D", "", regmatches(year_str, regexpr("\\d{4}", year_str))), NA)
  } else {
    born_died <- c(NA, NA)
  }
  
  return(born_died)
}

cclean_data[c("born", "died")] <- t(sapply(cclean_data$birth_death_year, extract_years))

cclean_data$born <- as.numeric(cclean_data$born)
cclean_data$died[is.na(cclean_data$died)] <- 2023
cclean_data$died <- as.numeric(cclean_data$died)
cleans_data <-
  cclean_data |>
  mutate(Age_at_death = died - born) |>
  select(Prime_Minister, born, died, Age_at_death)
print(cleans_data)

cleans_data |>
  ggplot(aes(x = born, xend = died, y = Prime_Minister, yend = Prime_Minister)) + geom_segment() +
  labs(title = "Lifespan of Prime Ministers of Canada", x = "Year", y = "Prime Minister") + theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

  