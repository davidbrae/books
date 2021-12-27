library(tidyverse)
library(readxl)
library(readr)

books <- read_csv('~/Desktop/books/Books.csv')


books %>%
  group_by(Year) %>%
  summarize(n(),sum(Pages),mean(Pages))


books %>%
  group_by(Year,Gender) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = Gender,values_from=n)


books %>%
  filter(Year>2012) %>%
  group_by(Month) %>%
  summarize(n=n()) %>%
  arrange(n)
        