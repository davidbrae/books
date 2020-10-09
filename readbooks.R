library(tidyverse)
library(readxl)

books <- read_xlsx('~/Desktop/books/Books.xlsx')


books %>%
  group_by(Year) %>%
  summarize(n(),sum(Pages),mean(Pages))


books %>%
  group_by(Year,Gender) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = Gender,values_from=n)
