
# Librerias ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyverse)


# 1 -----------------------------------------------------------------------

auto_mpg  <- readr::read_csv("Repaso prueba/datos/auto-mpg.csv")
View(auto_mpg)

auto_mpg %>% glimpse()

## Reviando NAN

auto_mpg %>% summary()

auto_mpg <- auto_mpg %>% 
  janitor::clean_names()

## Re-codificacion variable origin, y transformacion de horsepower

auto_mpg <- auto_mpg %>% 
  mutate(origin = case_when(origin == 1 ~ 'EEUU',
                            origin == 2 ~ 'Europa',
                            origin == 3 ~ 'Japón'),
         horsepower = as.numeric(horsepower))


auto_mpg %>% summary()

## Se eliminan
auto_mpg <- drop_na(auto_mpg) 
auto_mpg %>% summary()





